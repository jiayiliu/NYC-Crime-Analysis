################################################################################
#' Data processing and analyzing code
################################################################################
library(reshape2)
library(lubridate)

#' crime type information
crime.type <- c("RAPE","ROBBERY","BURGLARY","MURDER","FELONY.ASSAULT",
                "GRAND.LARCENY","GRAND.LARCENY.OF.MOTOR.VEHICLE")

#' Main function need once to create quick access to crime data.
main <- function(){
    raw.data <- read.csv("record.csv")
    data <- convert_data(raw.data)
    save(data,file="record.RData")
    precinct.info <- get_precinct_info(fromJSON("precinct.json"))
    save(precinct, precinct.info,file="precinct.RData")
}

#' Convert the data into a clean format for R analysis
#' @param data a data frame contain the data read from record.csv
#' @return data frame [date, precinct, type, number]
convert_data <- function(data){
    require(lubridate)
    require(reshape2)
    # put the record at 15th of each month
    data$date <- ymd(paste(data$year,data$month,15)) 
    data$year <- NULL
    data$month <- NULL
    new.data <- melt(data, id=c("date","precinct"))
    names(new.data) <- c("date","precinct","type","number")
    new.data$precinct <- as.factor(new.data$precinct)
    new.data
}

################################################################################
#' Extract data of specific month or precinct
################################################################################
#' Get the crime data over months
#' @param data a data.frame from \code{convert_data}
#' @param precinct precinct=-1 means all precincts, or ID for specific precinct
#' @param mean choose to average each month or full time frame
#' @return data.frame with month/date and crime numbers
get_monthly_data <- function(data, precinct, mean=TRUE){
    if (precinct==-1){ # get all precinct
        if (mean) {
            d <- dcast(data, month(date)~type, value.var="number", sum)
        } else {
            d <- dcast(data, date~type, value.var="number", sum)
        }
    } else {
        if (!precinct %in% data$precinct){
            warning(paste("The precinct",precinct,"is not exist."))
            return(NULL)
        } else {
            if (mean) {
                d <- dcast(data[data$precinct==precinct,], 
                           month(date)~type, value.var="number", sum)    
            } else {
                d <- dcast(data[data$precinct==precinct,], 
                           date~type, value.var="number", sum)
            }
        }
    }
    if (mean)
        names(d)[1] <- "MONTH"
    stopifnot(identical(names(d)[-1],crime.type))
    return(d)
}

#' Get total crime at specific month
#' @param data data.frame crime record from \code{convert_data}
#' @param month to look (-1 for all month)
#' @param precinct to look (-1 for all precinct)
#' @return crime numbers for 7 types
get_crime_snapshot <- function(data, month, precinct){
    if (precinct!=-1&month!=-1){
        ndata <- subset(data,select=c(type,number))
    } else {
        if (precinct==-1){
            ndata <- subset(data,select=c(type,number),month(data$date)==month)
        } else {
            if (month==-1) {
                ndata <- subset(data, select=c(type,number),data$precinct==precinct)
            }
            else{
                ndata <- subset(data,select=c(type,number), month(data$date)==month & 
                                    data$precinct==precinct)
            }
        }
    }
    d<-dcast(ndata, .~type, value.var="number", sum)[-1]
    stopifnot(identical(names(d),crime.type))
    return(d)
}


################################################################################
#' Manipulate data
################################################################################
#' Apply weighting on crime types.
#' @param data data.frame
#' @param weight weightings on crime types
#' @return a new data.frame with TOTAL crime
apply_weight <- function(data, weight, renormal=TRUE){
    start.id <- which(names(data)==crime.type[1])
    stopifnot(identical(names(data)[0:6+start.id], crime.type))
    data$TOTAL <- apply(data[,0:6+start.id],1, function(x) sum(x*weight))
    if (renormal)
      data$TOTAL <- data$TOTAL/sum(weight)*7 # renormalize
    data
}

#' Normalize crime rate by given weights
#' @param data.frame crime data with weighted TOTAL
#' @return a data.frame contains a new column of weighted total crime rate.
#' Also the individual crime numbers are normalized
get_normalized_crime <- function(data){
    totnum <- sum(data$TOTAL)
    for (crime in crime.type){
        thisnum <- sum(data[,crime])
        data[,crime] <- data[,crime] * totnum/thisnum
    }
    data
}

################################################################################
#' Extract information from data
################################################################################
#' fit a trend coefficient for given weight
#' @param data.frame the data frame of crime data
#' @param vector the weights for 7 crime types
#' @param integer precinct id (-1 for all precincts)
#' @return slope of the fitting
fit_full_time <- function(data, weight, precinct){
    data <- get_monthly_data(data, precinct, mean=FALSE)
    data <- apply_weight(data, weight)
    fit <- lm(TOTAL ~ date, data=data)
    return(fit$coefficients[2]*60*60*24*30) # convert monly rate
}

#' Get crime monthly change rate for all precincts
#' @param data.frame data is the full record (from "record.RData")
#' @param vector the weights for 7 crime types
get_crime_monthly_change_all <- function(data, weight){
    precincts <- sort(unique(data$precinct))
    rates <- rep(0, length(precincts))
    for (i in 1:length(precincts)){
        rates[i] <- fit_full_time(data, weight, precincts[i])
    }
    data.frame(precincts,rates)
}

#' Get Crime 
#' @param data
get_crime_month <- function(data, weight, month=NULL){
  if (is.null(month))
    d <- dcast(data, precinct ~ type, value.var="number", sum)
  else
    d <- dcast(data[month(data$date)==month,], precinct ~ type, value.var="number", sum)
  d <- apply_weight(d, weight, renormal=FALSE)
  d[,c("precinct","TOTAL")]
}

#' Get the total area for one precinct
#' @param ppoly the "g" for one precinct from precinct.json
#' @return area (notice this is not the correct geo area yet)
get_area <- function(ppoly){
    require(rgeos)
    require(sp)
    area <- 0
    if (is(ppoly,"array")){
        dim(ppoly) <- dim(ppoly)[-1]
        ppoly[,1] <- ppoly[,1]*cos(ppoly[,2]/180*pi)
        ppoly <- ppoly*69 # 1 degree is about 69 mile
        area <- area + Polygon(ppoly)@area
    } else {
        for (i in ppoly){
            area <- area+get_area(i)
        }
    }
    area
}

#' Get area/population from precinct file
#' @param precinct the data read directly from \code(fromJSON("precinct.json"))
get_precinct_info <- function(precinct){
    id <- as.integer(names(precinct))
    area <- sapply(precinct, function(x) get_area(x$g))
    population <- sapply(precinct, function(x) x$p)
    names(population)<- NULL
    data.frame(precinct=id, area=area, population=population)
}

#' Get precinct detail information
#' @param precinct precinct JSON data
#' @param precinct.info information extracted from \code{get_precinct_info}
#' @param precinct_id Id from map event return
get_precinct_detail <- function(precinct, precinct.info, precinct_id){
  s <- strsplit(precinct_id,"-")[[1]]
  s <- as.integer(s)
  pid <- which(precinct.info[,1]==s[1])
  if (length(s)==1){
    lng = precinct[[pid]]$g[1,1,1]
    lat = precinct[[pid]]$g[1,1,2]
  } else {
    sid <- s[2]
    lng = precinct[[pid]]$g[[sid]][1,1,1]
    lat = precinct[[pid]]$g[[sid]][1,1,2]
  }
  list(id=s[1], area=precinct.info[pid,2], population=precinct.info[pid,3],
       lat=lat,lng=lng)
}