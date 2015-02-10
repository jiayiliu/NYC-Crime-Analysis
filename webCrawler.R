###############################################################################
#' Web crawler to extract information from http://maps.nyc.gov/crime/
#' 
#' Date: Jan 28, 2015
#' 
#' Procedure:
#' 1. extract the precinct JSON from \url{http://maps.nyc.gov/crime/js/data.js}
#'    save everying within {} after pct:
#' 2. run \code{save_precinct} to get clean precinct data
#' 3. run \code{save_all_crime_data} to get all crime data
#' 4. run \code{fix_missing} if the warning from step 3 is true alarm
###############################################################################
library(httr)
library(jsonlite)

#' Read the nyc.data.pct 
#' The original data is from from \url{http://maps.nyc.gov/crime/js/data.js}
#' Only the JSON part of pct is extracted and saved into precinct.json
#' @return list read the full json file of precinct
#' useful information is name and population
get_raw_precinct <- function(){
    fromJSON("./precinct.json")
}

#' Convert and save precinct information.
#' @return data.frame with precinct.id and population
save_precinct <- function(){
    precinct <- get_raw_precinct()
    pid <- as.integer(names(precinct))
    all.data <- NULL
    for (p in pid){
        all.data <- rbind(all.data, c(p,precinct[[as.character(p)]]$p))
    }
    all.data <- data.frame(all.data)
    colnames(all.data) <- c("precinct","population")
    write.table(all.data, file="precinct.csv", sep=',', row.names=FALSE)
    all.data
}

#' Read the precinct information saved by \code{save_precinct}.
#' @return data.frame with precinct and population
read_precinct <- function(){
    read.csv('precinct.csv')
}

#' Get crime record from \url{http://maps.nyc.gov/crime/} API
#' @param year Choose from 2013 to 2014 calander year
#' @param month Choose from 1 to 12 for month
#' @param precinct Choose from \code pid to get corresponding precinct
#' @return data frame with one row of crime record
get_crime_data <- function(year, month, precinct){
    # given year, month and precinct and return crime records
    if (year < 2013 | year > 2014){
        warning("For 2013-2014 only")
    }    
    referer <- "http://maps.nyc.gov/crime/"
    url_base <- "https://www.googleapis.com/"
    path <- paste0("mapsengine/v1/tables/",
                   "02378420399528461352-02912990955588156238/features/",
                   "?key=AIzaSyDW3Wvk6xWLlLI6Bfu29DuDaseX-g18_mo&",
                   "version=published&maxResults=1000&select=CR,TOT&")
    query <- 
        sprintf("where=MO%%3D%d%%20AND%%20YR%%3D%d%%20AND%%20PCT%%3D%%27%d%%27", 
                month, year, precinct)
    html <- GET(url_base, path=paste0(path,query), add_headers(referer=referer))
    j <- fromJSON(rawToChar(html$content))
    j <- j$features$properties
    if (is.null(j)) warning(paste("No data retrieved",year, month, precinct))
    s <- list()
    s$year <- year
    s$month <- month
    s$precinct <- precinct
    for (crime in c("RAPE","ROBBERY","BURGLARY","MURDER","FELONY ASSAULT",
                    "GRAND LARCENY","GRAND LARCENY OF MOTOR VEHICLE")){
        # fill in 0 if no data.
        s[[crime]] <- ifelse(crime %in% j$CR, j$TOT[crime==j$CR], 0)
    }
    data.frame(s)       
}


#' Get all crime records from maps.nyc.gov/crime/
#' Save to record.csv
#' @return data.frame with [year, month, precinct, c(crime_num for each type)]
save_all_crime_data <- function() {
    precinct <- read_precinct()
    all.data <- NULL
    for (year in c(2013, 2014)){
        for (month in 1:12){
            for (this.precinct in precinct$precinct){
                one.data <- get_crime_data(year, month, this.precinct)
                all.data <- rbind(all.data, one.data)
            }
        }
    }
    write.table(all.data, file="record.csv", sep=',', row.names=FALSE)
    all.data
}

#' If report warning in \code{save_all_crime_data}, 
#' use this function to patch missing values 
#' if there is data from the website.
#' @input a list of missed entries, each row is (year, month, pricinct)
#' @return overwrite the record.csv
fix_missing <- function(missed_list){
    data <- read.csv("record.csv")
    for (i in missed_list){
        data[data$year==i[1] & data$month==i[2] & data$precinct==i[3],] <- 
            get_crime_data(i[1],i[2],i[3])
    }
    write.table(data, file="record.csv", sep=',', row.names=FALSE)
}
