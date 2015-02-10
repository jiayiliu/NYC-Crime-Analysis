###############################################################################
#' Plotting functions to support visualize the crime trend
###############################################################################
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(scales)
#' crime type information
crime.type <- c("RAPE","ROBBERY","BURGLARY","MURDER","FELONY.ASSAULT",
                "GRAND.LARCENY","GRAND.LARCENY.OF.MOTOR.VEHICLE")
#' Labels for crime types
labels <- c("TOTAL",gsub("\\."," ",crime.type))
labels[8] <- "GRAND LARCENY\nOF MOTOR VEHICLE"

#' Color palette
colorpalette <- colorRamp(brewer.pal(11,"Spectral"))
################################################################################
#' Additional ploting and decoration functions
################################################################################

#' Create a color bar with specific range
#' http://stackoverflow.com/questions/9314658/colorbar-from-custom-colorramppalette
#' Changed for a nicer ticks
color.bar <- function(lut, min, max=-min, nticks=11, title='') {
  scale = (length(lut)-1)/(max-min)
  
  deci <- 10^as.integer(log10((max-min)/nticks)-1)
  min <- as.integer(min/deci)*deci
  max <- as.integer(max/deci)*deci
  ticks=seq(min, max, len=nticks)

  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}
#' Decorate the time series crime rate plot
#' @param figure handler from \code{plot_monthly_trend} or 
#' \code{plot_full_trend}.
#' @return figure handler
decorate_time_trend <- function(fig){
    fig + geom_line(aes(alpha=as.factor(variable=="TOTAL"), 
                        size=as.factor(variable=="TOTAL"))) +
        scale_alpha_discrete(range=c(0.4,1),guide=FALSE) +
        scale_colour_brewer(palette="Set1", name="Crime Type",
                            breaks=c("TOTAL", crime.type),
                            labels=labels) + 
        scale_size_discrete(range=c(1,2),guide=FALSE)
}

################################################################################
#' Main plotting functions
################################################################################

#' Plot monthly averaged data
#' @param data the subset of crime data to plot
#' @param weight the weight for each crime type
#' @param overlay boolean to overplot the different crime types. 
plot_monthly_trend <- function(data, weight=rep(1,7), overlay=FALSE){
    data <- apply_weight(data, weight)
    if (!overlay) {
        fig <- qplot(data$MONTH, data$TOTAL)
        fig <- fig + geom_smooth(method="loess")    
    } else {
        data <- get_normalized_crime(data)
        fig <- ggplot(data=melt(data, id=c("MONTH")), 
                      aes(x=MONTH, y=value, col=variable))
        fig <- decorate_time_trend(fig) 
    }
    fig + xlab("Month") + ylab("Weighted Average Crime Rate") + 
        ggtitle("Historical Crime Rate") +
        scale_x_continuous(breaks=c(1,4,8,12),labels=c("Jan","Apr","Aug","Dec"))
}


#' Plot full time range data
#' @param data the subset of crime data to plot
#' @param weight the weight for each crime type
#' @param overlay boolean to overplot the different crime types. 
plot_full_trend <- function(data, weight=rep(1,7), overlay=FALSE){
    data <- apply_weight(data, weight)
    if (!overlay) {
        fig <- qplot(data$date, data$TOTAL, 
                     xlab="Date", ylab="Weighted Crime Number")
        fig <- fig + geom_smooth(method="loess")    
    } else {
        data <- get_normalized_crime(data)
        fig <- ggplot(data=melt(data, id=c("date")), 
                      aes(x=date,y=value, col=variable))
        fig <- decorate_time_trend(fig)    
    }
    fig + xlab("Date") + ylab("Weighted Crime Rate") + 
        ggtitle("Historical Crime Rate")
}

#' create multiple plots in one page
#' cf. http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

#' Plot pie chart of crime data
#' @param data the subset of crime data (need to be melted)
plot_crime_pie <- function(data, weight=NULL){
    fig <- ggplot(data, aes(x="", y=value, fill=variable)) + 
        geom_bar(width=1,stat="identity") + 
        coord_polar(theta="y") +
        scale_fill_brewer(palette="Set1", name="Crime Type",
                          breaks=c(crime.type),
                          labels=labels[-1]) + 
        xlab("") + ylab("") + ggtitle("Crime Dist.")
    
    if (is.null(weight))
        return(fig)
    
    data$wvalue <- data$value*weight
    fig2 <- ggplot(data, aes(x="", y=wvalue, fill=variable)) + 
        geom_bar(width=1,stat="identity") + 
        coord_polar(theta="y") +
        scale_fill_brewer(palette="Set1", name="Crime Type",
                          breaks=c(crime.type),
                          labels=labels[-1]) + 
        xlab("") + ylab("") + ggtitle("Weighted Crime Dist.")
    
    return(multiplot(fig,fig2,cols=1))
    
}

#' Create a color scaled map for all precincts
#' @param map leaflet map layer
#' @param precinct precinct JSON data
#' @param scale data.frame with precinct id and scale to use
addPrecinct_color <- function(map, precinct, scale){
    scale[,2]<-rescale(scale[,2])
    for (i in names(precinct)){
        this.color <- scale[as.integer(i)==scale[,1],2]
        this.color <- rgb(colorpalette(this.color),maxColorValue=256)
        this.g <- precinct[[i]]$g
        if (is(this.g,"array")){ # continued precinct
            map$addPolygon(lng=this.g[1,,1], lat=this.g[1,,2],rep(i,length(this.g[1,,1])),
                           defaultOptions=list(
                               weight=1, fillOpacity=0.6,color=this.color))
        } else {
            counter <- 1
            for (j in this.g){
                map$addPolygon(lng=j[1,,1],lat=j[1,,2], rep(paste0(i,"-",counter),length(j[1,,1])),
                               defaultOptions=list(
                                   weight=1, fillOpacity=0.6,color=this.color)
                )
                counter <- counter+1                      
            }
        }
    }
}