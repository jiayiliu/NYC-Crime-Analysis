---
Title: Interactive Analysis with Shiny R
Author: Jason (Jiayi) Liu
Date: Feb 8, 2015
---

# Background

Safty is my first concern when I search for a new place to live.  However it is a hard question when I move to a new city.  Suggestions from friends are useful but may be biased by their own experience.

Thanks to the NYC open data, now I can check the crime record interactive.  But it is hard to find the find the trend easily.  Here is the official [NYC crime map](http://maps.nyc.gov/crime/), and here is another [visualization](http://www.city-data.com/crime/crime-New-York-New-York.html).  So I decided to build my own visualization.

# Data Mining

After scrutinizing the javascript of the NYC crime map, I found that the basic information for precincts are stored in `crime/js/data.js` and the crime records for each precinct are retrieved by a google API request.  The easiest way to discover that request is by monitoring the network traffic, for example, using the network monitor in the [Chrome developer tools](https://developer.chrome.com/devtools#improving-network-performance).

The data mining are splitted into two steps.  In the following, I will explain these two steps in details.  In addition, the source code is avaialbe on [github]().

## Extract basic precinct information
We need three basic information for each precinct: `ID`, `population`, and `area`.  This information is stored in [data.js](http://maps.nyc.gov/crime/js/data.js).  Because it is in javascript format, we need a little manual tweak here to convert it into [JSON](https://developer.chrome.com/devtools#improving-network-performance) file.  From line 288 we copy the text after `pct:{`.  Notice keep the bracktes in pairs.  And save the text into a `precinct.json` file for the next step.  This text is now a JSON array with the ID as the key for each entry and in the entry, "g" for the area geometry and "p" for the population.

In `R`, the `jsonlite` package provides a convinient interface to read JSON data into a list.
`fromJSON("precinct.json")` will load the file into a list with precinct IDs for names, and contains two lists inside: `g` and `p`.

Now the data is almost ready to use.  However there is a potential problem.  Take a look of the precinct data, you will find the area of precinct 1 is not one continuious shape, so under the list element `g`, it contains several lists, each one is a 2-D list of the coordinates of the nodes for a polygon shape.

## Retrieve crime records

The official [NYC crime map](http://maps.nyc.gov/crime/) uses google APIs to provide unique data access points.  The detail crime record is retrieved by [AJAX](http://en.wikipedia.org/wiki/Ajax_%28programming%29) request in [ui.js](http://maps.nyc.gov/crime/js/ui.js).  The following code provide a web crawler to download one crime entry by given the precinct ID, year, and month.

```
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
```


# Analysis and Visualization

# Shiny R app


