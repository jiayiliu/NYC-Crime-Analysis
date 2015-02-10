---
Title: Interactive Analysis with Shiny R
Author: Jason (Jiayi) Liu
Date: Feb 8, 2015
---

# Background

Safty is my first concern when I search for a new place to live.  However it is a hard question when I move to a new city.  Suggestions from friends are useful but may be biased by their own experience.

Thanks to the NYC open data, now I can check the crime record interactive.  But it is hard to find the find the trend easily.  Here is the official [NYC crime map](http://maps.nyc.gov/crime/), and here is another [visualization](http://www.city-data.com/crime/crime-New-York-New-York.html).  So I decided to build my own visualization.

# Data Mining

After scrutinizing the javascript of the NYC crime map, I found that the basic information for precincts are stored in `crime/js/data.js` and the crime records for each precinct are retrieved by a google API request.  The easiest way to discover that request is by monitoring the network traffic, for example, using the network monitor in the Chrome developer tools.

The data mining are splitted into two steps.  In the following, I will explain these two steps in details.  In addition, the source code is avaialbe on [github]().

## Extract basic precinct information
We need three basic information for each precinct: ID, population, and area.  This information is stored in [data.js](http://maps.nyc.gov/crime/js/data.js).  Because it is in javascript format, we need a little manual tweak here to convert it into JSON file.  From line ?? we copy the text after `g=`.  Notice keep the bracktes in pairs.  And save the text into a `precinct.json` file for the next step.  This text is a JSON array with the ID as the key for each entry and in the entry, "g" for the geometry and "p" for the population.

In `R`, the `jsonlite` package provides a convinient interface to read JSON data into a list.
Just use:
```
fromJSON("precinct.json")
```

Then the data is ready to use.  However just be aware that for some precincts, the area is not one continuious shape, so under the list element "g" may contain several lists, each one is a 2-D list of the coordinates of the nodes for a polygon shape.

## Retrieve crime records



# Analysis and Visualization

# Shiny R app


