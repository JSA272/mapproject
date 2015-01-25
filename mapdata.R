## The following R code retrieves and cleans txt data for a random sampling of 
## popular first names, popular last names, and NYC ZIP codes.

## The totitle() function takes a string and converts it to title case, ie
## it converts the first character to upper case and the following characters
## to lower case. 

toTitle <- function(x){
        a <- substr(x, 1, 1)
        b <- substr(x, 2, nchar(x))
        paste(toupper(a), tolower(b), sep = "")
}

## Imports data by reading the appropriate .csv files

## Reads a list of baby names for the year 2013 with 5 or more occurances.
## SOURCE: http://www.ssa.gov/OACT/babynames/names.zip
fnames <- read.csv("Mapdata/yob2013.txt", header = FALSE, col.names = c("Name", "Gender", "Number"), stringsAsFactors=FALSE)

## Reads a list of the top 1000 most common last names.
## SOURCE: http://names.mongabay.com/data/1000.html
lnames <- read.csv("Mapdata/lnames.txt", stringsAsFactors=FALSE)

## Reads a table of ZIP codes within the boundaries of New York City
## SOURCE: https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm
zipsdata <- read.csv("Mapdata/zipcodes.txt", col.names = c("Borough", "Neighborhood", "ZIP"), colClasses = "character", stringsAsFactors=FALSE)

## The zips table contains the ZIP code information in the form of semicolon-delimited strings
## zipSplitter() cleans the zips table data, outputting a numeric vector of ZIP codes.

zipSplitter <- function(input){
        output = numeric()
        for (i in 1:length(input$ZIP)){
                newData <- strsplit(input[1,3], ";")
                output <- c(output, as.numeric(newData[[1]]))
        }
        output
}

zips <- zipSplitter(zipsdata)

## randNames(n) creates a data.frame of n rows, each populated with a randomly selected
## first name, last name, and NYC ZIP code.

set.seed(1)
randNames <- function(n){
        x <- sample(toTitle(fnames$Name), n)
        y <- sample(toTitle(lnames$Name), n)
        z <- sample(zips, n, replace = TRUE)
        cbind(x,y,z)
}

randNames(5)
