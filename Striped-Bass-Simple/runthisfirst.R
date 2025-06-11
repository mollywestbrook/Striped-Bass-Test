#Run this first to load everything in:

library(shiny)
library(bslib)
library(here)
library(tidyverse)
library(data.table)
library(leaflet)
library(sf)
library(sp)
library(ggrepel)
library(plotly)
library(leafgl)
library(htmlwidgets)
library(shinyjs)

#variables the app needs

#What cruise are we working with? Update for this month's
rawcruisedata<-read_csv("BAY844.csv")

###Ensure the data is confined to a single month, otherwise filter out extraneous dates
startdate<-min(rawcruisedata$Date)
enddate<-max(rawcruisedata$Date)

# rawcruisedata <- rawcruisedata %>%
#   filter(Date == startdate)

#This section ID's the cruise date to grab associated DO and Temp files later
monthdate <- as.numeric(substr(startdate,6,7))
monthname <- case_when(monthdate == '1' ~ 'January',
                       monthdate == '2' ~ 'February',
                       monthdate == '3' ~ 'March',
                       monthdate == '4' ~ 'April',
                       monthdate == '5' ~ 'May',
                       monthdate == '6' ~ 'June',
                       monthdate == '7' ~ 'July',
                       monthdate == '8' ~ 'August',
                       monthdate == '9' ~ 'September',
                       monthdate == '10' ~ 'October',
                       monthdate == '11' ~ 'November',
                       monthdate == '12' ~ 'December')
thisyear<-substr(startdate,1,4)

#And define parameters for Striped Bass:
suitabletemp<-82.4
tolerabletemp<-84.2
marginaltemp<-86
unsuitabletemp<-86
suitableDO<-4
tolerableDO<-3
marginalDO<-2
unsuitableDO<-2

suitability_colors <- c(
  "Unsuitable" = "black",
  "Marginal" = "orange",
  "Tolerable" = "yellow",
  "Suitable" = "dodgerblue"
)

#and the objects the app needs

files <- list.files(pattern = "wholebaysummary")
wholebaysurfacesummary <- read_csv(files[1])

files <- list.files(pattern = "fishinghotspotssummary")
fishinghotspotssummary <- read_csv(files[1])

files <- list.files(pattern = "historicbaydatafishingareassummary")
historicbaydata_fishingareas_summary <- read_csv(files[1])

files <- list.files(pattern = "historicbaydatasummary")
historicbaydata_summary <- read_csv(files[1])

files <- list.files(pattern = "mainchanneldata")
mainchanneldata <- read_csv(files[1])

files <- list.files(pattern = "potomacchanneldata")
potomacchanneldata <- read_csv(files[1])

fishingareapolygons.dd <- st_read(here("FishingAreaPolygons"))

fishingareacoords.dd_surface <- st_read(here("FishingAreaQuality"))

mddatathiscruise.dd_surface <- st_read(here("WholeBayQuality"))

#last bit of hard-code necessary for final figures:
#Next, we want the percent suitable volume. At the moment, that dataset isn't available to me
#So I'll just use Andrew's hard code for now.

percentsuitable<-c(100,100,100,100,88.25,78.68,87.54,12.92,48.79
                   ,81.06,75.02,87.92,NA,NA,NA)
months<-c("January","February","March","April","May","Early June","Late June","Early July",
          "Late July","Early August","Late August","September","October","November","December")

#This is hard-coded, but ideally I want to take this from a file:
maxdata<-c(100,100,100,100,99.9,91.8,89.1,88.6,85.5,86.3,91.4,97.9,100,100,100)
mindata<-c(98.6,100,99.9,96.3,71.6,67.9,59.5,11.7,2.52,4.31,26.2,77.1,88.4,97,95.3)
meandata<-c(99.9,100,100,99.4,90.8,78.3,76.1,64.5,56.9,61,72.1,87.9,97,99.8,99.8)


