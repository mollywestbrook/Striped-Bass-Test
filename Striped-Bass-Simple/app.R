#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(preset = "flatly"),
  
  # Application title
  titlePanel(paste("Striped Bass Habitat Suitability for", monthname, thisyear, sep=' ')),

  layout_columns(
    card(
      card_header("How To Use This App"),
      p("This dashboard provides helpful information on the most suitable fishing locations for Striped Bass.")
    ),
    navset_card_tab( 
      nav_panel("Legend", imageOutput('SuitableCriteria')), 
      nav_panel("Striped Bass Squeeze", imageOutput('StripedBassSqueeze')), 
      nav_panel("More Info", "DNR Links", verbatimTextOutput('DNRLinks'))
    )
  ),
  
  layout_columns(
    card(
      card_header("Chesapeake Bay Map"),
      leafletOutput("BayMap")
    )
  ),
  
  layout_columns(
    navset_card_tab(
      nav_panel("Hot Spot Surface Pie Chart", plotlyOutput(outputId = "HotSpotPie")),
      nav_panel("Hot Spot Suitability, Last 10 Yrs", plotlyOutput(outputId = "HotSpot10yrs"))#, 
      #nav_panel("Hot Spot Mean Suitability vs Historical Average", plotlyOutput(outputId = "HotSpotVolume")) #MW missing data for this line
    ),
    navset_card_tab(
      nav_panel("Whole Bay Surface Pie Chart", plotlyOutput(outputId = "WholeBayPie")),
      nav_panel("Total Bay Suitability, Last 10 Yrs", plotlyOutput(outputId = "WholeBay10yrs")), 
      nav_panel("Hot Spot Mean Suitability vs Historical Average", plotlyOutput(outputId = "WholeBayVolume"))
    )
  ),
  
  layout_columns(
    navset_card_tab(
      nav_panel("Cross-Section of the Whole Bay", plotlyOutput(outputId = 'WholeBayCrossSection')), 
      nav_panel("Cross-Section of the Potomac", plotlyOutput(outputId = 'PotomacCrossSection'))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #this generates our map
  output$BayMap <- renderLeaflet({
    leaflet() %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lng = -76.3, lat = 39.2, zoom = 9) %>%
      addPolygons(
        data = fishingareapolygons.dd, color = "#8373e2", stroke = 0.2, opacity = 0.8,
        label = fishingareapolygons.dd$name, group = "Fishing Areas") %>%
      addCircles(
        data = fishingareacoords.dd_surface, color = ~color, group = "Fishing Area Suitability",
        label = paste(fishingareacoords.dd_surface$name, fishingareacoords.dd_surface$habitat, sep=", ")) %>%
      addCircles(data = mddatathiscruise.dd_surface, color = ~color, group = "Whole Bay Suitability") %>%
      addLayersControl(
        overlayGroups = c("Fishing Areas", "Fishing Area Suitability", "Whole Bay Suitability"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Fishing Areas", "Whole Bay Suitability"))
  })
  
  #this makes our hot spot pie chart  
  output$HotSpotPie <- renderPlotly({
    plot_ly(fishinghotspotssummary, labels = ~habitat, values = ~percent, type = 'pie', 
            textposition = 'outside', 
            textinfo = 'label+percent',
            marker = list(colors = fishinghotspotssummary$color)) %>%
      layout(title = 'Fishing Hotspots Habitat Suitability',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #this makes our whole bay pie chart
  output$WholeBayPie <- renderPlotly({
    plot_ly(wholebaysurfacesummary, labels = ~habitat, values = ~percent, type = 'pie',
            textposition = 'outside',
            textinfo = 'label+percent',
            marker = list(colors = wholebaysurfacesummary$color)) %>%
      layout(title = 'Whole Bay Habitat Suitability',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
 #striped bass images
  output$SuitableCriteria <- renderImage({
    filename <- normalizePath(file.path('Bass Suitable Criteria.png'))
    list(src = filename, alt = "Striped Bass Suitability Criteria", width=500)
  }, deleteFile = FALSE)
  
  output$StripedBassSqueeze <- renderImage({
    filename <- normalizePath(file.path('Striped Bass Squeeze.png'))
    list(src = filename, alt = "Striped Bass Squeeze", width=500)
  }, deleteFile = FALSE)
  
  
  output$HotSpot10yrs <- renderPlotly({
    plot_ly(historicbaydata_fishingareas_summary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
            type = 'bar') %>%
      layout(title = 'Fishing Hot Spot Suitability for the Previous Ten Years',
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack')
  })
  
  output$WholeBay10yrs <- renderPlotly({
    plot_ly(historicbaydatasummary, x = ~year, y = ~percent, color = ~Habitat, colors = suitability_colors,
            type = 'bar') %>%
      layout(title = 'Whole Bay Suitability for the Previous Ten Years',
             yaxis = list(title = 'Percent of Habitat'),
             barmode = 'stack')
  })
  
  output$WholeBayVolume <- renderPlotly({
    plot_ly()%>%
      partial_bundle()%>%
      config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
      add_trace(x=months,y=maxdata,type='scatter',mode='lines',line = list(color = 'white'),showlegend = FALSE) %>%
      add_trace(x=months,y = mindata, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'white'),
                name = '1985-2022 Range')%>%
      add_trace(x=months,y=percentsuitable,type="scatter",mode='lines+markers',name="Percent Suitable Volume 2023",line=list(color="dodgerblue"),marker=list(color="dodgerblue"))%>%
      add_trace(x=months,y=meandata,type="scatter",mode='lines',name="Mean Percent Suitable Volume 1985-2022",line=list(color="darkgreen"))%>%
      layout(xaxis = list(categoryorder = "array",
                          categoryarray = months)) %>%
      layout(legend =list(x=0,y=0.05))%>%
      layout(showlegend = TRUE, legend = list(font = list(size = 8)))
  })
  
  output$WholeBayCrossSection <- renderPlotly({
    plot_ly()%>%
      config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
      add_trace(x=mainchanneldata$milesY,y=mainchanneldata$Sdepth
                ,hoverinfo="none"
                ,type='scatter',mode='markers'
                ,marker=list(color=mainchanneldata$color))%>%
      layout(xaxis=list(title="Distance from mouth of Bay (miles)",autorange="reversed"))%>%
      layout(yaxis=list(title="Depth (ft)",autorange="reversed"))
  })
  
  output$PotomacCrossSection <- renderPlotly({
    plot_ly()%>%
      config(displayModeBar=FALSE, modeBarButtonsToRemove = c("autoScale2d","hoverCompareCartesian","toggleSpikelines","select2d","lasso2d")) %>%
      add_trace(x=potomacchanneldata$milesY,y=potomacchanneldata$Sdepth
                ,hoverinfo="none"
                ,type='scatter',mode='markers'
                ,marker=list(color=potomacchanneldata$color))%>%
      layout(xaxis=list(title="Distance from mouth of Potomac (miles)",autorange="reversed",zeroline=FALSE))%>%
      layout(yaxis=list(title="Depth (ft)",autorange="reversed")) %>%
      layout(title = 'Potomac River Main Channel Cross Section')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
