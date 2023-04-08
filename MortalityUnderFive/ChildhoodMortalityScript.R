## 1. Load Packages 
library(tidyverse) # Easily Installand Load the 'Tidyverse' 
library(lubridate) # Make Dealing with Dates a Little Easier 
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data 
library(data.table) # Extension of `data.frame`
library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions 
library(geobr) # Download Official Spatial Data Sets of Brazil 
library(textclean) # Text Cleaning Tools
library(ggh4x) # Hacks for 'ggplot2' 
library(sf) # Simple Features for R 
library(openxlsx) # Read, Write and Edit xlsx Files
library(annotater) # Annotate Package Load Calls
library(pointblank) # Data Validation and Organization of Metadata for Local and Remote Tables
library(rjson) # JSON for R
library(WDI) #World Development Indicators and Other World Bank Data
library(countrycode) #Convert Country Names and Country Codes
library(plotly)
library(htmlwidgets)

## 2. Clean Data
'%!in%' = function(x,y)!('%in%'(x,y))
ProjectDirectory = 'C:/Users/vinic/OneDrive/GitHub Repo/DataVizProjects/MortalityUnderFive/'
ChildhoodMortalityRate.R = read.xlsx(paste0(ProjectDirectory,"UnderFive_MortalityRate.xlsx"),sheet = 3)

ChildhoodMortalityRate = ChildhoodMortalityRate.R %>% tibble::tibble() #Make it a tibble df
ChildhoodMortalityRate = ChildhoodMortalityRate %>% janitor::clean_names() #Clean colnames
ChildhoodMortalityRate = ChildhoodMortalityRate %>% tidylog::filter(uncertainty_bounds == 'Median') #Get only median estimates
ChildhoodMortalityRate = ChildhoodMortalityRate %>% subset(select = c(-uncertainty_bounds)) #Remove type of estiamte column
ChildhoodMortalityRate = ChildhoodMortalityRate %>% data.table::setnames(c('region',seq(1990,2021,1))) %>% janitor::clean_names() #Rename columns
ChildhoodMortalityRate = ChildhoodMortalityRate %>%
  pivot_longer(!region, names_to = "year", values_to = "mortality_rate") #Set the table in the long format
ChildhoodMortalityRate = ChildhoodMortalityRate %>% subset(select = c(2,1,3)) #Reorder columns
ChildhoodMortalityRate[,1] = lapply(ChildhoodMortalityRate[,1], function(x) stringr::str_replace(x,'x','')) #Replace 'x' by nothing on year col
ChildhoodMortalityRate[,1] = lapply(ChildhoodMortalityRate[,1], function(x) base::as.numeric(base::as.character(x))) #Make each year a integer
ChildhoodMortalityRate = ChildhoodMortalityRate %>% 
  mutate(case_when(region == 'World' ~ 'yes', .default = 'no')) #Make a boolean column to highlight world
colnames(ChildhoodMortalityRate)[4] = 'world'
ChildhoodMortalityRate = ChildhoodMortalityRate %>% 
  tidylog::filter(region %!in% c('Europe and Central Asia','Sub-Saharan Africa')) #Avoid double counting. E.g: Western Europe is part of Europe and Central Asia region

## 3. Make the Chart
RegionColors = c('Eastern and Southern Africa' = '#780000',
                 'West and Central Africa' = '#c1121f',
                 'Middle East and North Africa' = '#ffe169',
                 'South Asia' = '#edc531',
                 'East Asia and Pacific' = '#c9a227',
                 'Latin America and Caribbean' = '#386641',
                 'North America' = '#6a994e',
                 'Eastern Europe and Central Asia' = '#0096c7',
                 'Western Europe' = '#03045e',
                 'World' = '#495057')
RegionLineWidths = c('Eastern and Southern Africa' = 3,
                     'West and Central Africa' = 3,
                     'Middle East and North Africa' = 3,
                     'South Asia' = 3,
                     'East Asia and Pacific' = 3,
                     'Latin America and Caribbean' = 3,
                     'North America' = 3,
                     'Eastern Europe and Central Asia' = 3,
                     'Western Europe' = 3,
                     'World' = 9)

ChildhoodMortalityChart = 
  plot_ly(ChildhoodMortalityRate, x = ~year, type = "scatter", mode = "lines",
          y = ~mortality_rate, color = ~region, colors = RegionColors, 
          hovertemplate = paste("<b>%{text}</b><extra></extra>"),
          text = ~paste(region, ": ", round(mortality_rate, 1)),
          showlegend = FALSE,
          line = list(width = ~RegionLineWidths[region]),
          line = list(shape = 'spline')) %>%
  layout(title = list(text = "Child Under 5 Mortality Rate", y = 0.9,
                      font = list(size = 42, color = '#000000')),
         xaxis = list(title = "", showgrid = FALSE, tickvals = seq(1990,2020,5),
                      tickfont = list(family = "Arial", size = 18, color = "#333132"),
                      zeroline = FALSE, zerolinewidth = 0, zerolinecolor = NULL),
         yaxis = list(title = "",showgrid = TRUE, linewidth = 4, linecolor = "#acb0bd", 
                      range = c(0,200),tickvals = seq(0,200,25), ticktext = c('',seq(25,200,25)),
                      zeroline = TRUE, zerolinewidth = 5, zerolinecolor = "#000000",
                      tickfont = list(family = "Arial", size = 18, color = "#333132")),
         shapes = list(list(type = "rect", x0 = 1990, y0 = 0, x1 = 2021, y1 = 25,
                line = list(color = "black", width = 0), fillcolor = "#333132", opacity = 0.25)),
         annotations = list(list(
           text = "SDG Target for 2030",
           x = 1993, y = 20, showarrow = FALSE,
           font = list(family = "Arial", size = 22, color = "black"))),
         plot_bgcolor = "#ffffff",
         paper_bgcolor = "#ffffff")

ChildhoodMortalityChart

htmlwidgets::saveWidget(
  widget = ChildhoodMortalityChart, #the plotly object
  file = paste0(ProjectDirectory,"ChildhoodMortalityChart.html"), #the path & file name
  selfcontained = TRUE #creates a single html file
)
