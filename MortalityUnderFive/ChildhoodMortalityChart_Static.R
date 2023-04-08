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
library(countrycodes) #Convert Country Names and Country Codes
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

max(ChildhoodMortalityRate$mortality_rate)

## 3. Make the Chart
labels_year = c("90'","95'","00'","05'","10'","15'","20'") 

ChildhoodMortalityChart = 
  ggplot(ChildhoodMortalityRate, aes(x = year, y = mortality_rate, group = region)) + 
  geom_line(aes(color = region, linewidth = world)) +
  geom_hline(yintercept = 0, linewidth = 2, color = '#000000') +
  geom_hline(yintercept = 25, linewidth = 2, color = '#495057', linetype = 5) +
  scale_linewidth_manual(values = c('yes' = 3, 'no' = 1)) +
  scale_color_manual(values = c('Eastern and Southern Africa' = '#780000',
                                'West and Central Africa' = '#c1121f',
                                'Middle East and North Africa' = '#ffe169',
                                'South Asia' = '#edc531',
                                'East Asia and Pacific' = '#c9a227',
                                'Latin America and Caribbean' = '#386641',
                                'North America' = '#6a994e',
                                'Eastern Europe and Central Asia' = '#0096c7',
                                'Western Europe' = '#03045e',
                                'World' = '#495057')) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5, colour = '#333132', size = 16, margin = unit(c(0,0.5,0,0),'cm')),
        axis.text.y = element_text(hjust = 0.5, colour = '#333132', size = 18, margin = unit(c(0,2,0,0),'cm')),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = '#acb0bd'),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') +
  scale_x_continuous(breaks = seq(1990,2020,5),labels = labels_year) +
  scale_y_continuous(breaks = seq(0,200,25), labels = function(x) format(x, nsmall = 0))

PlotlyChart = ggplotly(ChildhoodMortalityChart)
PlotlyChart = PlotlyChart %>% 
  style(hoverinfo = "text", 
        text = paste("Region: ", ChildhoodMortalityRate$region,
                     "<br>Mortality Rate: ", ChildhoodMortalityRate$mortality_rate))

ChildhoodMortalityRate
tail(ChildhoodMortalityRate)
PlotlyChart


htmlwidgets::saveWidget(
  widget = PlotlyChart, #the plotly object
  file = paste0(ProjectDirectory,"ChildhoodMortalityChart.html"), #the path & file name
  selfcontained = TRUE #creates a single html file
)

ggsave(filename = 'ChildhoodMortalityChart.png',plot = ChildhoodMortalityChart, device = 'png', 
       width = 19.32, height = 12, path = ProjectDirectory) 
ChildhoodMortalityChart

