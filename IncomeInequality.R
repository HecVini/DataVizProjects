## 1. Load Packages
library(tidyverse) # Easily Installand Load the 'Tidyverse' 
library(lubridate) # Make Dealing with Dates a Little Easier 
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data 
library(data.table) # Extension of `data.frame`
library(tidylog) # Logging for 'dplyr' and 'tidyr' Functions 
library(textclean) # Text Cleaning Tools
library(ggh4x) # Hacks for 'ggplot2' 
library(sf) # Simple Features for R 
library(openxlsx) # Read, Write and Edit xlsx Files
library(annotater) # Annotate Package Load Calls
library(pointblank) # Data Validation and Organization of Metadata for Local and Remote Tables
library(rjson) # JSON for R
library(countrycode) #Convert Country Names and Country Codes
library(owidR) #API for Our World in Data
library(devtools) #Make Developing R Packages Easier
library(ungeviz) #For the rounded lines


## 2. Clean Data
ProjectDirectory = 'C:/Users/vinic/OneDrive/GitHub Repo/DataVizProjects/GlobalInequality/'
G20Countries = c('ARG','AUS','BRA','CAN','CHN','FRA','DEU','IND','IDN',
                 'ITA','JPN','KOR','MEX','RUS','SAU','ZAF','TUR','GBR','USA')
SelectedCountries = c('BRA','CHL','COL','MEX','USA',
                      'FRA','DEU','GBR','ITA','RUS',
                      'TUR','CHN','IND','IDN','AUS',
                      'NGA','EGY','ALG')

'%!in%' = function(x,y)!('%in%'(x,y)) #Function to get data not in a list

owid_search('Income') #Search for the chart which uses the dataset
IncomeInequalitytDataset.R = owid('daily-income-of-the-poorest-and-richest-decile')
IncomeInequalitytDataset = IncomeInequalitytDataset.R %>% tibble() %>% clean_names() #Make it a tibble and clean its colnames
IncomeInequalitytDataset = IncomeInequalitytDataset %>% setnames(c('entity_name','iso3c','year','p10','p90','population','continent')) #Rename cols
IncomeInequalitytDataset = IncomeInequalitytDataset %>% filter(year %in% c(2017,2018)) #Only get that from 2017 and 2018
IncomeInequalitytDataset = IncomeInequalitytDataset %>% filter(iso3c %in% SelectedCountries) #Get data from selected countries
IncomeInequalitytDataset = IncomeInequalitytDataset %>% subset(select = c(iso3c,year,p10,p90)) #Get rid of unnecessary columns
IncomeInequalitytDataset = IncomeInequalitytDataset %>% arrange(iso3c,year) #Arrange by iso3c, then by year
IncomeInequalitytDataset = IncomeInequalitytDataset %>% pivot_wider(names_from = year, values_from = c(p10,p90)) #turn into the wider format to easier mutate
IncomeInequalitytDataset = IncomeInequalitytDataset %>% 
  mutate(p90_2018 = case_when(is.na(p90_2018) ~ p90_2017 ,.default = p90_2018)) #Make the p90 of 2017 the p90 for 2018 if the country has no  data for 2018
IncomeInequalitytDataset = IncomeInequalitytDataset %>% 
  mutate(p10_2018 = case_when(is.na(p10_2018) ~ p10_2017 ,.default = p10_2018)) #Make the p10 of 2017 the p10 for 2018 if the country has no  data for 2018. This happend in EGY, GBR and CHL
IncomeInequalitytDataset = IncomeInequalitytDataset %>% subset(select = c(iso3c,p10_2018,p90_2018)) #Get the necessary cols
IncomeInequalitytDataset = IncomeInequalitytDataset %>% setnames(c('iso3c','p10','p90')) #Rename it
IncomeInequalitytDataset = IncomeInequalitytDataset %>% pivot_longer(cols = c(2,3), 
                                               names_to = 'percentile', 
                                               values_to = 'income_level') #Make into the long format (necessary for the chart). cols for iso3c, percentile (p10 or p90) and income level to be in the percentile

## 3. Make the Chart
CountriesOrder = IncomeInequalitytDataset %>% filter(percentile == 'p10') #I want to order from the lowest to highest P10 level
CountriesOrder = CountriesOrder %>% arrange(income_level) #Arrange from lowest to highest
CountriesOrder = CountriesOrder %>% subset(select = iso3c) #only get the countries, in order
CountriesOrder = CountriesOrder$iso3c # Make it a list to use below
IncomeInequalitytDataset$iso3c = factor(IncomeInequalitytDataset$iso3c,levels = CountriesOrder) #Reorder countries for the chart

IncomeInequalityChart =
  ggplot(IncomeInequalitytDataset, aes(x = iso3c, y = income_level, color = percentile, size = 4,width = .5)) +
  geom_hpline(stat = "summary", lineend = 'round',) +
  scale_color_manual(values = c('p10' = '#c1121f', 'p90' = '#0077b6')) +
  geom_hline(yintercept = 0, size = 1, color = '#000000') +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(hjust = 0.5, colour = '#ffffff', size = 5, margin = unit(c(0,0,.25,0),'cm'),angle = 0),
        axis.text.y = element_text(hjust = 1, colour = '#333132', size = 26, margin = unit(c(0,.5,0,0),'cm'),angle = 0),
        axis.title = element_blank(),
        axis.ticks.length.x = unit(.5,'cm'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none',
        legend.direction = 'vertical',
        legend.title = element_text(colour = '#333132', vjust = 0.7, hjust = 0.5, size = 16),
        legend.text = element_text(colour = '#333132', size = 22, margin = unit(c(0,1.5,0,0),'cm')),
        legend.key.size = unit(1,'cm'),
        legend.key = element_rect(fill = NA),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.spacing.y = unit(0.3, 'cm')) +
  scale_y_continuous(breaks = seq(0,160,20), limits = c(0,160), labels = c('',seq(20,160,20))) +
  geom_rect(aes(xmin=1.75, xmax=0, ymin=160, ymax=160), 
            fill= "#ffffff", colour = "#ffffff", alpha = 1)
  
ggsave(file = 'IncomeInequalityChart.png', path = ProjectDirectory,
       plot = IncomeInequalityChart, device = 'png',
       width = 25, height = 12) #Save it on my folder
