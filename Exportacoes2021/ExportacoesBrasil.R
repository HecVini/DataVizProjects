#### 1. Load Packages ####
library(tidyverse) # Easily Install and Load the 'Tidyverse' 
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
library(cartogram) # Create Cartograms with R
library(voronoiTreemap) # Voronoi Treemaps with Added Interactivity by Shiny
library(ggvoronoi) # Voronoi Diagrams and Heatmaps with 'ggplot2'
library(plotly) # Create Interactive Web Graphics via 'plotly.js'
library(ggplotify) # Convert Plot to 'grob' or 'ggplot' Object
library(rjson) # JSON for R
library(WeightedTreemaps) # Generate and Plot Voronoi or Sunburst Treemaps from Hierarchical Data

#### 2. Load Raw Data ####
project_directory = '/Users/hec_vini/Library/CloudStorage/OneDrive-Personal/GitHub Repo/DataVizProjects/Exportacoes2021/'
#Just set the path for your working directory

StatesCodes.R = geobr::read_state('all') %>% tibble() #Tiblle with Brazilian states data
ExportacoesBrasil.R = fread(paste0(project_directory,'ExportacoesBrasileiras2021.csv')) %>% clean_names() %>% tibble() #Tibble with Brazilian Exports
TabelasAuxiliares.R = fread(paste0(project_directory,'TabelasAuxiliaresComexStat.csv')) %>% clean_names() #Tibble with exports nomenclature

#### 3. Clean Data ####
StatesCodes = StatesCodes.R %>% subset(select = c(code_state,abbrev_state,name_state))
StatesCodes = StatesCodes %>% setnames(c('id_state','code_state','name_state')) #DataFrame with Brazilian states IDs, names, etc.

TabelasAuxiliares = TabelasAuxiliares.R %>% 
  setnames(c('code_sh2','desc_sh2','code_sh4','desc_sh4','code_sh6','desc_sh6','code_ncm','desc_ncm'))
TabelasAuxiliares[,c(1,3,5,7)] = TabelasAuxiliares[,c(1,3,5,7)] %>% lapply(function(x) as.character(as.integer(x)))
TabelasAuxiliares = TabelasAuxiliares %>% tibble()
TabelasAuxiliares = TabelasAuxiliares %>% mutate(
  code_sh2 = case_when(nchar(code_sh2) == 1 ~ paste0('0',code_sh2),
                       TRUE ~ as.character(code_sh2)),
  code_sh4 = case_when(nchar(code_sh4) == 3 ~ paste0('0',code_sh4),
                       TRUE ~ as.character(code_sh4)),
  code_sh6 = case_when(nchar(code_sh6) == 5 ~ paste0('0',code_sh6),
                       TRUE ~ as.character(code_sh6)),
  code_ncm = case_when(nchar(code_ncm) == 7 ~ paste0('0',code_ncm),
                       TRUE ~ as.character(code_ncm))) #Clean and organize HS codes. I used chr (insted of dbl) because the number of characters matter to set the code. 
TabelasAuxiliares = TabelasAuxiliares %>% subset(select = c(1,3,5,7,2,4,6,8))
TabelasAuxiliares = TabelasAuxiliares %>% arrange(code_ncm) #Dataframe with each product NCM and SH. SH2: less specific. SH6: more specific. NCM: Mercosul common names 

TabelasAuxiliaresValidation = 
  create_agent(tbl = TabelasAuxiliares) %>%
  rows_distinct() %>%
  interrogate() #Validate the code. Check if there is no duplicates. Made with {pointblank}

SH2Codes = TabelasAuxiliares %>% subset(select = c(code_sh2,desc_sh2)) %>% unique() #Dataframe with distinct HS2 codes 
SH4Codes = TabelasAuxiliares %>% subset(select = c(code_sh4,desc_sh4)) %>% unique() #Dataframe with distinct HS4 codes 

ExportacoesBrasil = ExportacoesBrasil.R %>% subset(select = c(state,sh2_code,sh4_code,us_fob)) #From raw data, select desired columns
ExportacoesBrasil[,c(2,3)] = ExportacoesBrasil[,c(2,3)] %>% lapply(function(x) as.character(as.integer(x))) #Set ideal formats
ExportacoesBrasil[,4] = ExportacoesBrasil[,4] %>% lapply(function(x) as.double(as.numeric(x)))
ExportacoesBrasil = ExportacoesBrasil %>% setnames(c('name_state','code_sh2','code_sh4','fob')) #Arrange columns
ExportacoesBrasil = ExportacoesBrasil %>% mutate(
  code_sh2 = case_when(nchar(code_sh2) == 1 ~ paste0('0',code_sh2),
                       TRUE ~ as.character(code_sh2)),
  code_sh4 = case_when(nchar(code_sh4) == 3 ~ paste0('0',code_sh4),
                       TRUE ~ as.character(code_sh4))) #Properly name code columns. nchar matters here. "07" is not "7"
ExportacoesBrasil = ExportacoesBrasil %>% 
  mutate(name_state = case_when(name_state == 'Rio de Janeiro' ~ 'Rio De Janeiro',
                                name_state == 'Rio Grande do Sul' ~ 'Rio Grande Do Sul',
                                name_state == 'Rio Grande do Norte' ~ 'Rio Grande Do Norte',
                                name_state == 'Mato Grosso do Sul' ~ 'Mato Grosso Do Sul',
                                name_state == 'Espírito Santo' ~ 'Espirito Santo',
                                TRUE ~ as.character(name_state))) #Match base states names with those provided by {geobr}
ExportacoesBrasil = ExportacoesBrasil %>% mutate(fob = fob/10^9) #Tranform nominal USD into bn of USD.
ExportacoesBrasil = left_join(ExportacoesBrasil,StatesCodes, by = 'name_state')
ExportacoesBrasil = ExportacoesBrasil %>% drop_na(id_state)
ExportacoesBrasil = ExportacoesBrasil %>% subset(select = c(id_state,code_state,code_sh2,code_sh4,fob)) #Rearrange
ExportacoesBrasil = ExportacoesBrasil %>% 
  mutate(region = case_when(id_state %in% c(10:19) ~ 'N',
                            id_state %in% c(21:29) ~ 'NE',
                            id_state %in% c(30:39) ~ 'SE',
                            id_state %in% c(40:49) ~ 'S',
                            id_state %in% c(50:59) ~ 'CO')) #N = North, NE = Northeast,...

TopStates_SH2 = ExportacoesBrasil %>% group_by(id_state,code_sh2) %>% 
  summarise(fob = sum(fob),code_state = code_state, region = region) %>% 
  ungroup() %>% arrange(desc(fob)) %>% unique()
TopStates_SH2 = left_join(TopStates_SH2,SH2Codes,by = 'code_sh2') %>% unique()
TopStates_SH2 = TopStates_SH2 %>% subset(select = c(id_state,code_state,region,code_sh2,fob,desc_sh2)) #Get total exports by HS2, for each state

TopStates_SH4 = ExportacoesBrasil %>% group_by(id_state,code_sh4) %>% 
  summarise(fob = sum(fob),code_state = code_state, region = region) %>% 
  ungroup() %>% arrange(desc(fob)) %>% unique()
TopStates_SH4 = left_join(TopStates_SH4,SH4Codes,by = 'code_sh4') %>% unique()
TopStates_SH4 = TopStates_SH4 %>% subset(select = c(id_state,code_state,region,code_sh4,fob,desc_sh4)) #Get total exports by HS4, for each state

TopStates = ExportacoesBrasil %>% group_by(id_state) %>% 
  summarise(fob = sum(fob),code_state = code_state, region = region) %>% 
  ungroup() %>% arrange(desc(fob)) %>% unique()
TopStates = TopStates %>% subset(select = c(id_state,code_state,region,fob)) #Get total exports, for each state

TopExports = ExportacoesBrasil %>% group_by(code_sh2) %>% 
  summarise(fob = sum(fob)) %>% ungroup() %>% 
  arrange(desc(fob)) %>% unique()
TopExports = left_join(TopExports,SH2Codes,by = 'code_sh2') %>% unique()
TopExports = TopExports %>% subset(select = c(code_sh2,fob,desc_sh2)) #Get top exports by HS2, for the whole country

#### 4. Make the Map ####
TopStatesMapData = TopStates
TopStatesMapData = TopStatesMapData %>% subset(select = -id_state)
TopStatesMapData = TopStatesMapData %>% 
  mutate(color = case_when(region == 'N' ~ '#588157',
                           region == 'NE' ~ '#e63946',
                           region == 'CO' ~ '#bc6c25',
                           region == 'SE' ~ '#457b9d',
                           region == 'S' ~ '#a8dadc'))
TopStatesMapData = TopStatesMapData %>% mutate(total = 'Brasil')
TopStatesMapData = TopStatesMapData %>% mutate(weight_fob = (fob/sum(fob))*100)
TopStatesMapData = TopStatesMapData %>% subset(select = c(total,region,code_state,color,weight_fob)) #Organize data for the Voronoi Diagram. 

#Check the {WeightedTreemaps} documentation for further details on Voronoi Treemaps on R
TopStatesMap = voronoiTreemap(data = TopStatesMapData,
                              levels = c('total','region','code_state'),
                              cell_size = "weight_fob",
                              shape = "circle",maxIteration = 100,
                              seed = 23,positioning = 'clustered_by_area',sort = TRUE,
                              verbose = FALSE)
DrawTopStatesMap = drawTreemap(TopStatesMap,color_type = 'categorical',color_level = 2,
                               color_palette = c('#ffc300','#008000','#e71d36','#a8dadc','#0096c7'),
                               border_level = c(1,2,3),border_size = c(10,2,2),
                               border_color = c('#000000','#ffffff','#ffffff'),
                               label_level = 3, label_size = .5,title = NULL, legend= FALSE,add = TRUE)
#Image saved mannualy. I think there's no "ggsave()" function for this type of object.

#### 5. Make the BarChart
TopExportsChartData = TopExports %>% subset(select = c(code_sh2,fob))
TopExportsChartData = TopExportsChartData %>% 
  mutate(code_sh2 = case_when(fob < 14 ~ '00', TRUE ~ as.character(code_sh2)))
TopExportsChartData = TopExportsChartData %>% group_by(code_sh2) %>%
  summarise(fob = sum(fob)) %>% ungroup() %>% arrange(desc(fob))
TopExportsChartData = TopExportsChartData %>% mutate(xaxis = 'xaxis')
TopExportsChartData$code_sh2 = 
  factor(TopExportsChartData$code_sh2,levels = 
           rev(c('26','72','12','02','27','00'))) #Organize the data for the barchart.

TopExportsChart = 
  ggplot(TopExportsChartData, aes(x = xaxis, y = fob, fill = code_sh2)) +
  geom_bar(position = 'fill', stat = 'identity', width = .35) +
  geom_hline(yintercept = 0, size = 2, color = '#000000') +
  geom_hline(yintercept = 1, size = 2, color = '#000000') +
  scale_fill_manual(values = c('00' = '#d9d9d9',
                               '26' = '#003049',
                               '12' = '#386641',
                               '27' = '#495057',
                               '02' = '#6a994e',
                               '72' = '#669bbc')) + 
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = .5, vjust = .5,
                                   colour = '#333132', size = 30,
                                   margin = unit(c(0,0,0,0),'cm'),angle = 0),
        axis.title = element_blank(),
        axis.ticks.length.x = unit(.5,'cm'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(color = '#acb0bd'),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') +
  scale_y_continuous(labels = c('',20,40,60,80,'100%'),breaks = seq(0,1,.2))
ggsave(filename = 'TopExportsChart.png', plot = TopExportsChart, width = 5, height = 20, device = 'png', 
       path = paste0(project_directory))

#### 6. Make the little Brazilian Map ####
BrazilPolygonals = read_country() %>% clean_names() %>% tibble()
RegionsPolygonals = read_region() %>% clean_names() %>% tibble()

BrazilianRegionMap = 
  ggplot() +
  geom_sf(RegionsPolygonals, mapping = aes(geometry = geom, fill = name_region), color = '#000000', size = 2) + 
  geom_sf(BrazilPolygonals, mapping = aes(geometry = geom), color = '#000000', size = 4, fill = NA) +
  scale_fill_manual(values = c('Norte' = '#008000',
                               'Nordeste' = '#e71d36',
                               'Sudeste' = '#0096c7',
                               'Sul' = '#a8dadc',
                               'Centro Oeste' = '#ffc300')) +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none',
        legend.direction = 'horizontal',
        legend.title = element_text(colour = '#333132', vjust = 0.7, hjust = 0.5, size = 24),
        legend.text = element_text(colour = '#333132', size = 20),
        legend.key.size = unit(2.5,'cm'))
ggsave(filename = 'BrazilianRegionMap.png',plot = BrazilianRegionMap, width = 19.32, height = 12, device = 'png', 
       path = project_directory)

#Further changes were made on Canvas. Hopefully, I'll use {ggtext} to better annotate on the next chart. 
