#### 1. Load Packages

#1.1 EDA
library('tidyverse')
library('lubridate')
library('stringr')
library('janitor')
library('data.table')
library('tidylog')
library('pointblank')
library('openxlsx')
library('fcuk')
library('dataCompareR')
library('httr')
library('textclean')
library('annotater')

#1.2 DataViz
library('cartogram')
library('reactablefmtr')
library('gt') 
library('ggimage')
library('ggtext')
library('scales')
library('gghighlight')
library('gtExtras')

#1.3 APIs
library('geobr')

#### 2. Load Data
project_directory = '/Users/hec_vini/Library/CloudStorage/OneDrive-Personal/GitHub Repo/DataVizProjects/30DayMapChallange_2022/Day1_Points/'
highlightUsinas = c('Belo Monte','Itaipu (Parte Brasileira)',
                    'Almirante Álvaro Alberto - Unidade II (Antiga Angra II)',
                    'Porto de Sergipe I')
PolygonalsBrasil = geobr::read_country() %>% tibble()

GeracaoEnergia.R = fread(paste0(project_directory,'SIGA_ANEEL.csv'),encoding = 'Latin-1',dec = ',') %>% tibble()
GeracaoEnergia = GeracaoEnergia.R %>% subset(select = c(DatGeracaoConjuntoDados,NomEmpreendimento,SigUFPrincipal,CodCEG,SigTipoGeracao,
                                    DscFaseUsina,MdaPotenciaOutorgadaKw,MdaPotenciaFiscalizadaKw,
                                    NumCoordNEmpreendimento,NumCoordEEmpreendimento))
GeracaoEnergia = GeracaoEnergia %>% filter(DscFaseUsina == 'Operação')
GeracaoEnergia = GeracaoEnergia %>% subset(select = c(NomEmpreendimento,SigTipoGeracao,SigUFPrincipal,CodCEG,MdaPotenciaOutorgadaKw,NumCoordNEmpreendimento,NumCoordEEmpreendimento))
GeracaoEnergia = GeracaoEnergia %>% setnames(c('nome','tipo','uf','id','potencia_instalada','lat','long'))
GeracaoEnergia = GeracaoEnergia %>% arrange(desc(potencia_instalada))
GeracaoEnergia = GeracaoEnergia %>% mutate(potencia_instalada = potencia_instalada/10^3)
GeracaoEnergia = GeracaoEnergia %>% filter(potencia_instalada > 5)
GeracaoEnergia = GeracaoEnergia %>% 
  mutate(tipo = case_when(tipo %in% c('PCH','CGH') ~ 'PCH',TRUE ~ as.character(tipo)))
GeracaoEnergia = GeracaoEnergia %>% filter(lat > -33,lat < 6, long > -74,long < -33)
GeracaoEnergia = GeracaoEnergia %>%
  mutate(highlight = case_when(nome %in% highlightUsinas ~ 'y',TRUE ~ 'n'))

GeracaoEnergia = GeracaoEnergia %>% 
  mutate(highlight = case_when(tipo == 'UHE' & highlight == 'n' ~ 'UHE',
                               tipo == 'UTE' & highlight == 'n' ~ 'UTE',
                               tipo == 'UTN' & highlight == 'n' ~ 'UTN',
                               tipo == 'UFV' & highlight == 'n' ~ 'UFV',
                               tipo == 'EOL' & highlight == 'n' ~ 'EOL',
                               tipo == 'PCH' & highlight == 'n' ~ 'PCH',
                               TRUE ~ 'Highlight'))
GeracaoEnergia = GeracaoEnergia %>% 
  mutate(sorting_group = case_when(highlight == 'Highlight' ~ 'special',TRUE ~ 'ordinary'))
GeracaoEnergia = GeracaoEnergia %>% arrange(sorting_group,desc(potencia_instalada))
GeracaoEnergiaSpecial = GeracaoEnergia %>% filter(sorting_group == 'special')

GeracaoTipo = GeracaoEnergia %>% group_by(tipo) %>%
  summarise(potencia_instalada = sum(potencia_instalada)) %>%
  ungroup()
GeracaoTipo = GeracaoTipo %>% arrange(desc(potencia_instalada))
GeracaoTipo = GeracaoTipo %>% mutate(potencia_instalada = potencia_instalada/10^3)
GeracaoTipo %>% mutate(pct = potencia_instalada/sum(potencia_instalada))

GeracaoTipo %>% summarise(sum(potencia_instalada))
#### 3. Make the Maps
GeracaoEnergiaBrasil_Map = ggplot() +
  geom_sf(data = PolygonalsBrasil,aes(geometry = geom),color = 'NA',size = 1, fill = '#e9ecef') +
  geom_point(data = GeracaoEnergia,aes(x = long,y = lat,size = potencia_instalada, fill = tipo, color = highlight),shape = 21) +
  geom_point(data = GeracaoEnergiaSpecial,aes(x = long,y = lat,size = potencia_instalada),fill = NA, shape = 21,color = '#000000',stroke = 2) +
  scale_size_continuous(range = c(.01,24)) +
  scale_color_manual(values = c('UHE' = '#023e8a','UTE' = '#343a40',
                                'UTN' = '#c1121f','UFV' = '#ffb703',
                                'EOL' = '#55a630','PCH' = '#8ecae6',
                                'Highlight' = '#000000')) +
  scale_fill_manual(values = c('UHE' = '#023e8a','UTE' = '#343a40',
                               'UTN' = '#c1121f','UFV' = '#ffb703',
                               'EOL' = '#55a630','PCH' = '#8ecae6')) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none')
ggsave(filename = 'GeracaoEnergiaBrasil.png',plot = GeracaoEnergiaBrasil_Map, device = 'png', 
       width = 19.32, height = 12, path = project_directory) 


GeracaoTipo$tipo = factor(GeracaoTipo$tipo, levels = rev(c('UHE','UTE','EOL','UFV','PCH','UTN')))
LegendChart = ggplot(GeracaoTipo, aes(x = tipo, y = potencia_instalada, fill = tipo)) +
  geom_bar(stat = 'identity', width = .8) +
  scale_fill_manual(values = c('UHE' = '#023e8a','UTE' = '#343a40',
                               'UTN' = '#c1121f','UFV' = '#ffb703',
                               'EOL' = '#55a630','PCH' = '#8ecae6')) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none') +
  coord_flip()
ggsave(filename = 'LegendChart.png',plot = LegendChart, device = 'png', 
       width = 6, height = 6, path = project_directory) 

LegendChart = 
  ggplot(GeracaoTipo, aes(x = 0, y = potencia_instalada, fill = tipo)) +
  geom_bar(position = 'fill', stat = 'identity', width = .5) +
  geom_hline(yintercept = 0, size = 2, color = '#000000') +
  geom_hline(yintercept = 1, size = 2, color = '#000000') +
  scale_fill_manual(values = c('UHE' = '#023e8a','UTE' = '#343a40',
                               'UTN' = '#c1121f','UFV' = '#ffb703',
                               'EOL' = '#55a630','PCH' = '#8ecae6')) +
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
  scale_y_continuous(labels = c('','','','','',''),breaks = seq(0,1,.2))
ggsave(filename = 'LegendChart.png', plot = LegendChart, width = 5, height = 20, device = 'png', 
       path = paste0(project_directory))
  
  
  
LegendChart
GeracaoTipo
