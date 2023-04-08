


# DataViz
DataViz is an exercise of patience, effort, and constant learning. As an enthusiast, this repository aims to track some visualizations and (hopefully) inspire people. 

## [A World of Inequalities](https://github.com/HecVini/DataVizProjects/blob/main/GlobalInequality/IncomeInequality.R)
The world is unequal, but how it looks like. Using data from the World Bank and organized by the Our World in Data team, we notice that income inequality is a reality within and between countries. Using ungeviz::geom_hpline() we can better visualize this reality with point-like short-line segments. That's an alternative to the traditional scatterplots. Further discussion on the data used can be found on [this thread](https://twitter.com/hec_vini/status/1644028833236918279).
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/GlobalInequality/GlobalInequality_Day6.png" >
</p>

## [2022 Brazilian Elections](https://github.com/HecVini/DataVizProjects/blob/main/Brazilian2022Elections_DorlingCartogram/2022BrazilianElectionResults_DorlingCartogram.R)
In late 2022, Lula and Bolsonaro run for the Brazilian Presidency. Understanding the dynamics of votes in all 5570 Brazilian cities might be a challenging duty. But by grouping it by immediate regions, we can understand it better. Also, Dorling cartograms are a really useful tool to reduce the influence of large, but sparsely populated regions. [Here](https://twitter.com/hec_vini/status/1577553930837524481) there are some ideas on it:
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/Brazilian2022Elections_DorlingCartogram/Eleicoes2022_RegiaoImediata.png" >
</p>

## [Under Five Child Mortality Rates](https://github.com/HecVini/DataVizProjects/blob/main/MortalityUnderFive/ChildhoodMortalityScript.R)
We often think that things are always getting worse, but that's not the whole story. In just 20 years, mankind halved the child mortality rate. In 2000, 1 in every 13 children under five died. In 2021, it was 1 in 13. There is a lot of work yet to be done and a lot of regions still have unacceptable high rates, but it's undeniable that progress has been done.

This was my first attempt to use {Plotly} for R. The chart lacks better aesthetics, but it's a first try. This chart was made for Day 4 of the #30DayChartChallenge. Further discussion on the data used can be found on [this thread](https://twitter.com/hec_vini/status/1643484384819806208).
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/MortalityUnderFive/ChildhoodMortalityFinal.gif" >
</p>

## [2021 Brazilian Exports](https://github.com/HecVini/DataVizProjects/blob/main/Exportacoes2021/ExportacoesBrasil.R)
In 2021, Brazil exported nearly USD 280 Bn in goods. The lead export was iron ore, and the more significant export state was São Paulo. Voronoi Treemaps are such a valuable tool to display proportions. Each area is equivalent to the variant weight, and colors are helpful to categorize variables. This viz was inspired by Raul Amoros and Jennifer West's work for [Visual Capitalist](https://www.visualcapitalist.com/us-goods-exports-by-state/).
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/Exportacoes2021/ExportacoesBrasileiras2021.png" >
</p>


## [Brazilian Power Plants](https://github.com/HecVini/DataVizProjects/blob/main/30DayMapChallange_2022/Day1_Points/PowerPlantsBrazil.R)
Brazil produces a lot of energy. How it is made and from where it comes from. Using data from the Brazilian electricity grid operator, we can see that the country is a hydropower leader and green energy is growing. Unlike other emerging economies, such as India and China, coal has a minimal share of the country's grid. This plot was made for the first day of the #30DayMapChallange in late 2022.
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/30DayMapChallange_2022/Day1_Points/BrazilianPowerPlants.png" >
</p>

## [The Middle Passage, Vizualized](https://github.com/HecVini/TidyTuesday/blob/main/week25.R)
This chord diagram portrays one of the most tragic events in the history of mankind: the middle passage. In nearly 300 years, almost 10m enslave people crossed the Atlantic to the Americas. 30% of it went to Brazil, the other 60%, to the Caribbean, and the remaining 10%, to the US and Hispanic America. Chord Diagrams are excellent to show fluxes. 
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/PreviousViz/MiddlePassage.jpg" >
</p>

## The Burning Amazon
The rainforest is catching fire. After a steady decline in deforestation rates, devastation began to rise again in the last decade. But this time is different. Amazonas now has the second largest deforestation rate in the region - just after the longstanding leader Pará. For years, the problem was restrained to the states of Pará, Mato Grosso, and Rondônia. Now it is going north, and the Amazon is reaching the turning point. If we reach it, no effort can restore the forest.
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/PreviousViz/DesmatamentoAmazoniaUFs.png" >
</p>

## Do You Consider Yourself Rich?
Brazil is one of the most unequal countries. The GDP per capita is about 7K USD (or 14k USD, adjusted by purchase power parity) per year. But this income is badly distributed. If you are in a family with a monthly per capita household of 4 thousand BRL (roughly 800 USD per month), you might be in the top 10% - and this level quickly goes down and we reach the poorest regions in the country.
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/PreviousViz/DesigualdadeDeRendaBrasil.png" >
</p>

## [Dog Breeds](https://github.com/HecVini/TidyTuesday/blob/main/week5.R)
This was my first ever #TidyTuesday challenge, back in early 2022. The default dataset was a ranking of dog breeds, according to the American Kennel Club methodology. So the question was: are the cuter and more funny dog breeds also harder to take care of? It seems to be.
<p align="center">
  <img width="800" src="https://github.com/HecVini/DataVizProjects/blob/main/PreviousViz/DogBreeds.png" >
</p>

