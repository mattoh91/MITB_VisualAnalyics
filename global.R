packages <- c('DT', 'data.table', 'tidyverse', 'shinydashboard', 'RColorBrewer', 'countrycode', 
              'sunburstR', 'treemap', 'ggplot2', 'geofacet', 'networkD3', 'plotly','stringr')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

setwd("D:/RStudio/Visual Analytics/Project/data")

load(file = "data_sample.Rda")
load(file = "data_sunburst.Rda")
load(file = "data_tree_export.Rda")
load(file = "data_tree_import.Rda")
load(file = "data_eu.Rda")
load(file = "data_sea.Rda")
load(file = "data_oecd.Rda")
load(file = "data_sankey_import.Rda")
load(file = "data_sankey_export.Rda")
load(file = "data_choropleth.Rda")
load(file = "data_choropleth_full.Rda")
load(file = "data_years.Rda")
load(file = "data_consolidated.Rda")
load(file = "data_r2_tradeposition.Rda")
load(file = "data_r2_import.Rda")
load(file = "data_r2_export.Rda")
load(file = "data_corrtrellis_tradeposition.Rda")
load(file = "data_corrtrellis_import.Rda")
load(file = "data_corrtrellis_export.Rda")
load(file = "data_timeseries.Rda")