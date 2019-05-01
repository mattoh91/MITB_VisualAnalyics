#Libraries
library(data.table)
library(tidyverse)
library(countrycode)
library(geofacet)
library(plotly)

#Set working directory
setwd("D:/RStudio/Visual Analytics/Project/data")

#Load data
data <- fread("commodity_trade_statistics_data.csv",sep=",")

#Data preparation
data$comm_code <- NULL
data$commodity <- NULL

colnames(data) <- c("Country", "Year", "Flow", "Trade.USD", "Weight.KG", "Qty.Name", "Qty", "Category")

data <- data %>% 
  filter(Flow %in% c("Export", "Import")) %>%
  filter(Year %in% c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")) %>%
  mutate(Category.class = substr(Category, start=1, stop=2))

metals <- as.character(c(28,72,81,74,76,71,73,26,75,79,78,80))
data.metals <- data %>%
  filter(Category.class %in% metals) %>%
  mutate(Category.main = "Metals")

agriculture <- as.character(c(31,29,11,12,09,08,07,20,19,44,48,40,52,53,23,10,06,18,13,45,14))
data.agriculture <- data %>%
  filter(Category.class %in% agriculture) %>%
  mutate(Category.main = "Agriculture")

livestock.and.meat <- as.character(c(02,16,04,03,42,01))
data.livestock.and.meat <- data %>%
  filter(Category.class %in% livestock.and.meat) %>%
  mutate(Category.main = "Livestock and Meat")

energy <- as.character(27,84)
data.energy <- data %>% 
  filter(Category.class %in% energy) %>%
  mutate(Category.main = "Energy")

data.cleaned <- rbind(data.metals, data.agriculture, data.livestock.and.meat, data.energy)

#Country errors
ISO <- factor(countrycode(sourcevar=data.cleaned$Country, origin='country.name', destination='iso3c'))

errors <- c("Central African Rep.", "EU-28", "FS Micronesia", "Neth. Antilles", "Other Asia, nes")

countrycode::codelist$country.name.en #List of country names that can be encoded into iso3c usign countrycode package.
#To recode: "Central African Republic", "Micronesia (Federated States of)", "Netherlands Antilles"
#To remove: "EU-28", "Other Asia", "nes" (=not elsewhere specificed)

data.cleaned$Country[data.cleaned$Country == "Central African Rep."] = "Central African Republic"
data.cleaned$Country[data.cleaned$Country == "FS Micronesia"] = "Micronesia (Federated States of)"
data.cleaned$Country[data.cleaned$Country == "Neth. Antilles"] = "Netherlands Antilles"

data.cleaned <- data.cleaned %>% 
  filter(!Country %in% c("EU-28", "Other Asia, nes"))

data.cleaned$ISO <- factor(countrycode(sourcevar=data.cleaned$Country, origin='country.name', destination='iso3c'))

#Manually encode ISO code for Netherlands Antilles = "ANT".
length(levels(data.cleaned$ISO))
levels(data.cleaned$ISO) <- c(levels(data.cleaned$ISO), "ANT")
length(levels(data.cleaned$ISO))
data.cleaned$ISO[data.cleaned$Country == "Netherlands Antilles"] = "ANT"

save(data.cleaned,file="data_matt.Rda")

#------------------------Geofacet------------------------

#Note to self: Spread the import and export values, then summarize, then calculate trade position
cols2keep <- c("Country", "Year", "Flow", "Trade.USD", "Category", "Category.main", "ISO")

#Note that Trade.USD is in billions after this aggregation.
data.geo.mean <- data.cleaned[,(colnames(data.cleaned) %in% cols2keep)] %>%
  group_by(Country,Year,Flow,Category,Category.main,ISO) %>%
  summarize(Trade.USD = mean(Trade.USD)/1000000)

data.geo.import <- data.sum %>%
  filter(Flow == "Import")

data.geo.export <- data.sum %>%
  filter(Flow == "Export")

ggplot(data.sum.export, aes(Year, Trade.USD)) +
  geom_line() +
  facet_geo(~ ISO, grid = "world_countries_grid1") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  labs(title = "Title",
       caption = "Data Source:",
       x = "Year",
       y = "Export (USD)") +
  theme(strip.text.x = element_text(size = 6))

#-----------Choropleth Data (w/o 'Others' Commodity Category)-------------
# data prep (Not summarized on main category level)
data.choropleth <- data.cleaned[,(colnames(data.cleaned) %in% cols2keep)] %>%
  group_by(Country,Year,Flow,Category,Category.main,ISO) %>%
  summarize(Trade.USD = mean(Trade.USD)/1000000) %>%
  dcast(...~Flow, value.var="Trade.USD", fill=0) %>%
  mutate(Trade.Position = Export - Import) %>%
  melt(id.vars = c("Country","Year","Category","Category.main","ISO"),
       measure.vars = c("Export","Import","Trade.Position"), variable.name = "Flow",
       value.name = "Trade.USD")

save(data.choropleth,file="data_choropleth.Rda")

#------------------Choropleth Data (All Categories)------------------------

#Csv containing manual mapping of main trade commodity group to HS commodity category.
data.com.groupings <- fread("commodity_categories.csv",sep=",",header=TRUE,select=c(1,2))

#Inner join by Category
data.grouped <- merge(data, data.com.groupings, by="Category")

#Country errors
ISO <- factor(countrycode(sourcevar=data.grouped$Country, origin='country.name', destination='iso3c'))

errors <- c("Central African Rep.", "EU-28", "FS Micronesia", "Neth. Antilles", "Other Asia, nes")

countrycode::codelist$country.name.en #List of country names that can be encoded into iso3c usign countrycode package.
#To recode: "Central African Republic", "Micronesia (Federated States of)", "Netherlands Antilles"
#To remove: "EU-28", "Other Asia", "nes" (=not elsewhere specificed)

data.grouped$Country[data.grouped$Country == "Central African Rep."] = "Central African Republic"
data.grouped$Country[data.grouped$Country == "FS Micronesia"] = "Micronesia (Federated States of)"
data.grouped$Country[data.grouped$Country == "Neth. Antilles"] = "Netherlands Antilles"

data.full.cleaned <- data.grouped %>% 
  filter(!Country %in% c("EU-28", "Other Asia, nes"))

data.full.cleaned$ISO <- factor(countrycode(sourcevar=data.full.cleaned$Country, origin='country.name', destination='iso3c'))

#Manually encode ISO code for Netherlands Antilles = "ANT".
length(levels(data.full.cleaned$ISO))
levels(data.full.cleaned$ISO) <- c(levels(data.full.cleaned$ISO), "ANT")
length(levels(data.full.cleaned$ISO))
data.full.cleaned$ISO[data.full.cleaned$Country == "Netherlands Antilles"] = "ANT"

# data prep (Not summarized on main category level)
data.choropleth.full <- data.full.cleaned[,(colnames(data.full.cleaned) %in% cols2keep)] %>%
  group_by(Country,Year,Flow,Category,Category.main,ISO) %>%
  summarize(Trade.USD = mean(Trade.USD)/1000000) %>%
  dcast(...~Flow, value.var="Trade.USD", fill=0) %>%
  mutate(Trade.Position = Export - Import) %>%
  melt(id.vars = c("Country","Year","Category","Category.main","ISO"),
       measure.vars = c("Export","Import","Trade.Position"), variable.name = "Flow",
       value.name = "Trade.USD")

save(data.choropleth.full,file="data_choropleth_full.Rda")

#---------------------GDP Data-------------------
data.GDP <- fread("data_gdp.csv",sep=",", header = TRUE)

data.GDP.cleaned <- data.GDP %>%
  melt(id.vars=c('Country Name', 'Country Code'),
       variable.name='Year', value.name='GDP') %>%
  replace_na(list(GDP = 0)) %>%
  mutate(GDP.mil = GDP/1000000)

data.GDP.forjoin <- data.GDP.cleaned[,c('Country Code','Year','GDP.mil')]

#---------------------Commodity+GDP Data-------------------

#The following 5 Countries present in the Trade Commodity dataset but not present in the GDP dataset will
#be excluded from the inner join: c("Anguilla","Cook Isds","Mayotte","Montserrat","Netherlands Antilles").
data.consolidated <- merge(data.choropleth,data.GDP.forjoin, by.x=c('ISO','Year'), 
                     by.y=c('Country Code','Year'))

data.consolidated <- data.consolidated %>%
  mutate(Category.clean = gsub("_"," ",substr(Category,4,nchar(Category))))

save(data.consolidated,file="data_consolidated.Rda")

#There will be warning messages for Countries with standard dev = 0 because they had missing 
#GDP values in the GDP source data. These will eventually be filtered out so just ignore.
#Filtered for countries with r2 > 0.7, and with >= 5 years worth of data.
data.r2.tradeposition <- data.consolidated %>%
  filter(Flow == "Trade.Position") %>%
  group_by(Country,Year) %>%
  summarize(Trade.USD = sum(Trade.USD), GDP.mil = first(GDP.mil)) %>%
  filter(n() >= 5) %>%
  summarize(r = cor(Trade.USD,GDP.mil), r2 = cor(Trade.USD,GDP.mil)^2) %>%
  arrange(desc(r)) %>%
  filter(r2 >= 0.7)

data.r2.import <- data.consolidated %>%
  filter(Flow == "Import") %>%
  group_by(Country,Year) %>%
  summarize(Trade.USD = sum(Trade.USD*-1), GDP.mil = first(GDP.mil)) %>%
  filter(n() >= 5) %>%
  summarize(r = cor(Trade.USD,GDP.mil), r2 = cor(Trade.USD,GDP.mil)^2) %>%
  arrange(desc(r)) %>%
  filter(r2 >= 0.7)

data.r2.export <- data.consolidated %>%
  filter(Flow == "Export") %>%
  group_by(Country,Year) %>%
  summarize(Trade.USD = sum(Trade.USD), GDP.mil = first(GDP.mil)) %>%
  filter(n() >= 5) %>%
  summarize(r = cor(Trade.USD,GDP.mil), r2 = cor(Trade.USD,GDP.mil)^2) %>%
  arrange(desc(r)) %>%
  filter(r2 >= 0.7)

data.corrtrellis.tradeposition <- data.consolidated %>%
  filter(Country %in% data.r2.tradeposition$Country) %>%
  filter(Flow == "Trade.Position") %>%
  group_by(Country,Year) %>%
  summarize(Trade.USD = sum(Trade.USD), GDP.mil = first(GDP.mil)) %>%
  filter(n() >= 5)

data.corrtrellis.import <- data.consolidated %>%
  filter(Country %in% data.r2.import$Country) %>%
  filter(Flow == "Import") %>%
  group_by(Country,Year) %>%
  summarize(Trade.USD = sum(Trade.USD*-1), GDP.mil = first(GDP.mil)) %>%
  filter(n() >= 5)

data.corrtrellis.export <- data.consolidated %>%
  filter(Country %in% data.r2.export$Country) %>%
  filter(Flow == "Export") %>%
  group_by(Country,Year) %>%
  summarize(Trade.USD = sum(Trade.USD), GDP.mil = first(GDP.mil)) %>%
  filter(n() >= 5)

save(data.r2.tradeposition,file="data_r2_tradeposition.Rda")
save(data.r2.import,file="data_r2_import.Rda")
save(data.r2.export,file="data_r2_export.Rda")
save(data.corrtrellis.tradeposition,file="data_corrtrellis_tradeposition.Rda")
save(data.corrtrellis.import,file="data_corrtrellis_import.Rda")
save(data.corrtrellis.export,file="data_corrtrellis_export.Rda")

#---------------------Time-Series Data---------------------

data.timeseries.import <- data.choropleth %>%
  filter(Flow == 'Import') %>%
  group_by(Country, Category, ISO) %>%
  summarize(Trade.USD = mean(Trade.USD)) %>%
  group_by(Country) %>%
  arrange(desc(Trade.USD)) %>%
  slice(1:5) %>%
  summarize(Import.cat = list(Category))

data.timeseries.export <- data.choropleth %>%
  filter(Flow == 'Export') %>%
  group_by(Country, Category, ISO) %>%
  summarize(Trade.USD = mean(Trade.USD)) %>%
  group_by(Country) %>%
  arrange(desc(Trade.USD)) %>%
  slice(1:5) %>%
  summarize(Export.cat = list(Category))

data.timeseries <- merge(data.timeseries.import, data.timeseries.export, by = 'Country')

save(data.timeseries,file='data_timeseries.Rda')


#------------------(FYI )SECONDARY PLOTS-------------------

#Trade Vol (USD) by Year
data %>% 
  group_by(Year, Flow) %>%
  summarise(USD=round(sum(Trade.USD)/1000000)) %>%
  mutate(Year.DT=make_date(year=Year),Flow= as.factor(Flow)) %>%
  ggplot(aes(x=Year.DT, y=USD, group=Flow, color=Flow))+  geom_line()+
  geom_point()+labs(y= "USD (in Million)", x="Year", title="Trade (USD) over Time")

#---Horizontal Bargraph of Country Trade Position (USD)---

#---Boxplot and ANOVA for Country Imports and Exports (Normalized)---
