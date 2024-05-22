################### Part 1 ########################

### Installing Required Libraries ###

install.packages('caret')
install.packages('MASS')
install.packages('Metrics')

### Loading Required Libraries ###

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(MASS)
library(Metrics)

### Loading Datasets ###

path <- '/Users/kshitizsahay/Documents/University of Chicago/ADSP 31007 Statistical Analysis/Final Project/Data/'

df_2016 <- read.csv(paste(path, '2016_brooklyn.csv', sep = ''))
df_2017 <- read.csv(paste(path, '2017_brooklyn.csv', sep = ''))
df_2018 <- read.csv(paste(path, '2018_brooklyn.csv', sep = ''))
df_2019 <- read.csv(paste(path, '2019_brooklyn.csv', sep = ''))
df_2020 <- read.csv(paste(path, '2020_brooklyn.csv', sep = ''))

### Data Standardization and Cleaning ###

# Removing the first few rows that contain metadata (found out after manual inspection) from the yearly housing price datasets
df_2016_2 <- df_2016[-c(1, 2, 3, 4),]
df_2017_2 <- df_2017[-c(1, 2, 3, 4),]
df_2018_2 <- df_2018[-c(1, 2, 3, 4),]
df_2019_2 <- df_2019[-c(1, 2, 3, 4),]
df_2020_2 <- df_2020[-c(1, 2, 3, 4, 5, 6, 7),]

# Renaming the column names across the datasets
df_2016_3 <- df_2016_2 %>% rename(
  borough = BROOKLYN.ANNUALIZE.SALE.FOR.2016....All.Sales.From..January.1..2016...December.31..2016.,
  neighborhood = X,
  bldclasscat = X.1,
  taxclasscurr = X.2,
  block = X.3, 
  lot = X.4,
  easement = X.5,
  bldclasscurr = X.6,
  address = X.7,
  aptnum = X.8,
  zip = X.9,
  resunits = X.10,
  comunits = X.11,
  totunits = X.12,
  landsqft = X.13,
  grosssqft = X.14,
  yrbuilt = X.15,
  taxclasssale = X.16,
  bldclasssale = X.17,
  price = X.18,
  date = X.19)

df_2017_3 <- df_2017_2 %>% rename(
  borough = BROOKLYN.ANNUALIZE.SALE.FOR.2017....All.Sales.From..January.1..2017...December.31..2017.,
  neighborhood = X,
  bldclasscat = X.1,
  taxclasscurr = X.2,
  block = X.3, 
  lot = X.4,
  easement = X.5,
  bldclasscurr = X.6,
  address = X.7,
  aptnum = X.8,
  zip = X.9,
  resunits = X.10,
  comunits = X.11,
  totunits = X.12,
  landsqft = X.13,
  grosssqft = X.14,
  yrbuilt = X.15,
  taxclasssale = X.16,
  bldclasssale = X.17,
  price = X.18,
  date = X.19)

df_2018_3 <- df_2018_2 %>% rename(
  borough = BROOKLYN.ANNUALIZE.SALE.FOR.2018....All.Sales.From..January.1..2018...December.31..2018.,
  neighborhood = X,
  bldclasscat = X.1,
  taxclasscurr = X.2,
  block = X.3, 
  lot = X.4,
  easement = X.5,
  bldclasscurr = X.6,
  address = X.7,
  aptnum = X.8,
  zip = X.9,
  resunits = X.10,
  comunits = X.11,
  totunits = X.12,
  landsqft = X.13,
  grosssqft = X.14,
  yrbuilt = X.15,
  taxclasssale = X.16,
  bldclasssale = X.17,
  price = X.18,
  date = X.19)

df_2019_3 <- df_2019_2 %>% rename(
  borough = BROOKLYN.ANNUAL.SALES.FOR.CALENDAR.YEAR.2019...All.Sales.From.January.2019...December.2019..PTS.Sales.as.of.04.01.2020.,
  neighborhood = X,
  bldclasscat = X.1,
  taxclasscurr = X.2,
  block = X.3, 
  lot = X.4,
  easement = X.5,
  bldclasscurr = X.6,
  address = X.7,
  aptnum = X.8,
  zip = X.9,
  resunits = X.10,
  comunits = X.11,
  totunits = X.12,
  landsqft = X.13,
  grosssqft = X.14,
  yrbuilt = X.15,
  taxclasssale = X.16,
  bldclasssale = X.17,
  price = X.18,
  date = X.19)

df_2020_3 <- df_2020_2 %>% rename(
  borough = BROOKLYN.ANNUAL.SALES.FOR.CALENDAR.YEAR.2020,
  neighborhood = X,
  bldclasscat = X.1,
  taxclasscurr = X.2,
  block = X.3, 
  lot = X.4,
  easement = X.5,
  bldclasscurr = X.6,
  address = X.7,
  aptnum = X.8,
  zip = X.9,
  resunits = X.10,
  comunits = X.11,
  totunits = X.12,
  landsqft = X.13,
  grosssqft = X.14,
  yrbuilt = X.15,
  taxclasssale = X.16,
  bldclasssale = X.17,
  price = X.18,
  date = X.19)

# Checking the record count
records_count <- dim(df_2016_3)[1] + dim(df_2017_3)[1] + dim(df_2018_3)[1] + dim(df_2019_3)[1] + dim(df_2020_3)[1]
print(records_count)

# Creating a copy of the datasets
df_2016_4 <- df_2016_3
df_2017_4 <- df_2017_3
df_2018_4 <- df_2018_3
df_2019_4 <- df_2019_3
df_2020_4 <- df_2020_3

# Creating the `sale_year` variable
df_2016_4$sale_year <- 2016
df_2017_4$sale_year <- 2017
df_2018_4$sale_year <- 2018
df_2019_4$sale_year <- 2019
df_2020_4$sale_year <- 2020

# Cleaning `borough`
df_2016_4["borough"][df_2016_4["borough"] == ''] <- NA
df_2017_4["borough"][df_2017_4["borough"] == ''] <- NA
df_2018_4["borough"][df_2018_4["borough"] == ''] <- NA
df_2019_4["borough"][df_2019_4["borough"] == ''] <- NA
df_2020_4["borough"][df_2020_4["borough"] == ''] <- NA

# Cleaning `neighborhood`
df_2016_4$neighborhood <- trimws(df_2016_4$neighborhood)
df_2017_4$neighborhood <- trimws(df_2017_4$neighborhood)
df_2018_4$neighborhood <- trimws(df_2018_4$neighborhood)
df_2019_4$neighborhood <- trimws(df_2019_4$neighborhood)
df_2020_4$neighborhood <- trimws(df_2020_4$neighborhood)

df_2016_4["neighborhood"][df_2016_4["neighborhood"] == ''] <- NA
df_2017_4["neighborhood"][df_2017_4["neighborhood"] == ''] <- NA
df_2018_4["neighborhood"][df_2018_4["neighborhood"] == ''] <- NA
df_2019_4["neighborhood"][df_2019_4["neighborhood"] == ''] <- NA
df_2020_4["neighborhood"][df_2020_4["neighborhood"] == ''] <- NA

# Cleaning `bldclasscat`
df_2016_4$bldclasscat <- trimws(df_2016_4$bldclasscat)
df_2017_4$bldclasscat <- trimws(df_2017_4$bldclasscat)
df_2018_4$bldclasscat <- trimws(df_2018_4$bldclasscat)
df_2019_4$bldclasscat <- trimws(df_2019_4$bldclasscat)
df_2020_4$bldclasscat <- trimws(df_2020_4$bldclasscat)

df_2016_4["bldclasscat"][df_2016_4["bldclasscat"] == ''] <- NA
df_2017_4["bldclasscat"][df_2017_4["bldclasscat"] == ''] <- NA
df_2018_4["bldclasscat"][df_2018_4["bldclasscat"] == ''] <- NA
df_2019_4["bldclasscat"][df_2019_4["bldclasscat"] == ''] <- NA
df_2020_4["bldclasscat"][df_2020_4["bldclasscat"] == ''] <- NA

# Cleaning `taxclasscurr`
df_2016_4["taxclasscurr"][df_2016_4["taxclasscurr"] == ''] <- NA
df_2016_4["taxclasscurr"][df_2016_4["taxclasscurr"] == '  '] <- NA
df_2017_4["taxclasscurr"][df_2017_4["taxclasscurr"] == ''] <- NA
df_2017_4["taxclasscurr"][df_2017_4["taxclasscurr"] == ' '] <- NA
df_2018_4["taxclasscurr"][df_2018_4["taxclasscurr"] == ''] <- NA
df_2019_4["taxclasscurr"][df_2019_4["taxclasscurr"] == ''] <- NA
df_2020_4["taxclasscurr"][df_2020_4["taxclasscurr"] == ''] <- NA

# Cleaning `block`
df_2016_4["block"][df_2016_4["block"] == ''] <- NA
df_2017_4["block"][df_2017_4["block"] == ''] <- NA
df_2018_4["block"][df_2018_4["block"] == ''] <- NA
df_2019_4["block"][df_2019_4["block"] == ''] <- NA
df_2020_4["block"][df_2020_4["block"] == ''] <- NA

# Cleaning `lot`
df_2016_4["lot"][df_2016_4["lot"] == ''] <- NA
df_2017_4["lot"][df_2017_4["lot"] == ''] <- NA
df_2018_4["lot"][df_2018_4["lot"] == ''] <- NA
df_2019_4["lot"][df_2019_4["lot"] == ''] <- NA
df_2020_4["lot"][df_2020_4["lot"] == ''] <- NA

# Cleaning `easement`
df_2016_4["easement"][df_2016_4["easement"] == ''] <- NA
df_2016_4["easement"][df_2016_4["easement"] == ' '] <- NA
df_2017_4["easement"][df_2017_4["easement"] == ''] <- NA
df_2017_4["easement"][df_2017_4["easement"] == ' '] <- NA
df_2018_4["easement"][df_2018_4["easement"] == ''] <- NA
df_2018_4["easement"][df_2018_4["easement"] == ' '] <- NA
df_2019_4["easement"][df_2019_4["easement"] == ''] <- NA
df_2019_4["easement"][df_2019_4["easement"] == ' '] <- NA
df_2020_4["easement"][df_2020_4["easement"] == ''] <- NA
df_2020_4["easement"][df_2020_4["easement"] == ' '] <- NA

# Cleaning `bldclasscurr`
df_2016_4["bldclasscurr"][df_2016_4["bldclasscurr"] == ''] <- NA
df_2017_4["bldclasscurr"][df_2017_4["bldclasscurr"] == ''] <- NA
df_2018_4["bldclasscurr"][df_2018_4["bldclasscurr"] == ''] <- NA
df_2019_4["bldclasscurr"][df_2019_4["bldclasscurr"] == ''] <- NA
df_2020_4["bldclasscurr"][df_2020_4["bldclasscurr"] == ''] <- NA

df_2016_4$bldclasscat <- trimws(df_2016_4$bldclasscat)
df_2017_4$bldclasscat <- trimws(df_2017_4$bldclasscat)
df_2018_4$bldclasscat <- trimws(df_2018_4$bldclasscat)
df_2019_4$bldclasscat <- trimws(df_2019_4$bldclasscat)
df_2020_4$bldclasscat <- trimws(df_2020_4$bldclasscat)

# Cleaning `address`
df_2016_4$address <- trimws(df_2016_4$address)
df_2017_4$address <- trimws(df_2017_4$address)
df_2018_4$address <- trimws(df_2018_4$address)
df_2019_4$address <- trimws(df_2019_4$address)
df_2020_4$address <- trimws(df_2020_4$address)

df_2016_4["address"][df_2016_4["address"] == ''] <- NA
df_2017_4["address"][df_2017_4["address"] == ''] <- NA
df_2018_4["address"][df_2018_4["address"] == ''] <- NA
df_2019_4["address"][df_2019_4["address"] == ''] <- NA
df_2020_4["address"][df_2020_4["address"] == ''] <- NA

df_2016_4$address <- gsub('   ', ' ', df_2016_4$address)
df_2017_4$address <- gsub('   ', ' ', df_2017_4$address)
df_2018_4$address <- gsub('   ', ' ', df_2018_4$address)
df_2019_4$address <- gsub('   ', ' ', df_2019_4$address)
df_2020_4$address <- gsub('   ', ' ', df_2020_4$address)

# Cleaning `aptnum`
df_2016_4$aptnum <- trimws(df_2016_4$aptnum)
df_2017_4$aptnum <- trimws(df_2017_4$aptnum)
df_2018_4$aptnum <- trimws(df_2018_4$aptnum)
df_2019_4$aptnum <- trimws(df_2019_4$aptnum)
df_2020_4$aptnum <- trimws(df_2020_4$aptnum)

df_2016_4["aptnum"][df_2016_4["aptnum"] == ''] <- NA
df_2017_4["aptnum"][df_2017_4["aptnum"] == ''] <- NA
df_2018_4["aptnum"][df_2018_4["aptnum"] == ''] <- NA
df_2019_4["aptnum"][df_2019_4["aptnum"] == ''] <- NA
df_2020_4["aptnum"][df_2020_4["aptnum"] == ''] <- NA

df_2016_4$aptnum <- gsub('   ', ' ', df_2016_4$aptnum)
df_2017_4$aptnum <- gsub('   ', ' ', df_2017_4$aptnum)
df_2018_4$aptnum <- gsub('   ', ' ', df_2018_4$aptnum)
df_2019_4$aptnum <- gsub('   ', ' ', df_2019_4$aptnum)
df_2020_4$aptnum <- gsub('   ', ' ', df_2020_4$aptnum)

# Cleaning `zip`
df_2016_4["zip"][df_2016_4["zip"] == ''] <- NA
df_2017_4["zip"][df_2017_4["zip"] == ''] <- NA
df_2018_4["zip"][df_2018_4["zip"] == ''] <- NA
df_2019_4["zip"][df_2019_4["zip"] == ''] <- NA
df_2020_4["zip"][df_2020_4["zip"] == ''] <- NA

df_2016_4["zip"][df_2016_4["zip"] == '0'] <- NA
df_2017_4["zip"][df_2017_4["zip"] == '0'] <- NA
df_2018_4["zip"][df_2018_4["zip"] == '0'] <- NA
df_2019_4["zip"][df_2019_4["zip"] == '0'] <- NA
df_2020_4["zip"][df_2020_4["zip"] == '0'] <- NA

# Cleaning `resunits`
df_2016_4$resunits <- trimws(df_2016_4$resunits)
df_2017_4$resunits <- trimws(df_2017_4$resunits)
df_2018_4$resunits <- trimws(df_2018_4$resunits)
df_2019_4$resunits <- trimws(df_2019_4$resunits)
df_2020_4$resunits <- trimws(df_2020_4$resunits)

df_2016_4["resunits"][df_2016_4["resunits"] == ''] <- NA
df_2017_4["resunits"][df_2017_4["resunits"] == ''] <- NA
df_2018_4["resunits"][df_2018_4["resunits"] == ''] <- NA
df_2019_4["resunits"][df_2019_4["resunits"] == ''] <- NA
df_2020_4["resunits"][df_2020_4["resunits"] == ''] <- NA

df_2016_4["resunits"][df_2016_4["resunits"] == '-'] <- NA
df_2017_4["resunits"][df_2017_4["resunits"] == '-'] <- NA
df_2018_4["resunits"][df_2018_4["resunits"] == '-'] <- NA
df_2019_4["resunits"][df_2019_4["resunits"] == '-'] <- NA
df_2020_4["resunits"][df_2020_4["resunits"] == '-'] <- NA

df_2016_4$resunits <- gsub(',', '', df_2016_4$resunits)
df_2017_4$resunits <- gsub(',', '', df_2017_4$resunits)
df_2018_4$resunits <- gsub(',', '', df_2018_4$resunits)
df_2019_4$resunits <- gsub(',', '', df_2019_4$resunits)
df_2020_4$resunits <- gsub(',', '', df_2020_4$resunits)

# Cleaning `comunits`
df_2016_4$comunits <- trimws(df_2016_4$comunits)
df_2017_4$comunits <- trimws(df_2017_4$comunits)
df_2018_4$comunits <- trimws(df_2018_4$comunits)
df_2019_4$comunits <- trimws(df_2019_4$comunits)
df_2020_4$comunits <- trimws(df_2020_4$comunits)

df_2016_4["comunits"][df_2016_4["comunits"] == ''] <- NA
df_2017_4["comunits"][df_2017_4["comunits"] == ''] <- NA
df_2018_4["comunits"][df_2018_4["comunits"] == ''] <- NA
df_2019_4["comunits"][df_2019_4["comunits"] == ''] <- NA
df_2020_4["comunits"][df_2020_4["comunits"] == ''] <- NA

df_2016_4["comunits"][df_2016_4["comunits"] == '-'] <- NA
df_2017_4["comunits"][df_2017_4["comunits"] == '-'] <- NA
df_2018_4["comunits"][df_2018_4["comunits"] == '-'] <- NA
df_2019_4["comunits"][df_2019_4["comunits"] == '-'] <- NA
df_2020_4["comunits"][df_2020_4["comunits"] == '-'] <- NA

df_2016_4$comunits <- gsub(',', '', df_2016_4$comunits)
df_2017_4$comunits <- gsub(',', '', df_2017_4$comunits)
df_2018_4$comunits <- gsub(',', '', df_2018_4$comunits)
df_2019_4$comunits <- gsub(',', '', df_2019_4$comunits)
df_2020_4$comunits <- gsub(',', '', df_2020_4$comunits)

# Cleaning `totunits`
df_2016_4$totunits <- trimws(df_2016_4$totunits)
df_2017_4$totunits <- trimws(df_2017_4$totunits)
df_2018_4$totunits <- trimws(df_2018_4$totunits)
df_2019_4$totunits <- trimws(df_2019_4$totunits)
df_2020_4$totunits <- trimws(df_2020_4$totunits)

df_2016_4["totunits"][df_2016_4["totunits"] == ''] <- NA
df_2017_4["totunits"][df_2017_4["totunits"] == ''] <- NA
df_2018_4["totunits"][df_2018_4["totunits"] == ''] <- NA
df_2019_4["totunits"][df_2019_4["totunits"] == ''] <- NA
df_2020_4["totunits"][df_2020_4["totunits"] == ''] <- NA

df_2016_4["totunits"][df_2016_4["totunits"] == '-'] <- NA
df_2017_4["totunits"][df_2017_4["totunits"] == '-'] <- NA
df_2018_4["totunits"][df_2018_4["totunits"] == '-'] <- NA
df_2019_4["totunits"][df_2019_4["totunits"] == '-'] <- NA
df_2020_4["totunits"][df_2020_4["totunits"] == '-'] <- NA

df_2016_4$totunits <- gsub(',', '', df_2016_4$totunits)
df_2017_4$totunits <- gsub(',', '', df_2017_4$totunits)
df_2018_4$totunits <- gsub(',', '', df_2018_4$totunits)
df_2019_4$totunits <- gsub(',', '', df_2019_4$totunits)
df_2020_4$totunits <- gsub(',', '', df_2020_4$totunits)

# Cleaning `landsqft`
df_2016_4$landsqft <- trimws(df_2016_4$landsqft)
df_2017_4$landsqft <- trimws(df_2017_4$landsqft)
df_2018_4$landsqft <- trimws(df_2018_4$landsqft)
df_2019_4$landsqft <- trimws(df_2019_4$landsqft)
df_2020_4$landsqft <- trimws(df_2020_4$landsqft)

df_2016_4["landsqft"][df_2016_4["landsqft"] == ''] <- NA
df_2017_4["landsqft"][df_2017_4["landsqft"] == ''] <- NA
df_2018_4["landsqft"][df_2018_4["landsqft"] == ''] <- NA
df_2019_4["landsqft"][df_2019_4["landsqft"] == ''] <- NA
df_2020_4["landsqft"][df_2020_4["landsqft"] == ''] <- NA

df_2016_4["landsqft"][df_2016_4["landsqft"] == '-'] <- NA
df_2017_4["landsqft"][df_2017_4["landsqft"] == '-'] <- NA
df_2018_4["landsqft"][df_2018_4["landsqft"] == '-'] <- NA
df_2019_4["landsqft"][df_2019_4["landsqft"] == '-'] <- NA
df_2020_4["landsqft"][df_2020_4["landsqft"] == '-'] <- NA

df_2016_4["landsqft"][df_2016_4["landsqft"] == '0'] <- NA
df_2017_4["landsqft"][df_2017_4["landsqft"] == '0'] <- NA
df_2018_4["landsqft"][df_2018_4["landsqft"] == '0'] <- NA
df_2019_4["landsqft"][df_2019_4["landsqft"] == '0'] <- NA
df_2020_4["landsqft"][df_2020_4["landsqft"] == '0'] <- NA

df_2016_4["landsqft"][df_2016_4["landsqft"] == '1'] <- NA
df_2017_4["landsqft"][df_2017_4["landsqft"] == '1'] <- NA
df_2018_4["landsqft"][df_2018_4["landsqft"] == '1'] <- NA
df_2019_4["landsqft"][df_2019_4["landsqft"] == '1'] <- NA
df_2020_4["landsqft"][df_2020_4["landsqft"] == '1'] <- NA

df_2016_4$landsqft <- gsub(',', '', df_2016_4$landsqft)
df_2017_4$landsqft <- gsub(',', '', df_2017_4$landsqft)
df_2018_4$landsqft <- gsub(',', '', df_2018_4$landsqft)
df_2019_4$landsqft <- gsub(',', '', df_2019_4$landsqft)
df_2020_4$landsqft <- gsub(',', '', df_2020_4$landsqft)

# Cleaning `grosssqft`
df_2016_4$grosssqft <- trimws(df_2016_4$grosssqft)
df_2017_4$grosssqft <- trimws(df_2017_4$grosssqft)
df_2018_4$grosssqft <- trimws(df_2018_4$grosssqft)
df_2019_4$grosssqft <- trimws(df_2019_4$grosssqft)
df_2020_4$grosssqft <- trimws(df_2020_4$grosssqft)

df_2016_4["grosssqft"][df_2016_4["grosssqft"] == ''] <- NA
df_2017_4["grosssqft"][df_2017_4["grosssqft"] == ''] <- NA
df_2018_4["grosssqft"][df_2018_4["grosssqft"] == ''] <- NA
df_2019_4["grosssqft"][df_2019_4["grosssqft"] == ''] <- NA
df_2020_4["grosssqft"][df_2020_4["grosssqft"] == ''] <- NA

df_2016_4["grosssqft"][df_2016_4["grosssqft"] == '-'] <- NA
df_2017_4["grosssqft"][df_2017_4["grosssqft"] == '-'] <- NA
df_2018_4["grosssqft"][df_2018_4["grosssqft"] == '-'] <- NA
df_2019_4["grosssqft"][df_2019_4["grosssqft"] == '-'] <- NA
df_2020_4["grosssqft"][df_2020_4["grosssqft"] == '-'] <- NA

df_2016_4["grosssqft"][df_2016_4["grosssqft"] == '0'] <- NA
df_2017_4["grosssqft"][df_2017_4["grosssqft"] == '0'] <- NA
df_2018_4["grosssqft"][df_2018_4["grosssqft"] == '0'] <- NA
df_2019_4["grosssqft"][df_2019_4["grosssqft"] == '0'] <- NA
df_2020_4["grosssqft"][df_2020_4["grosssqft"] == '0'] <- NA

df_2016_4["grosssqft"][df_2016_4["grosssqft"] == '1'] <- NA
df_2017_4["grosssqft"][df_2017_4["grosssqft"] == '1'] <- NA
df_2018_4["grosssqft"][df_2018_4["grosssqft"] == '1'] <- NA
df_2019_4["grosssqft"][df_2019_4["grosssqft"] == '1'] <- NA
df_2020_4["grosssqft"][df_2020_4["grosssqft"] == '1'] <- NA

df_2016_4$grosssqft <- gsub(',', '', df_2016_4$grosssqft)
df_2017_4$grosssqft <- gsub(',', '', df_2017_4$grosssqft)
df_2018_4$grosssqft <- gsub(',', '', df_2018_4$grosssqft)
df_2019_4$grosssqft <- gsub(',', '', df_2019_4$grosssqft)
df_2020_4$grosssqft <- gsub(',', '', df_2020_4$grosssqft)

# Cleaning `yrbuilt`
df_2016_4["yrbuilt"][df_2016_4["yrbuilt"] == ''] <- NA
df_2017_4["yrbuilt"][df_2017_4["yrbuilt"] == ''] <- NA
df_2018_4["yrbuilt"][df_2018_4["yrbuilt"] == ''] <- NA
df_2019_4["yrbuilt"][df_2019_4["yrbuilt"] == ''] <- NA
df_2020_4["yrbuilt"][df_2020_4["yrbuilt"] == ''] <- NA

df_2016_4["yrbuilt"][df_2016_4["yrbuilt"] == '0'] <- NA
df_2017_4["yrbuilt"][df_2017_4["yrbuilt"] == '0'] <- NA
df_2018_4["yrbuilt"][df_2018_4["yrbuilt"] == '0'] <- NA
df_2019_4["yrbuilt"][df_2019_4["yrbuilt"] == '0'] <- NA
df_2020_4["yrbuilt"][df_2020_4["yrbuilt"] == '0'] <- NA

# Cleaning `taxclasssale`
df_2016_4$taxclasssale <- trimws(df_2016_4$taxclasssale)
df_2017_4$taxclasssale <- trimws(df_2017_4$taxclasssale)
df_2018_4$taxclasssale <- trimws(df_2018_4$taxclasssale)
df_2019_4$taxclasssale <- trimws(df_2019_4$taxclasssale)
df_2020_4$taxclasssale <- trimws(df_2020_4$taxclasssale)

df_2016_4["taxclasssale"][df_2016_4["taxclasssale"] == ''] <- NA
df_2017_4["taxclasssale"][df_2017_4["taxclasssale"] == ''] <- NA
df_2018_4["taxclasssale"][df_2018_4["taxclasssale"] == ''] <- NA
df_2019_4["taxclasssale"][df_2019_4["taxclasssale"] == ''] <- NA
df_2020_4["taxclasssale"][df_2020_4["taxclasssale"] == ''] <- NA

# Cleaning `bldclasssale`
df_2016_4$bldclasssale <- trimws(df_2016_4$bldclasssale)
df_2017_4$bldclasssale <- trimws(df_2017_4$bldclasssale)
df_2018_4$bldclasssale <- trimws(df_2018_4$bldclasssale)
df_2019_4$bldclasssale <- trimws(df_2019_4$bldclasssale)
df_2020_4$bldclasssale <- trimws(df_2020_4$bldclasssale)

df_2016_4["bldclasssale"][df_2016_4["bldclasssale"] == ''] <- NA
df_2017_4["bldclasssale"][df_2017_4["bldclasssale"] == ''] <- NA
df_2018_4["bldclasssale"][df_2018_4["bldclasssale"] == ''] <- NA
df_2019_4["bldclasssale"][df_2019_4["bldclasssale"] == ''] <- NA
df_2020_4["bldclasssale"][df_2020_4["bldclasssale"] == ''] <- NA

# Cleaning `price`
df_2016_4$price <- trimws(df_2016_4$price)
df_2017_4$price <- trimws(df_2017_4$price)
df_2018_4$price <- trimws(df_2018_4$price)
df_2019_4$price <- trimws(df_2019_4$price)
df_2020_4$price <- trimws(df_2020_4$price)

df_2016_4["price"][df_2016_4["price"] == ''] <- NA
df_2017_4["price"][df_2017_4["price"] == ''] <- NA
df_2018_4["price"][df_2018_4["price"] == ''] <- NA
df_2019_4["price"][df_2019_4["price"] == ''] <- NA
df_2020_4["price"][df_2020_4["price"] == ''] <- NA

df_2016_4["price"][df_2016_4["price"] == '-'] <- NA
df_2017_4["price"][df_2017_4["price"] == '-'] <- NA
df_2018_4["price"][df_2018_4["price"] == '-'] <- NA
df_2019_4["price"][df_2019_4["price"] == '-'] <- NA
df_2020_4["price"][df_2020_4["price"] == '-'] <- NA

# df_2016_4["price"][df_2016_4["price"] == '0'] <- NA
# df_2017_4["price"][df_2017_4["price"] == '0'] <- NA
# df_2018_4["price"][df_2018_4["price"] == '0'] <- NA
# df_2019_4["price"][df_2019_4["price"] == '0'] <- NA
# df_2020_4["price"][df_2020_4["price"] == '0'] <- NA

# df_2016_4["price"][df_2016_4["price"] == '1'] <- NA
# df_2017_4["price"][df_2017_4["price"] == '1'] <- NA
# df_2018_4["price"][df_2018_4["price"] == '1'] <- NA
# df_2019_4["price"][df_2019_4["price"] == '1'] <- NA
# df_2020_4["price"][df_2020_4["price"] == '1'] <- NA

df_2016_4$price <- gsub('\\$', '', df_2016_4$price)
df_2017_4$price <- gsub('\\$', '', df_2017_4$price)
df_2018_4$price <- gsub('\\$', '', df_2018_4$price)
df_2019_4$price <- gsub('\\$', '', df_2019_4$price)
df_2020_4$price <- gsub('\\$', '', df_2020_4$price)

df_2016_4$price <- gsub(',', '', df_2016_4$price)
df_2017_4$price <- gsub(',', '', df_2017_4$price)
df_2018_4$price <- gsub(',', '', df_2018_4$price)
df_2019_4$price <- gsub(',', '', df_2019_4$price)
df_2020_4$price <- gsub(',', '', df_2020_4$price)

# Cleaning `date`
df_2016_4["date"][df_2016_4["date"] == ''] <- NA
df_2017_4["date"][df_2017_4["date"] == ''] <- NA
df_2018_4["date"][df_2018_4["date"] == ''] <- NA
df_2019_4["date"][df_2019_4["date"] == ''] <- NA
df_2020_4["date"][df_2020_4["date"] == ''] <- NA

# Converting `date` column in the date format
df_2016_4$date_2 <- as.Date(df_2016_4$date, format = "%m/%d/%Y")
df_2017_4$date_2 <- as.Date(df_2017_4$date, format = "%m/%d/%y")
df_2018_4$date_2 <- as.Date(df_2018_4$date, format = "%m/%d/%y")
df_2019_4$date_2 <- as.Date(df_2019_4$date, format = "%m/%d/%y")
df_2020_4$date_2 <- as.Date(df_2020_4$date, format = "%m/%d/%y")

### Joining Datasets ###

df <- data.frame(rbind(df_2016_4, df_2017_4, df_2018_4, df_2019_4, df_2020_4))

dim(df)

# Changing data types
df$block <- as.numeric(df$block)
df$lot <- as.numeric(df$lot)
df$resunits <- as.integer((df$resunits))
df$comunits <- as.integer((df$comunits))
df$totunits <- as.integer((df$totunits))
df$landsqft <- as.numeric(df$landsqft)
df$grosssqft <- as.numeric(df$grosssqft)
df$yrbuilt <- as.integer(df$yrbuilt)
df$price <- as.numeric(df$price)

### Filtering Data ###

df2 <- df[grepl("^A|^R", df$bldclasssale), ]

dim(df2)

df3 <- df2 %>% filter(totunits == 1 & resunits == 1 & grosssqft > 0 & price > 1)

dim(df3)

### Data Cleaning ###

df3["bldclasscat"][df3["bldclasscat"] == '01  ONE FAMILY DWELLINGS'] <- '01 ONE FAMILY DWELLINGS'
df3$address <- sub(",.*", "", df3$address)
df3["aptnum"][df3["aptnum"] == '5-Jan'] <- NA
df3 <- df3[-(which(df3$price %in% c(1e+05,1e+06,1e+07,2e+05,2e+06,3e+05,3e+06,4e+05,4e+06,5e+05,5e+06,6e+05,6e+06,7e+05,7e+06,8e+05,8e+06,9e+05,9e+06))),]
df4 <- subset(df3, select = -c(resunits, comunits, totunits, borough, easement))
df4 <- df4 %>% mutate(id = row_number())

dim(df4)

### Feature Engineering ###

df4$taxclasscurr_2 <- ifelse(df4$taxclasscurr %in% c('1', '1A', '1C'), '1', df4$taxclasscurr)
df4$taxclasscurr_2 <- ifelse(df4$taxclasscurr %in% c('2', '2C'), '2', df4$taxclasscurr_2)

df4$bldclasscurr_2 <- ifelse(df4$bldclasscurr %in% c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A9', 'A0'), 'A', df4$bldclasscurr)
df4$bldclasscurr_2 <- ifelse(df4$bldclasscurr %in% c('R1', 'R2', 'R3', 'R4', 'R6', 'RR'), 'R', df4$bldclasscurr_2)

df4$bldclasssale_2 <- ifelse(df4$bldclasssale %in% c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A9', 'A0'), 'A', df4$bldclasssale)
df4$bldclasssale_2 <- ifelse(df4$bldclasssale %in% c('R1', 'R2', 'R3', 'R4', 'R6', 'RR'), 'R', df4$bldclasssale_2)

df4$bldclasscat_2 <- ifelse(df4$bldclasscat %in% c('01 ONE FAMILY DWELLINGS'), 'Family Dwellings', df4$bldclasscat)
df4$bldclasscat_2 <- ifelse(df4$bldclasscat %in% c('04 TAX CLASS 1 CONDOS', '11 SPECIAL CONDO BILLING LOTS', '12 CONDOS - WALKUP APARTMENTS', '13 CONDOS - ELEVATOR APARTMENTS', '15 CONDOS - 2-10 UNIT RESIDENTIAL'), 'Condos', df4$bldclasscat_2)

df4$todays_date <- "2023-12-01"
df4$todays_date <- as.Date(df4$todays_date)

df4$years_since_sale <- (as.Date(df4$todays_date, format = "%Y/%m/%d") - as.Date(df4$date_2, format = "%Y/%m/%d"))/365
df4$years_since_sale <- as.numeric(df4$years_since_sale)

df4$sale_quarter <- quarter(df4$date_2)
df4$sale_month <- month(df4$date_2)

df4$building_current_age <- 2023 - df4$yrbuilt
df4$building_age_sale <- df4$sale_year - df4$yrbuilt

df4$sqft_diff <- df4$grosssqft - df4$landsqft

df4$block_2 <- NA
df4$block_2 <- ifelse(df4$block > 0 & df4$block < 1000, 1, df4$block_2)
df4$block_2 <- ifelse(df4$block > 1000 & df4$block < 2000, 2, df4$block_2)
df4$block_2 <- ifelse(df4$block > 2000 & df4$block < 3000, 3, df4$block_2)
df4$block_2 <- ifelse(df4$block > 3000 & df4$block < 4000, 4, df4$block_2)
df4$block_2 <- ifelse(df4$block > 4000 & df4$block < 5000, 5, df4$block_2)
df4$block_2 <- ifelse(df4$block > 5000 & df4$block < 6000, 6, df4$block_2)
df4$block_2 <- ifelse(df4$block > 6000 & df4$block < 7000, 7, df4$block_2)
df4$block_2 <- ifelse(df4$block > 7000 & df4$block < 8000, 8, df4$block_2)
df4$block_2 <- ifelse(df4$block > 8000 & df4$block < 9000, 9, df4$block_2)

df4$block_3 <- NA
df4$block_3 <- ifelse(df4$block > 0 & df4$block < 500, 1, df4$block_3)
df4$block_3 <- ifelse(df4$block > 500 & df4$block < 1000, 2, df4$block_3)
df4$block_3 <- ifelse(df4$block > 1000 & df4$block < 1500, 3, df4$block_3)
df4$block_3 <- ifelse(df4$block > 1500 & df4$block < 2000, 4, df4$block_3)
df4$block_3 <- ifelse(df4$block > 2000 & df4$block < 2500, 5, df4$block_3)
df4$block_3 <- ifelse(df4$block > 2500 & df4$block < 3000, 6, df4$block_3)
df4$block_3 <- ifelse(df4$block > 3000 & df4$block < 3500, 7, df4$block_3)
df4$block_3 <- ifelse(df4$block > 3500 & df4$block < 4000, 8, df4$block_3)
df4$block_3 <- ifelse(df4$block > 4000 & df4$block < 4500, 9, df4$block_3)
df4$block_3 <- ifelse(df4$block > 4500 & df4$block < 5000, 10, df4$block_3)
df4$block_3 <- ifelse(df4$block > 5000 & df4$block < 5500, 11, df4$block_3)
df4$block_3 <- ifelse(df4$block > 5500 & df4$block < 6000, 12, df4$block_3)
df4$block_3 <- ifelse(df4$block > 6000 & df4$block < 6500, 13, df4$block_3)
df4$block_3 <- ifelse(df4$block > 6500 & df4$block < 7000, 14, df4$block_3)
df4$block_3 <- ifelse(df4$block > 7000 & df4$block < 7500, 15, df4$block_3)
df4$block_3 <- ifelse(df4$block > 7500 & df4$block < 8000, 16, df4$block_3)
df4$block_3 <- ifelse(df4$block > 8000 & df4$block < 8500, 17, df4$block_3)
df4$block_3 <- ifelse(df4$block > 8500 & df4$block < 9000, 18, df4$block_3)

df4$block_2 <- as.factor(df4$block_2)
df4$block_3 <- as.factor(df4$block_3)

df4$lot_2 <- NA
df4$lot_2 <- ifelse(df4$lot > 0 & df4$lot < 500, 1, df4$lot_2)
df4$lot_2 <- ifelse(df4$lot > 500 & df4$lot < 1000, 2, df4$lot_2)
df4$lot_2 <- ifelse(df4$lot > 1000 & df4$lot < 1500, 3, df4$lot_2)
df4$lot_2 <- ifelse(df4$lot > 1500 & df4$lot < 2000, 4, df4$lot_2)
df4$lot_2 <- ifelse(df4$lot > 2000 & df4$lot < 2500, 5, df4$lot_2)
df4$lot_2 <- ifelse(df4$lot > 2500 & df4$lot < 3000, 6, df4$lot_2)
df4$lot_2 <- ifelse(df4$lot > 3000 & df4$lot < 3500, 7, df4$lot_2)

df4$neighborhood_region <- NA
df4$neighborhood_region <- ifelse(df4$neighborhood %in% c('CROWN HEIGHTS','FLATBUSH-CENTRAL','FLATBUSH-EAST','FLATBUSH-LEFFERTS GARDEN','FLATBUSH-NORTH','KENSINGTON','OCEAN PARKWAY-NORTH','OCEAN PARKWAY-SOUTH','WINDSOR TERRACE'), 'Central Brooklyn', df4$neighborhood_region)
df4$neighborhood_region <- ifelse(df4$neighborhood %in% c('BROWNSVILLE','CANARSIE','CYPRESS HILLS','EAST NEW YORK','SPRING CREEK'), 'Eastern Brooklyn', df4$neighborhood_region)
df4$neighborhood_region <- ifelse(df4$neighborhood %in% c('BEDFORD STUYVESANT','BUSHWICK','GREENPOINT','OCEAN HILL','WILLIAMSBURG-CENTRAL','WILLIAMSBURG-EAST','WILLIAMSBURG-NORTH','WILLIAMSBURG-SOUTH','WYCKOFF HEIGHTS'), 'Northern Brooklyn', df4$neighborhood_region)
df4$neighborhood_region <- ifelse(df4$neighborhood %in% c('BOERUM HILL','BROOKLYN HEIGHTS','CARROLL GARDENS','CLINTON HILL','COBBLE HILL','COBBLE HILL-WEST','DOWNTOWN-FULTON FERRY','DOWNTOWN-FULTON MALL','DOWNTOWN-METROTECH','FORT GREENE','GOWANUS','NAVY YARD','PARK SLOPE','PARK SLOPE SOUTH','PROSPECT HEIGHTS','RED HOOK'), 'Northwestern Brooklyn', df4$neighborhood_region)
df4$neighborhood_region <- ifelse(df4$neighborhood %in% c('BERGEN BEACH','BRIGHTON BEACH','CONEY ISLAND','FLATLANDS','GERRITSEN BEACH','GRAVESEND','MADISON','MANHATTAN BEACH','MARINE PARK','MIDWOOD','MILL BASIN','OLD MILL BASIN','SEAGATE','SHEEPSHEAD BAY'), 'Southern Brooklyn', df4$neighborhood_region)
df4$neighborhood_region <- ifelse(df4$neighborhood %in% c('BATH BEACH','BAY RIDGE','BENSONHURST','BOROUGH PARK','BUSH TERMINAL','DYKER HEIGHTS','SUNSET PARK'), 'Southwestern Brooklyn', df4$neighborhood_region)

df4$neighborhood_2 <- df4$neighborhood
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('FLATBUSH-CENTRAL', 'FLATBUSH-EAST', 'FLATBUSH-LEFFERTS GARDEN', 'FLATBUSH-NORTH'), 'FLATBUSH', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('OCEAN PARKWAY-NORTH', 'OCEAN PARKWAY-SOUTH'), 'OCEAN PARKWAY', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('WILLIAMSBURG-CENTRAL', 'WILLIAMSBURG-EAST', 'WILLIAMSBURG-NORTH', 'WILLIAMSBURG-SOUTH'), 'WILLIAMSBURG', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('COBBLE HILL', 'COBBLE HILL-WEST'), 'COBBLE HILL', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('DOWNTOWN-FULTON FERRY', 'DOWNTOWN-FULTON MALL', 'DOWNTOWN-METROTECH', 'BOERUM HILL', 'BROOKLYN HEIGHTS', 'NAVY YARD'), 'DOWNTOWN', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('PARK SLOPE', 'PARK SLOPE SOUTH'), 'PARK SLOPE', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('MILL BASIN', 'OLD MILL BASIN'), 'MILL BASIN', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('EAST NEW YORK', 'SPRING CREEK'), 'EAST NEW YORK', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('BROWNSVILLE', 'CANARSIE', 'CYPRESS HILLS'), 'EASTERN BROOKLYN', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('BATH BEACH', 'BAY RIDGE', 'BENSONHURST', 'DYKER HEIGHTS'), 'BAY RIDGE', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('BEDFORD STUYVESANT', 'BUSHWICK', 'OCEAN HILL'), 'BEDFORD STUYVESANT', df4$neighborhood_2)
df4$neighborhood_2 <- ifelse(df4$neighborhood %in% c('BOROUGH PARK', 'BUSH TERMINAL', 'SUNSET PARK'), 'SOUTHWESTERN BROOKLYN', df4$neighborhood_2)

df4$neighborhood_3 <- df4$neighborhood
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('FLATBUSH-CENTRAL', 'FLATBUSH-EAST', 'FLATBUSH-LEFFERTS GARDEN', 'FLATBUSH-NORTH', 'KENSINGTON', 'WINDSOR TERRACE'), 'FLATBUSH', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('OCEAN PARKWAY-NORTH', 'OCEAN PARKWAY-SOUTH'), 'OCEAN PARKWAY', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('WILLIAMSBURG-CENTRAL', 'WILLIAMSBURG-EAST', 'WILLIAMSBURG-NORTH', 'WILLIAMSBURG-SOUTH'), 'WILLIAMSBURG', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('COBBLE HILL', 'COBBLE HILL-WEST', 'CARROLL GARDENS'), 'COBBLE HILL', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('DOWNTOWN-FULTON FERRY', 'DOWNTOWN-FULTON MALL', 'DOWNTOWN-METROTECH', 'BOERUM HILL', 'BROOKLYN HEIGHTS', 'NAVY YARD'), 'DOWNTOWN', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('PARK SLOPE', 'PARK SLOPE SOUTH'), 'PARK SLOPE', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('MILL BASIN', 'OLD MILL BASIN'), 'MILL BASIN', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('EAST NEW YORK', 'SPRING CREEK'), 'EAST NEW YORK', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('BROWNSVILLE', 'CANARSIE', 'CYPRESS HILLS'), 'EASTERN BROOKLYN', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('BATH BEACH', 'BAY RIDGE', 'BENSONHURST', 'DYKER HEIGHTS'), 'BAY RIDGE', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('BEDFORD STUYVESANT', 'BUSHWICK', 'OCEAN HILL'), 'BEDFORD STUYVESANT', df4$neighborhood_3)
df4$neighborhood_3 <- ifelse(df4$neighborhood %in% c('BOROUGH PARK', 'BUSH TERMINAL', 'SUNSET PARK'), 'SOUTHWESTERN BROOKLYN', df4$neighborhood_3)


df4$zip_groups <- NA
df4$zip_groups <- ifelse(df4$zip %in% c(11201, 11205, 11215, 11217, 11231, 11232, 11249), 'Downtown and Northwestern Brooklyn', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11211, 11222), 'Northern Brooklyn', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11237, 11206, 11221), 'Northeastern Brooklyn', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11212, 11213, 11216, 11233, 11238), 'Central Brooklyn', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11209, 11219, 11220, 11228), 'Western Brooklyn', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11214, 11223, 11224, 11229, 11235), 'Southwestern Brooklyn', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11234, 11236), 'Southeastern Brooklyn', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11207, 11208, 11239), 'Eastern Brooklyn', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11210, 11230, 11226, 11204), 'Midwood and Surrounding Areas', df4$zip_groups)
df4$zip_groups <- ifelse(df4$zip %in% c(11203, 11218, 11225, 11226), 'Flatbush and Surrounding Areas', df4$zip_groups)

# Variable transformations
df4$grossqft_log <- log(df4$grosssqft)

df4$landsqft_log <- log(df4$landsqft)

df4$sqft_ratio <- df4$grosssqft/df4$landsqft

# Treating Outlier Based On IQR Analysis
df5 <- df4[df4$price < 5200000, ]

dim(df5)

### Visualizing Data Distributions ###

ggplot(df5, aes(x = price)) + geom_histogram()

hist(df5$price, probability = TRUE)
lines(density(df5$price), col = 'blue', lwd = 3)

ggplot(df5, aes(x = grosssqft)) + geom_histogram()

ggplot(df5, aes(x = log(grosssqft))) + geom_histogram()

ggplot(df5, aes(x = landsqft)) + geom_histogram()

ggplot(df5, aes(x = log(landsqft))) + geom_histogram()

ggplot(df5, aes(x = sqft_ratio)) + geom_histogram()

ggplot(df5, aes(x = log(sqft_ratio))) + geom_histogram()

ggplot(df5, aes(x = sqrt(sqft_ratio))) + geom_histogram()

ggplot(df5, aes(x = grosssqft, y = price)) + geom_point()

ggplot(df5, aes(x = log(grosssqft), y = price)) + geom_point()

ggplot(df5, aes(x = landsqft, y = price)) + geom_point()

ggplot(df5, aes(x = log(landsqft), y = price)) + geom_point()

### Saving Cleaned Dataset ###

write.csv(df5, "/Users/kshitizsahay/Documents/University of Chicago/ADSP 31007 Statistical Analysis/Final Project/Data/preprocessed_data.csv", row.names = FALSE)

### Test Predictors ###

# model <- lm(price ~ grosssqft, data = df5)
# summary(model)
# 
# model <- lm(price ~ grossqft_log, data = df5)
# summary(model)
# 
# model <- lm(price ~ sqrt(grosssqft), data = df5)
# summary(model)

### Model Development ###

# model <- lm(price ~ grosssqft * landsqft + bldclasscurr + building_age_sale + block_3, data = df4)
# summary(model) # 0.4439
#
# model <- lm(price ~ grosssqft * landsqft + bldclasscurr + building_current_age * building_age_sale + block_3, data = df4)
# summary(model) # 0.4433
#
# model <- lm(price ~ grosssqft + bldclasscurr + building_age_sale + building_current_age +  neighborhood, data = df4)
# summary(model) # 0.4581
# 
# model <- lm(price ~ block_3 * (grosssqft^2) + block_3 * bldclasscurr + yrbuilt, data = df4)
# summary(model) # 0.4581
# 
# model <- lm(price ~ grosssqft * landsqft + bldclasscurr + building_current_age * building_age_sale + block_3 * lot, data = df4)
# summary(model) # 0.5819

model_0 <- lm(price ~ grossqft_log * block + sqrt(sqft_ratio) + sale_year + sale_quarter + neighborhood_2, data = df5)
summary(model_0) # 0.6061

RMSE <- sqrt(mean(model_0$residuals^2))
print(RMSE)

### Final Model ###

final_model <- lm(price ~ sqrt(sqft_ratio) + sale_year + sale_quarter + neighborhood_3 + grossqft_log:neighborhood_region, data = df5)
summary(final_model) # 0.6185 # DF = 40

RMSE <- sqrt(mean(final_model$residuals^2))
print(RMSE) # 435077.1

saveRDS(list(model = final_model, data = df5), file = '/Users/kshitizsahay/Documents/University of Chicago/ADSP 31007 Statistical Analysis/Final Project/Model/kshitizsahay.RDS')

### Model Diagnostics ###

plot(final_model)

library(lmtest)
dwtest(formula = final_model,  alternative = "two.sided")

bptest(final_model)

library(car)
vif(final_model)


################### Part 2 ########################

final_predictors <- c('grossqft_log', 'sqft_ratio', 'neighborhood_region', 'sale_year', 'sale_quarter', 'neighborhood_3')
predictions <- predict(final_model, newdata = df5[final_predictors])

df5$predicted_price <- predictions
df6 <- df5[!is.na(df5$predicted_price),]

rmse(as.numeric(df6$price), as.numeric(df6$predicted_price))

### Visualizing the difference between the two groups ###
df6 %>% group_by(sale_year, sale_quarter) %>% summarize(mean_price = mean(price),
                                                        .groups = 'drop')

df6 %>% group_by(sale_year, sale_quarter) %>% summarize(mean_price = mean(predicted_price),
                                                        .groups = 'drop')


df6 %>% group_by(sale_year, sale_quarter) %>% summarize(median_price = median(price),
                                                        .groups = 'drop')

df6 %>% group_by(sale_year, sale_quarter) %>% summarize(median_price = median(predicted_price),
                                                        .groups = 'drop')

### Sub-setting Q3/Q4 2020 to create new data ###
q3_2020 <- df6[df6$sale_year == 2020 & df6$sale_quarter == 3,]
q4_2020 <- df6[df6$sale_year == 2020 & df6$sale_quarter == 4,]

### Comparing the mean/median of actual and predicted price ###
mean(q3_2020$price)
mean(q4_2020$price)

median(q3_2020$price)
median(q4_2020$price)

mean(q3_2020$predicted_price)
mean(q4_2020$predicted_price)

median(q3_2020$predicted_price)
median(q4_2020$predicted_price)

### T Test ###

t.test(q3_2020$price, q4_2020$price, alternative = c("two.sided", "less", "greater"))

t.test(price ~ sale_quarter, data = rbind(q3_2020, q4_2020))

t.test(log(price) ~ as.factor(sale_quarter), data = rbind(q3_2020, q4_2020))

# t.test(q3_2020$predicted_price, q4_2020$predicted_price, alternative = c("two.sided", "less", "greater"))
# t.test(predicted_price ~ sale_quarter, data = rbind(q3_2020, q4_2020))

### Wilcox Test ###
wilcox.test(price ~ as.factor(sale_quarter), data = rbind(q3_2020, q4_2020))

# wilcox.test(predicted_price ~ as.factor(sale_quarter), data = rbind(q3_2020, q4_2020))

### Fitting a regression model to compare the difference ###
new_data <- rbind(q3_2020, q4_2020)
new_data$sale_quarter <- as.factor(new_data$sale_quarter)
new_data$sale_quarter <- relevel(new_data$sale_quarter, ref = "3")

p2_model1 <- lm(price ~ sale_quarter + grosssqft + zip, data = new_data)
summary(p2_model1)

# p2_model1 <- lm(price ~ sale_quarter, data = new_data)
# summary(p2_model1)

### Data Visualizations ###

ggplot(new_data, aes(predicted_price)) +
  geom_boxplot() + facet_wrap(~sale_quarter) + xlab('Predicted Home Purchase Price') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplot(new_data, aes(predicted_price)) + geom_histogram(fill = "white", color = "grey30") + facet_wrap(~sale_quarter)

# Q <- quantile(df4$price, probs = c(.25, .75), na.rm = FALSE)
# iqr <- IQR(df5$price)
# up <-  Q[2] + 1.5 * iqr
# low <- Q[1] - 1.5 * iqr
