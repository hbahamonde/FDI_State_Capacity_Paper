cat("\014")
rm(list=ls())


############################
#### Loadings
############################


## ---- loadings:data ----
# Load the census data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Census data 
census.d <- read.dta("../FDI_State_Capacity_Paper/Data/Census_data_until_2015.dta") 

## replaces NAs with 0s
census.d$census_full[is.na(census.d$census_full)] <- 0

## generate cumulative census variable by country
census.d = census.d[with(census.d, order(country, year)),]
census.d$cum.census <- as.numeric(ave(census.d$census_full, census.d$country, FUN=cumsum))

## keep just important variables
census.d <- census.d[, c("country", "year", "census_full", "cum.census")]





if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table) 
census.d = merge(data.frame(setDT(census.d[ which(census.d$cum.census > 0),])[, .(country_num_of_census = uniqueN(cum.census)), by = country]),
                 census.d,
                 by=c("country"),
                 all.x=T
)
### generate variable with number of years by country
census.d = merge(
        data.frame(setDT(census.d)[, .(country_tot_years = uniqueN(year)), by = country]),
        census.d,
        by=c("country"),
        all.x=T
)

# average census per year
census.d$ave_census = census.d$country_num_of_census/census.d$country_tot_years


# Load Inequality data
inequality.d <- read.dta("../FDI_State_Capacity_Paper/Data/SWIID/dataverse_files/swiid6_0/swiid6_0.dta") 

### creating DISP
inequality.disp.d = as.data.frame(inequality.d[ , grepl("gini_disp", names(inequality.d))]) # generating another DF with DISP only
inequality.d$gini_disp.mean = rowMeans(inequality.disp.d[,-1]) # attaching "gini_disp.mean" with mean of 100 columns
### creating MKT
inequality.mkt.d = as.data.frame(inequality.d[ , grepl("gini_mkt", names(inequality.d))]) # generating another DF with mkt only
inequality.d$gini_mkt.mean = rowMeans(inequality.mkt.d[,-1]) # attaching "gini_mkt.mean" with mean of 100 columns
### creating abs_red
inequality.abs_red.d = as.data.frame(inequality.d[ , grepl("_abs_red", names(inequality.d))]) # generating another DF with abs_red only
inequality.d$gini_abs_red.mean = rowMeans(inequality.abs_red.d[,-1]) # attaching "gini_abs_red.mean" with mean of 100 columns
### creating rel_red
inequality.rel_red.d = as.data.frame(inequality.d[ , grepl("rel_red", names(inequality.d))]) # generating another DF with rel_red only
inequality.d$gini_rel_red.mean = rowMeans(inequality.rel_red.d[,-1]) # attaching "gini_rel_red.mean" with mean of 100 columns
### keeping just these variables (and country, and year) and dropping everything else
inequality.d <- inequality.d[, c("country", "year", "gini_disp.mean", "gini_mkt.mean", "gini_abs_red.mean", "gini_rel_red.mean")]
## change some country names to be consistent with the other datasets
inequality.d$country[inequality.d$country == "Saint Kitts and Nevis"] <- "St Kitts and Nevis"
inequality.d$country[inequality.d$country == "Saint Vincent and the Grenadines"] <- "St. Vincent"
inequality.d$country[inequality.d$country == "Saint Lucia"] <- "St. Lucia"


# Load Boix's democracy variable

democracy.d <- read.dta("../FDI_State_Capacity_Paper/Data/Boix_Dem_Variable/democracy.dta") 

##change the the type of variable from numeric to character
democracy.d$country = as.character(democracy.d$country)
## change some country names to be consistent with the other datasets
democracy.d$country[democracy.d$country == "UNITED STATES OF AMERICA"] <- "UNITED STATES"
democracy.d$country[democracy.d$country == "TRINIDAD&TOBAGO"] <- "Trinidad and Tobago"
democracy.d$country[democracy.d$country == "CAPE VERDE IS."] <- "Cape Verde"
democracy.d$country[democracy.d$country == "CONGO, DEM REP (ex Zaire)"] <- "CONGO"
democracy.d$country[democracy.d$country == "CENTRAL AFR.R."] <- "Central African Republic"
democracy.d$country[democracy.d$country == "BURKINA FASO"] <- "Burkina Faso"
democracy.d$country[democracy.d$country == "ETHIOPIA  (INCL. ERIT)"] <- "Ethiopia"
democracy.d$country[democracy.d$country == "ST.KITTS&NEVIS"] <- "St Kitts and Nevis"
democracy.d$country[democracy.d$country == "ST.VINCENT&GRE"] <- "St. Vincent"
democracy.d$country[democracy.d$country == "SOLOMON IS."] <- "Solomon Islands"
democracy.d$country[democracy.d$country == "YEMEN PEOPLE REPUBLIC"] <- "Yemen" # In 1990 it changes from "Yemen People Republic" to just "Yemen." This is the one that we will use.
# democracy.d$country[democracy.d$country == "YEMEN ARAB REPUBLIC"] <- "Yemen" # This is before the unification.
democracy.d$country[democracy.d$country == "GUINEA-BISS"] <- "Guinea-Bissau"
democracy.d$country[democracy.d$country == "DOMINICAN REP."] <- "Dominican Republic"
democracy.d$country[democracy.d$country == "UNITED ARAB E."] <- "United Arab Emirates"



## to merge with the other datasets, change country names from all uppercase, to just first letter uppercase
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringi)
democracy.d$country <- stri_trans_totitle(dput(as.character(democracy.d$country)))



# merge census.d, inequality.d, democracy.d
df <- merge(census.d, inequality.d, by=c("country", "year"), all.x=T)
df <- merge(df, democracy.d, by=c("country", "year"), all.x=T)

# merging vdem and df datasets
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
vdem <- read_excel("../FDI_State_Capacity_Paper/Data/VDem/vdem2.xlsx") 

## clean VDEM and keep what we need.
vdem <- vdem[c("v2x_polyarchy", "v2x_libdem", "country_name", "year", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem")]
## change name of country variable
colnames(vdem)[3] <- "country"

## from charct to numeric
vdem$v2x_polyarchy = scan(text=vdem$v2x_polyarchy, dec=",", sep=".")
vdem$v2x_libdem = scan(text=vdem$v2x_libdem, dec=",", sep=".")
vdem$v2x_partipdem = scan(text=vdem$v2x_partipdem, dec=",", sep=".")
vdem$v2x_delibdem = scan(text=vdem$v2x_delibdem, dec=",", sep=".")
vdem$v2x_egaldem = scan(text=vdem$v2x_egaldem, dec=",", sep=".")

# merge DF with vdem
df <- merge(df, vdem, by=c("country", "year"), all.x=T) 



# WDI data added 
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
wdi <- read_excel("Data/WDI/wdi.xlsx")

colnames(wdi)[3] <- "country"

## changing some country names in wdi dataset to be consistent with the df dataset
wdi$country[wdi$country == "Bahamas, The"] <- "Bahamas"
wdi$country[wdi$country == "Czech Republic"] <- "Czechoslovakia"
wdi$country[wdi$country == "Egypt, Arab Rep."] <- "Egypt"
wdi$country[wdi$country == "Gambia, The"] <- "Gambia"
wdi$country[wdi$country == "Iran, Islamic Rep."] <- "Iran"

## renaming with shorter variables -- use first 15 characters of the long variables, kill white spaces and special characters.
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringr)
names(wdi)[5:ncol(wdi)] <- paste("wdi.", str_replace_all(stri_trans_totitle(dput(as.character(substr(colnames(wdi[,5:ncol(wdi)]), start = 1, stop = 15)))), "[^[:alnum:]]", ""), sep="")

## loop to change from charct to numeric // it also rounds to the first 10 digits.
for(i in c(5:ncol(wdi))) {
        wdi[,i] <- round(as.numeric(as.character(unlist(wdi[,i]))), 10)
}



## Adding WDI New Variables
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
wdi.new <- read_excel("../FDI_State_Capacity_Paper/Data/WDI/WDI_new_variables.xlsx") 

## renaming
names(wdi.new)[names(wdi.new) == 'Country Name'] <- 'country'
names(wdi.new)[names(wdi.new) == 'Time'] <- 'year'

names(wdi.new)[names(wdi.new) == 'General government final consumption expenditure (% of GDP)'] <- 'wdi.govt.exp.gdp'
names(wdi.new)[names(wdi.new) == 'General government final consumption expenditure (constant 2010 US$)'] <- 'wdi.govt.exp.const.2010'
names(wdi.new)[names(wdi.new) == 'General government final consumption expenditure (current US$)'] <- 'wdi.govt.exp.current.us.dollar'
names(wdi.new)[names(wdi.new) == 'Health expenditure, total (% of GDP)'] <- 'wdi.health.exp.gdp'


## keep just important variables
wdi.new <- wdi.new[, c("country", "year", "wdi.govt.exp.gdp", "wdi.govt.exp.const.2010", "wdi.govt.exp.current.us.dollar", "wdi.health.exp.gdp")]

wdi.new$wdi.health.exp.gdp = as.numeric(wdi.new$wdi.health.exp.gdp)
wdi.new$wdi.govt.exp.gdp = as.numeric(wdi.new$wdi.govt.exp.gdp)


## merging wdi with wdi.new
wdi <- merge(wdi, wdi.new, by=c("country", "year"), all.x=T) 

# merging df and wdi datasets, created a new one called dataset
df <- merge(df, wdi, by=c("country", "year"), all.x=T) 

## drop Yogoslavia (as it is not considered in the VDEM dataset)
df <- df[!(df$country=="Yugoslavia"),]




# New GDP from WDI (longer series)
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
wdi.gdp.d <- read_excel("../FDI_State_Capacity_Paper/Data/WDI/WDI_GDP_per_ capita_new.xlsx") 
## changing name of variables
names(wdi.gdp.d)[names(wdi.gdp.d) == "GDP per capita (constant 2010 US$) [NY.GDP.PCAP.KD]"] <- "wdi.gdp"
names(wdi.gdp.d)[names(wdi.gdp.d) == "Country Name"] <- "country"
names(wdi.gdp.d)[names(wdi.gdp.d) == "Time"] <- "year"
## keeping what we need.
wdi.gdp.d <- wdi.gdp.d[c("country", "year", "wdi.gdp")]
## deleting weird characters in wdi.gdp and replacing them with NA
wdi.gdp.d$wdi.gdp = as.numeric(wdi.gdp.d$wdi.gdp)
## merging with df
df <- merge(df, wdi.gdp.d, by=c("country", "year"), all.x=T) 


# Polity Merging
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
polity.d <- read_excel("Data/Polity/Polity_data.xlsx") 
## changing name of variables
names(polity.d)[names(polity.d) == "country_name"] <- "country"
names(polity.d)[names(polity.d) == "Polity2_revised"] <- "polity"
## keeping what we need.
polity.d <- polity.d[c("country", "year", "polity")]
## merging with df
# merging df and wdi datasets, created a new one called dataset
df <- merge(df, polity.d, by=c("country", "year"), all.x=T) 


# State Antiquity Merging
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
state.ant.d <- read_excel("../FDI_State_Capacity_Paper/Data/State_Antiquity/State_Antiquity_data.xlsx")
## drop NAs
state.ant.d = subset(state.ant.d, !is.na(`State hist 5`) | !is.na(`State hist 10`) | !is.na(`State hist 50`))
## change var names
colnames(state.ant.d)[3] <- "state.history.5"
colnames(state.ant.d)[4] <- "state.history.10"
colnames(state.ant.d)[5] <- "state.history.50"
## keeping what we need
state.ant.d <- state.ant.d[c("country", "state.history.5", "state.history.10", "state.history.50")]
## generating a data.table object
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)
state.ant.d = data.table(state.ant.d)
## merging
df <- merge(df, state.ant.d, by = "country", all = TRUE, sort = FALSE)


# regional dummies
# Load the census data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readstata13)

## Load Census data 
regions.d <- read.dta13("../FDI_State_Capacity_Paper/Data/Census_Data_Hanson/Census_data_until_2015.dta") 

## recoding
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)
regions.d$region <- recode(as.factor(regions.d$region), 
                           "1 = 'South/Central America' ; 
                           2 = 'Middle East' ; 
                           3 = 'East Europe' ; 
                           4 = 'Africa' ; 
                           5 = 'Central Asia' ; 
                           6 = 'South East Asia' ;
                           7 = 'Europe/U.S.' ")
## keeping what we need
regions.d <- regions.d[c("country", "year", "region")]
## merging
df <- merge(df, regions.d, by=c("country", "year"), all.x=T)



# Merge Fraser dataset
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
fraser.d <- read_excel("../FDI_State_Capacity_Paper/Data/Fraser/fraser.xlsx", sheet = "dataR") 

colnames(fraser.d) <- paste("fraser", colnames(fraser.d), sep = "_") # adds prefix to identify the source data

names(fraser.d)[names(fraser.d) == 'fraser_year'] <- 'year' # renames
names(fraser.d)[names(fraser.d) == 'fraser_country'] <- 'country' # renames
names(fraser.d)[names(fraser.d) == 'fraser_ISO_Code'] <- 'ISO' # renames

names(fraser.d) <- gsub(x = names(fraser.d), pattern = " ", replacement = "_") # replace blank with _ in var names

df <- merge(df, fraser.d, by=c("country", "year"), all.x=T) ## merging


# Merge GCID
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readxl)
gcid.d <- read_excel("../FDI_State_Capacity_Paper/Data/GCID/GCID_2006_2016.xlsx", sheet = "dataR") 

gcid.d <- gcid.d[!names(gcid.d) %in% c("Table 1", "X__1", "v3") ] # dropping vars

gcid.d = data.frame(t(gcid.d)) # transposing df

gcid.d = data.frame(reshape(gcid.d, # reshaping df
                            varying = c("X2", "X3","X4","X5","X6","X7","X8","X9","X10","X11"), 
                            v.names = "gcid_index",
                            timevar = "year", 
                            times =c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"), 
                            direction = "long")); rownames(gcid.d) <- NULL

names(gcid.d)[names(gcid.d) == 'X1'] <- 'country' # renames
gcid.d <- gcid.d[!names(gcid.d) %in% c("id") ] # dropping vars
gcid.d$year = as.factor(gcid.d$year)
options(digits=16) # sets precision
gcid.d$gcid_index = as.numeric(as.character(gcid.d$gcid_index)) # transforms character to numeric
df <- merge(df, gcid.d, by=c("country", "year"), all.x=T) # merging 



# Merging Asal_Conrad_Toronto
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Asal Conrad Toronto data 
act.d <- read.dta("../FDI_State_Capacity_Paper/Data/Asal_Conrad_Toronto/Asal_Conrad_Toronto.dta") 

## renaming with shorter variables -- use first 15 characters of the long variables, kill white spaces and special characters// Capitalizes first character // adds "act" to identify variables.
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringr)
names(act.d)[4:ncol(act.d)] <- paste("act.", str_replace_all(stri_trans_totitle(dput(as.character(substr(colnames(act.d[,4:ncol(act.d)]), start = 1, stop = 15)))), "[^[:alnum:]]", ""), sep="")

options(digits=16) # sets precision
df <- merge(df, act.d, by=c("ccode", "year"), all.x=T) # merging 



# Merging Good_For_The_Money
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Good_For_The_Money
gfm.d <- read.dta("../FDI_State_Capacity_Paper/Data/Good_For_The_Money/Good_For_The_Money.dta") 

gfm.d <- gfm.d[3:ncol(gfm.d)] # select columns we want

## renaming with shorter variables -- use first 15 characters of the long variables, kill white spaces and special characters// Capitalizes first character // adds "gfm" to identify variables.
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringr)
names(gfm.d)[3:ncol(gfm.d)] <- paste("gfm.", str_replace_all(stri_trans_totitle(dput(as.character(substr(colnames(gfm.d[3:ncol(gfm.d)]), start = 1, stop = 15)))), "[^[:alnum:]]", ""), sep="")

options(digits=16) # sets precision
df <- merge(df, gfm.d, by=c("ccode", "year"), all.x=T) # merging 



# fix(df)
# df$act

# Data Recoding
########################

# same DF but with our variables lagged and differenced BY COUNTRY

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(plm)

p.df <- pdata.frame(df, index = c("country", "year"), row.names = FALSE) # declare df as panel df. Panel is country.

p.df$lag.gini_disp.mean = lag(p.df$gini_disp.mean, k=1) 
p.df$lag.v2x_polyarchy = lag(p.df$v2x_polyarchy, k=1) 
p.df$lag.v2x_libdem = lag(p.df$v2x_libdem, k=1) 
p.df$lag.democracy = lag(p.df$democracy, k=1) 
p.df$lag.cum.census = lag(p.df$cum.census, k=1) 
p.df$lag.wdi.GdpPerCapita = lag(p.df$wdi.GdpPerCapita, k=1) 
p.df$lag.wdi.InflationGdp = lag(p.df$wdi.InflationGdp, k=1) 
p.df$lag.wdi.ForeignDirect = lag(p.df$wdi.ForeignDirect, k=1) 
p.df$lag.wdi.Manufacturing = lag(p.df$wdi.Manufacturing, k=1) 
p.df$lag.wdi.HealthExpendit = lag(p.df$wdi.HealthExpendit, k=1) 
p.df$lag.wdi.AgricultureVa = lag(p.df$wdi.AgricultureVa, k=1) 
p.df$lag.wdi.PopulationAges = lag(p.df$wdi.PopulationAges, k=1) 
p.df$lag.wdi.GovernmentExpe = lag(p.df$wdi.GovernmentExpe, k=1) 
p.df$lag.wdi.TaxesOnIncome = lag(p.df$wdi.TaxesOnIncome, k=1) 
p.df$lag.polity = lag(p.df$polity, k=1) 
p.df$lag.wdi.gdp = lag(p.df$wdi.gdp, k=1) 
p.df$lag.wdi.TradeOfGdp = lag(p.df$wdi.TradeOfGdp, k=1) 
p.df$lag.gini_mkt.mean = lag(p.df$gini_mkt.mean, k=1) 
p.df$lag.gini_rel_red.mean = lag(p.df$gini_rel_red.mean, k=1) 
p.df$lag.wdi.TaxRevenue = lag(p.df$wdi.TaxRevenue, k=1) 
p.df$lag.wdi.OilRentsOf = lag(p.df$wdi.OilRentsOf, k=1) 
p.df$lag.wdi.UrbanPopulatio = lag(p.df$wdi.UrbanPopulatio, k=1) 
p.df$lag.wdi.PopulationGrow = lag(p.df$wdi.PopulationGrow, k=1) 
p.df$lag.fraser_econ_fred_sum = lag(p.df$fraser_econ_fred_sum, k=1) 
p.df$lag.fraser_Protection_of_property_rights= lag(p.df$fraser_Protection_of_property_rights, k=1) 
p.df$lag.fraser_Integrity_of_the_legal_system= lag(p.df$fraser_Integrity_of_the_legal_system, k=1) 
p.df$lag.fraser_Legal_enforcement_of_contracts= lag(p.df$fraser_Legal_enforcement_of_contracts, k=1) 
p.df$lag.fraser_Impartial_courts= lag(p.df$fraser_Impartial_courts, k=1) 



p.df$lag.gcid_index= lag(p.df$gcid_index, k=1) 
p.df$lag.gcid_index= lag(p.df$gcid_index, k=1) 
p.df$lag.gfm.Cim1= lag(p.df$gfm.Cim1, k=1) 
p.df$lag.gfm.Iiavg1= lag(p.df$gfm.Iiavg1, k=1) 
p.df$lag.act.Thomrival= lag(p.df$act.Thomrival, k=1) 
p.df$lag.act.Inter= lag(p.df$act.Inter, k=1) 
p.df$lag.act.Intra= lag(p.df$act.Intra, k=1) 
p.df$lag.wdi.health.exp.gdp = lag(p.df$wdi.health.exp.gdp, k=1) 
p.df$lag.wdi.govt.exp.gdp = lag(p.df$wdi.govt.exp.gdp, k=1) 


# differentiating variables
p.df$diff.gini_disp.mean =  diff(p.df$gini_disp.mean, k=1)
p.df$diff.v2x_polyarchy =  diff(p.df$v2x_polyarchy, k=1)
p.df$diff.v2x_libdem =  diff(p.df$v2x_libdem, k=1)
p.df$diff.democracy =  diff(p.df$democracy, k=1)
p.df$diff.cum.census =  diff(p.df$cum.census, k=1)
p.df$diff.wdi.GdpPerCapita =  diff(p.df$wdi.GdpPerCapita, k=1)
p.df$diff.wdi.InflationGdp =  diff(p.df$wdi.InflationGdp, k=1)
p.df$diff.wdi.ForeignDirect =  diff(p.df$wdi.ForeignDirect, k=1)
p.df$diff.wdi.Manufacturing =  diff(p.df$wdi.Manufacturing, k=1)
p.df$diff.wdi.HealthExpendit =  diff(p.df$wdi.HealthExpendit, k=1)
p.df$diff.wdi.AgricultureVa =  diff(p.df$wdi.AgricultureVa, k=1)
p.df$diff.wdi.PopulationAges =  diff(p.df$wdi.PopulationAges, k=1)
p.df$diff.wdi.GovernmentExpe =  diff(p.df$wdi.GovernmentExpe, k=1)
p.df$diff.polity =  diff(p.df$polity, k=1)
p.df$diff.wdi.gdp =  diff(p.df$wdi.gdp, k=1)
p.df$diff.wdi.TradeOfGdp =  diff(p.df$wdi.TradeOfGdp, k=1)
p.df$diff.gini_mkt.mean =  diff(p.df$gini_mkt.mean, k=1)
p.df$diff.gini_rel_red.mean =  diff(p.df$gini_rel_red.mean, k=1)
p.df$diff.wdi.TaxRevenue =  diff(p.df$wdi.TaxRevenue, k=1)
p.df$diff.wdi.OilRentsOf =  diff(p.df$wdi.OilRentsOf, k=1)
p.df$diff.wdi.UrbanPopulatio =  diff(p.df$wdi.UrbanPopulatio, k=1)
p.df$diff.fraser_econ_fred_sum =  diff(p.df$fraser_econ_fred_sum, k=1)
p.df$diff.fraser_Protection_of_property_rights=  diff(p.df$fraser_Protection_of_property_rights, k=1)
p.df$diff.fraser_Integrity_of_the_legal_system=  diff(p.df$fraser_Integrity_of_the_legal_system, k=1)
p.df$diff.fraser_Legal_enforcement_of_contracts=  diff(p.df$fraser_Legal_enforcement_of_contracts, k=1)
p.df$diff.fraser_Impartial_courts=  diff(p.df$fraser_Impartial_courts, k=1)
p.df$diff.gcid_index =  diff(p.df$gcid_index, k=1)
p.df$diff.gfm.Cim1 = diff(p.df$gfm.Cim1, k=1)
p.df$diff.gfm.Iiavg1 =  diff(p.df$gfm.Iiavg1, k=1)
p.df$diff.wdi.health.exp.gdp =  diff(p.df$wdi.health.exp.gdp, k=1)
p.df$diff.wdi.govt.exp.gdp =  diff(p.df$wdi.govt.exp.gdp, k=1)


#df <- data.frame(p.df) # convert panel df to regular df.
df <- p.df # convert panel df to regular df.


# for fixed effects
#df$country = as.factor(df$country)
# df$year = as.factor(df$year)


# dataset with country ISO codes (for graphs)
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Good_For_The_Money
iso.ccode.d <- read.csv("../FDI_State_Capacity_Paper/Data/ISO_ccode/iso_ccode.csv") 

iso.ccode.d <- iso.ccode.d[c("alpha.2", "alpha.3", "region.code", "name")]

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stringr)
names(iso.ccode.d) <- paste("iso.", str_replace_all(stri_trans_totitle(dput(as.character(substr(colnames(iso.ccode.d), start = 1, stop = 15)))), "[^[:alnum:]]", ""), sep="")
names(iso.ccode.d)[names(iso.ccode.d) == 'iso.Name'] <- 'country' # renames

iso.ccode.d$country = as.character(iso.ccode.d$country)
iso.ccode.d$iso.Alpha2 = as.character(iso.ccode.d$iso.Alpha2)
iso.ccode.d$iso.Alpha3 = as.character(iso.ccode.d$iso.Alpha3)

df <- merge(df, iso.ccode.d, by=c("country"), all.x=T) # merging 



# complete cases // this is to input numbers in the paper
completeFun <- function(data, desiredCols) {
        completeVec <- complete.cases(data[, desiredCols])
        return(data[completeVec, ])
}

df.complete.model.1 = completeFun(df, c("iso.Alpha3", "diff.wdi.ForeignDirect", "lag.wdi.ForeignDirect", "lag.wdi.AgricultureVa", "lag.cum.census", "lag.wdi.Manufacturing", "lag.cum.census", "lag.polity", "diff.polity",  "lag.wdi.InflationGdp", "diff.wdi.InflationGdp", "lag.wdi.PopulationAges", "diff.wdi.PopulationAges", "lag.wdi.UrbanPopulatio", "diff.wdi.UrbanPopulatio", "country","year"))


# save dataset
save(df, file = "../FDI_State_Capacity_Paper/df.RData")



########################
# All models
########################
cat("\014")
rm(list=ls())

# Regions are
# "Africa", "Central Asia", "East Europe", "Europe/U.S.", "Middle East", "South East Asia" #"South/Central America". // != means does NOT equal to, == means equals to


# load data
load("../FDI_State_Capacity_Paper/df.RData") 




########################################
######  MAIN MODELS
########################################

# Model 1
options(scipen=9999999)
model.1 = lm(diff.wdi.ForeignDirect ~ 
                     # Lagged DV
                     lag.wdi.ForeignDirect +
                     # Constitutive Terms
                             # lag.wdi.AgricultureVa +
                             # lag.wdi.Manufacturing +
                             # lag.cum.census +
                     # Interactions
                     lag.wdi.AgricultureVa * lag.cum.census +
                     lag.wdi.Manufacturing * lag.cum.census +
                     # Political Controls
                     lag.democracy + diff.democracy +
                     lag.fraser_Impartial_courts + diff.fraser_Impartial_courts +
                     # Economic Controls
                     lag.wdi.InflationGdp + diff.wdi.InflationGdp + 
                     lag.gini_disp.mean + diff.gini_disp.mean +
                     lag.wdi.PopulationAges + diff.wdi.PopulationAges +
                     # Demographic Controls
                     lag.wdi.UrbanPopulatio + diff.wdi.UrbanPopulatio + 
                     # Fixed Effects
                     factor(country) +  
                     factor(year),
             data = df)

# summary(model.1)



# Pending
## try wdi.IndustryValue
## try wdi.HighTechnology


## Computing Conditional Effects

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(DAMisc) # if it asks compilation for sm package, type "no"


# plot 1
DAintfun2(model.1, c("lag.wdi.AgricultureVa", "lag.cum.census"), rug = T, hist = T, scale.hist=.7,
          #xlab = c("State Capacity\n(Cumulative Census)", "Democracy\n(Polity)"), 
          nclass = c(16,20),
          border=NA#,
          #ylab = c("Conditional Effect of Democracy", "Conditional Effect of State Capacity")
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf"
)

# plot 2
DAintfun2(model.1, c("lag.wdi.Manufacturing", "lag.cum.census"), rug = T, hist = T, scale.hist=.7,
          #xlab = c("State Capacity\n(Cumulative Census)", "Democracy\n(Polity)"), 
          nclass = c(16,20),
          border=NA#,
          #ylab = c("Conditional Effect of Democracy", "Conditional Effect of State Capacity")
          #name.stem = c("cond_eff","cond_eff"), 
          #plot.type = "pdf"
)

## 