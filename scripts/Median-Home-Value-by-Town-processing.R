library(dplyr)
library(devtools)
load_all('../datapkg')
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Median Home Value by Town
# Created by Jenna Daly
# On 11/27/2017
#
##################################################################

options(scipen=999)
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2017,
    table = "B25077"
)

med_home_val <- data.table()
for (data in acsdata) {
    year <- data@endyear
    total <- acsSum(data, 1, "Median Value")
    datafips <- data.table(fips = getACSFips(data))

    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(total)       
    )
    
    names(estimates)[names(estimates) == "HD01_VD01.Estimate; Median value (dollars)"] <- "Median Home Value"


    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Variable",
        variable.factor = F,
        value.name = "Number",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(total) * 1.645      
    )
    
    names(moes)[names(moes) == "HD01_VD01.Estimate; Median value (dollars)"] <- "Median Home Value"


    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Variable",
        variable.factor = F,
        value.name = "Margins of Error",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Variable`)
    setkey(moes, FIPS, Year, `Variable`)

    med_home_val <- rbind(med_home_val, estimates[moes])
}

#create CT df to merge with original to caluate ratios

ct_value <- med_home_val[med_home_val$FIPS == "09",]

merge <- merge(med_home_val, ct_value, by = "Year")

#set any NA to 1000001 to replicate "1,000,000+" in the raw Census data
merge$Number.x[is.na(merge$Number.x)] <- 1000001

merge$`Ratio to State Median Num` <- merge$Number.x / merge$Number.y
merge$`Ratio to State Median MOE` <- sqrt((merge$`Margins of Error.x`)^2 + ((merge$Number.x/merge$Number.y)*((merge$`Margins of Error.y`)^2))) / (merge$Number.y)

merge <- merge %>% 
  select(FIPS.x, Year, Variable.x, Number.x, `Margins of Error.x`, `Ratio to State Median Num`, `Ratio to State Median MOE`) 

merge <- merge[merge$FIPS.x != "0900100000",]

data_long <- gather(merge, `Measure Type`, Value, 4:7)

#Clean up columns
data_long$Variable.x[grepl("Margins", data_long$`Measure Type`)] <- "Margins of Error"
data_long$Variable.x[grepl("MOE", data_long$`Measure Type`)] <- "Margins of Error"
data_long$`Measure Type` <- gsub(".x", "", data_long$`Measure Type`)
data_long$`Measure Type`[data_long$`Measure Type` == "Margins of Error"] <- "Number"
data_long$`Measure Type` <- gsub(" Num", "", data_long$`Measure Type`)
data_long$`Measure Type` <- gsub(" MOE", "", data_long$`Measure Type`)

#Merge in Towns by FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

data_long_fips <- merge(data_long, towns, by.x = "FIPS.x", by.y = "FIPS")

data_long_fips$Year <- paste(data_long_fips$Year-4, data_long_fips$Year, sep="-")

data_long_final <- data_long_fips %>% 
  select(Town, FIPS.x, Year, `Measure Type`, Variable.x, Value) 
    
names(data_long_final)[names(data_long_final) == "FIPS.x"] <- "FIPS"
names(data_long_final)[names(data_long_final) == "Variable.x"] <- "Variable"

data_long_final$Value <-  round(data_long_final$Value, 2)

data_long_final <- data_long_final %>% 
  arrange(Town, Year, `Measure Type`, desc(Variable))

write.table(
    data_long_final,
    file.path("data", "median-home-value-town-2017.csv"),
    sep = ",",
    row.names = F,
    col.names = T,
    na = "-9999" 
)
