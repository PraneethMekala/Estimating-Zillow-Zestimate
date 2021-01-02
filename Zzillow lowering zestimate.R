"Project Zillow: Lowering Zestimate"

setwd("~/Documents/Data Science Projects/Zillow Zestimate/zillow prize project data") #mac
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(leaps)


setwd("~/Documents/Data Science Projects/Zillow Zestimate") #mac
source("zillow EDA", echo = TRUE) #allows me to run and sample all of the script previously

good_feature = filter(missing_values, missing_pct<0.25)
good_feature
gfeature = as.vector(good_feature[,1])
## For logerror
zdata = cor_tmp %>% select(logerror, gfeature)
## For abs_logerror
zdata3 = cor_tmp %>% select(abs_logerror, gfeature)
#remove variables for zdata3 and zdata
zdata3 <- subset(zdata3, select = -c(id_parcel, fips, longitude, zoning_landuse_county, zoning_property, rawcensustractandblock, region_city, region_zip, censustractandblock))
zdata3 <- subset(zdata3, select = -c(tax_year))
zdata3 <- subset(zdata3, select = -c(tax_building, tax_land))
str(zdata3) #16 variables
zdata <- subset(zdata, select = -c(id_parcel, fips, longitude, zoning_landuse_county, zoning_property, rawcensustractandblock, region_city, region_zip, censustractandblock))
zdata <- subset(zdata, select = -c(tax_year))
zdata <- subset(zdata, select = -c(tax_building, tax_land))
str(zdata) #16 variables

#Find correlation between all variables for zdata3
corrplot(cor(zdata3, use="complete.obs"), type="lower", method="num")
#Remove tax_property, num_bathroom_calc, num_bathroom, area_live_finished
zdata3 <- subset(zdata3, select = -c(tax_property, num_bathroom_calc, num_bathroom, area_live_finished))
corrplot(cor(zdata3, use="complete.obs"), type="lower", method="num")
str(zdata3) #convert latitude and region_county to factor
#zdata
corrplot(cor(zdata, use="complete.obs"), type="lower", method="num")
#Remove tax_property, num_bathroom_calc, num_bathroom, area_live_finished
zdata <- subset(zdata, select = -c(tax_property, num_bathroom_calc, num_bathroom, area_live_finished))
corrplot(cor(zdata, use="complete.obs"), type="lower", method="num")
str(zdata) #convert zoning_landuse and tax_deliquency to factor

#find the proper int variables and convert to factor
zdata3$zoning_landuse = as.factor(zdata3$zoning_landuse)
zdata3$tax_delinquency = as.factor(zdata3$tax_delinquency)
str(zdata3)
zdata$zoning_landuse = as.factor(zdata$zoning_landuse)
zdata$tax_delinquency = as.factor(zdata$tax_delinquency)
str(zdata)


#logerror from zdata
fit03 = lm(logerror ~ build_year + area_total_calc + area_lot + num_bedroom + num_bath + num_room + region_county + latitude + zoning_landuse + tax_total + tax_delinquency, data=zdata)
summary(fit03)
#find best model using regsubsets
regfit.full=regsubsets(logerror~.,zdata)
summary(regfit.full)
#best model
fit04 = lm(logerror ~ area_total_calc + tax_total + tax_delinquency, data=zdata)
summary(fit04)

#abs_logerror from zdata3
fit05 = lm(abs_logerror ~ build_year + area_total_calc + area_lot + num_bedroom + num_bath + num_room + region_county + latitude + zoning_landuse + tax_total + tax_delinquency, data=zdata3)
summary(fit05)
#find best model using regsubsets
regfit.full=regsubsets(abs_logerror~.,zdata3)
summary(regfit.full)
#best model
fit06 = lm(abs_logerror ~ build_year + area_total_calc + num_bedroom + region_county + tax_delinquency, data=zdata3)
summary(fit06)

"By using regsubsets to find the best model and running the linear regression 
for logerror, we can see that the variables and the coefficients which can predict the Zestimate's forcast errors. 
We can see that through these varaibles (it is possible to say that)despite a low r square value) the Zestimate is not a perfect model,
allowing for the variables; tax_total, tax_deliquency1, and area_total_calc to be predictors of the error. 

By taking the abs_logerror, we see the statistically signifigant variables are;
build_year, area_total_calc, num_bedroom, region_county, tax_delinquency1."