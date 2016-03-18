library(plyr)
library(sqldf)
library(ggplot2)
require(gdata)
library(plotrix)
bk <- read.xls("rollingsales_brooklyn.xls",pattern="BOROUGH", perl = "/usr/bin/perl")
colnames(bk) <- gsub("\\.","_",colnames(bk))
bk$SALE_PRICE_N <- as.numeric(gsub("[^[:digit:]]","",bk$SALE_PRICE))
count(is.na(bk$SALE_PRICE_N))
names(bk) <- tolower(names(bk))

## clean/format the data with regular expressions
bk$gross_sqft <- as.numeric(gsub("[^[:digit:]]","",bk$gross_square_feet))
bk$land_sqft <- as.numeric(gsub("[^[:digit:]]","", bk$land_square_feet))
bk$sale_date <- as.Date(bk$sale_date)
bk$year_built <- as.numeric(as.character(bk$year_built))

#Classify based on Built year
bk$built_era[bk$year_built<1800] <- "No Data Available"
bk$built_era[bk$year_built>=1800 & bk$year_built <1850] <- "early 1800s"
bk$built_era[bk$year_built>=1850 & bk$year_built <1900] <- "late 1800s"
bk$built_era[bk$year_built>=1900 & bk$year_built <1950] <- "early 1900s"
bk$built_era[bk$year_built>=1950 & bk$year_built <2000] <- "late 1900s"
bk$built_era[bk$year_built>=2000 & bk$year_built <2050] <- "early 2000s"



bk$tax_class_at_present <- as.numeric(as.character(bk$tax_class_at_present))
bk$year_built_cat <-cut(bk$year_built,c(-Inf,0,1800,1850,1900,1950,2000,2016,Inf))
bk$sale_year <- as.numeric(format(bk$sale_date, "%Y"))

# clean Neighborhood



# bk <- subset(bk, select = -ease_ment)
# bk <- subset(bk , !is.null(levels(bk$neighborhood)))


#Outliers --> REMOVE ONES WITH SALE-YEAR BEFORE BUILT YEAR
bk_clean <- subset(bk, bk$sale_year > bk$year_built)

# SUBSET SALE DATA ONLY, i.e. ONLY THE DATA WILL SALE YEAR
bk_sale <- bk_clean[bk_clean$sale_price_n!=0,]

#Removing Outliers

#Neighborhood vs No of Houses Sold
ggplot(bk_sale, aes(x = neighborhood, fill=built_era))+geom_bar()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ylab("No of Houses Sold")+ xlab("Neighborhood")
#Number of houses sold in Each year Category
ggplot(bk_sale, aes(x = built_era ))+geom_bar()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("No of Houses Sold")+ xlab("Era")

#Mean Price per sale year
mean_sales_year <- sqldf("select sale_year, (sum(sale_price_n)/count(sale_price_n)) as Mean_Sale from bk_sale group by sale_year")
barplot( mean_sales_year[,2] ,names.arg = mean_sales_year[,1], cex.axis = 1)

#Mean Price per built year 
#2012
bk_sale_2012 <- subset(bk_sale, sale_year = 2012)
mean_sales_year2012 <- sqldf("select built_era, (sum(sale_price_n)/count(sale_price_n)) as Mean_Sale_Price from bk_sale_2012 where built_era <> \"No Data Available\" group by built_era")
#2013
bk_sale_2013 <- subset(bk_sale, sale_year = 2013)
mean_sales_year2012 <- sqldf("select built_era, (sum(sale_price_n)/count(sale_price_n)) as Mean_Sale_Price from bk_sale_2013 where built_era <> \"No Data Available\" group by built_era")
##Check this plot
ggplot(mean_sales_year2012 ,aes(x = mean_sales_year2012[,1], y= mean_sales_year2012[,2]))+geom_line()
plot(mean_sales_year2012[,1], mean_sales_year2012[,2])
barplot( mean_sales_year2012[,2], names.arg = mean_sales_year2012[,1])#,names.arg = mean_sales_year2012[,1], cex.axis = 1)

# Built year Cat vs total Sales amount
total_sales_year_cat <- sqldf("select built_era, sum(sale_price_n) as Total_Sale from bk_sale where built_era <> \"No Data Available\" group by built_era")
pie3D(total_sales_year_cat[,2], labels = total_sales_year_cat[,1], main = "Divisions of Sales by ERA", theta = 1, col =rainbow(length(total_sales_year_cat[,1])), start = 200, explode = 0.1)