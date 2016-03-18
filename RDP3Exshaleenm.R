library(plyr)
library(sqldf)
library(ggplot2)
require(gdata)
library(plotrix)
data <- read.xls("rollingsales_brooklyn.xls",pattern="BOROUGH", perl = "/usr/bin/perl")
data<- rbind(data, read.xls("rollingsales_bronx.xls",pattern="BOROUGH", perl = "/usr/bin/perl"))
data<- rbind(data, read.xls("rollingsales_manhattan.xls",pattern="BOROUGH", perl = "/usr/bin/perl"))
data<- rbind(data, read.xls("rollingsales_queens.xls",pattern="BOROUGH", perl = "/usr/bin/perl"))
data<- rbind(data, read.xls("rollingsales_statenisland.xls",pattern="BOROUGH", perl = "/usr/bin/perl"))
colnames(data) <- gsub("\\.","_",colnames(data))
data$SALE_PRICE_N <- as.numeric(gsub("[^[:digit:]]","",data$SALE_PRICE))
count(is.na(data$SALE_PRICE_N))
names(data) <- tolower(names(data))

## clean/format the data with regular expressions
data$gross_sqft <- as.numeric(gsub("[^[:digit:]]","",data$gross_square_feet))
data$land_sqft <- as.numeric(gsub("[^[:digit:]]","", data$land_square_feet))
data$sale_date <- as.Date(data$sale_date)
data$year_built <- as.numeric(as.character(data$year_built))

#Classify based on Built year
data$built_era[data$year_built<1800] <- "No Data Available"
data$built_era[data$year_built>=1800 & data$year_built <1850] <- "early 1800s"
data$built_era[data$year_built>=1850 & data$year_built <1900] <- "late 1800s"
data$built_era[data$year_built>=1900 & data$year_built <1950] <- "early 1900s"
data$built_era[data$year_built>=1950 & data$year_built <2000] <- "late 1900s"
data$built_era[data$year_built>=2000 & data$year_built <2050] <- "early 2000s"

data$tax_class_at_present <- as.numeric(as.character(data$tax_class_at_present))
data$year_built_cat <-cut(data$year_built,c(-Inf,0,1800,1850,1900,1950,2000,2016,Inf))
data$sale_year <- as.numeric(format(data$sale_date, "%Y"))

# clean Neighborhood
#Outliers --> REMOVE ONES WITH SALE-YEAR BEFORE BUILT YEAR
data_clean <- subset(data, data$sale_year > data$year_built)

# SUBSET SALE DATA ONLY, i.e. ONLY THE DATA WILL SALE YEAR
data_sale <- data_clean[data_clean$sale_price_n!=0,]

# Mean of Sale price across burrows
mean_sale_burrow <- as.data.frame(sqldf("select borough, avg(sale_price_n) Avg_Sale from data_sale group by borough"))
p<-barplot( mean_sale_burrow[,2] ,names.arg = mean_sale_burrow[,1], cex.axis = 1)
axis(1, at=p,labels = c("Manhattan","Bronx","Brooklyn","Queens","Staten Island"))

# Borough wise no of deals made
ggplot(data_sale , aes(x = borough))+geom_bar()+scale_x_discrete(breaks=c("1","2","3","4","5"),labels=c("Manhattan","Bronx","Brooklyn","Queens","Staten Island")) 

# Mean rate of land
mean_rate_burrow <- sqldf("select borough, avg(sale_price_n/land_sqft) from data_sale group by borough")
p2<- barplot( mean_rate_burrow[,2] ,names.arg = mean_rate_burrow[,1], cex.axis = 1)##Give name to borough
axis(1, at=p2,labels = c("Manhattan","Bronx","Brooklyn","Queens","Staten Island"))
