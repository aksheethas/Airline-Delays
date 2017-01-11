#DATA PROCESSING

###The final dataframe as a result of the data processing has been provided in the submission

##########################################################################################################
library(readr)
library(data.table)

#Reading in all the files (in total there were 31 files that had to be read in)

#processing 1988, 1991, 1994, 1997, 2000, 2003, 2006
files <- list.files(path = '~/Documents/MSDS/Linear_Regression/Finalproject/FinalProjUnzip', pattern = ".csv")
temp <- lapply(files, fread, sep=",")
names(temp)[1] <- "1988"
names(temp)[2] <- "1991"
names(temp)[3] <- "1994"
names(temp)[4] <- "1997"
names(temp)[5] <- "2000"
names(temp)[6] <- "2003"
names(temp)[7] <- "2006"
list2env(temp, envir=.GlobalEnv)
dffull <- rbindlist(temp) 

#processing 2009
files09 <- list.files(path = '~/Documents/MSDS/Linear_Regression/Finalproject/RecentYears/2009Years', pattern = ".csv")
temp09 <- lapply(files09, fread, sep=",", drop=30) 
df09 <- rbindlist(temp09)

#processing 2012
files12 <- list.files(path = '~/Documents/MSDS/Linear_Regression/Finalproject/RecentYears/2012Years', pattern = ".csv")
temp12 <- lapply(files12, fread, sep=",", drop=30) 
df12 <- rbindlist(temp12)

#processing 2015
files15 <- list.files(path = '~/Documents/MSDS/Linear_Regression/Finalproject/RecentYears/2015Years', pattern = ".csv")
temp15 <- lapply(files15, fread, sep=",", drop=30) 
df15 <- rbindlist(temp15)

#saving dataframes into RDS files for compression of size
saveRDS(df09, file = '~/Documents/MSDS/Linear_Regression/Finalproject/dataframe2009', ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(dffull, file = '~/Documents/MSDS/Linear_Regression/Finalproject/dataframefull.rds', ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(df12, file = '~/Documents/MSDS/Linear_Regression/Finalproject/dataframe2012.rds', ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

saveRDS(df15, file = '~/Documents/MSDS/Linear_Regression/Finalproject/dataframe2015.rds', ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

#########################################################################################
#Creating the full dataframe
                             
df2012 <- readRDS('dataframe2012.rds')
df2009 <- readRDS('dataframe2009')
df2015 <- readRDS('dataframe2015.rds')
dffull <- readRDS('dataframefull.rds')

#3 airports that we're going to look at LGA, JFK, EWR - subset the DFs
df2009 <- df2009[df2009$DEST == 'LGA' | df2009$DEST == 'JFK' | df2009$DEST == 'EWR'
| df2009$ORIGIN == 'LGA' | df2009$ORIGIN == 'JFK' | df2009$ORIGIN == 'EWR',]
df2012 <- df2012[df2012$DEST == 'LGA' | df2012$DEST == 'JFK' | df2012$DEST == 'EWR'
| df2012$ORIGIN == 'LGA' | df2012$ORIGIN == 'JFK' | df2012$ORIGIN == 'EWR',]
df2015 <- df2015[df2015$DEST == 'LGA' | df2015$DEST == 'JFK' | df2015$DEST == 'EWR'
| df2015$ORIGIN == 'LGA' | df2015$ORIGIN == 'JFK' | df2015$ORIGIN == 'EWR',]
dffull <- dffull[dffull$Origin == "LGA" | dffull$Origin  == "JFK" | dffull$Origin  == "EWR" 
| dffull$Dest == "LGA" | dffull$Dest == "JFK" | dffull$Dest == "EWR",]

colnames(dffull)[which(names(dffull) == "Year")] <- "YEAR"
colnames(dffull)[which(names(dffull) == "Month")] <- "MONTH"
colnames(dffull)[which(names(dffull) == "DayofMonth")] <- "DAY_OF_MONTH"
colnames(dffull)[which(names(dffull) == "DayOfWeek")] <- "DAY_OF_WEEK"
colnames(dffull)[which(names(dffull) == "DepTime")] <- "DEP_TIME"
colnames(dffull)[which(names(dffull) == "CRSDepTime")] <- "CRS_DEP_TIME"
colnames(dffull)[which(names(dffull) == "ArrTime")] <- "ARR_TIME"
colnames(dffull)[which(names(dffull) == "CRSArrTime")] <- "CRS_ARR_TIME"
colnames(dffull)[which(names(dffull) == "UniqueCarrier")] <- "UNIQUE_CARRIER"
colnames(dffull)[which(names(dffull) == "FlightNum")] <- "FL_NUM"
colnames(dffull)[which(names(dffull) == "TailNum")] <- "TAIL_NUM"
colnames(dffull)[which(names(dffull) == "ActualElapsedTime")] <- "ACTUAL_ELAPSED_TIME"
colnames(dffull)[which(names(dffull) == "CRSElapsedTime")] <- "CRS_ELAPSED_TIME"
colnames(dffull)[which(names(dffull) == "AirTime")] <- "AIR_TIME"
colnames(dffull)[which(names(dffull) == "ArrDelay")] <- "ARR_DELAY"
colnames(dffull)[which(names(dffull) == "DepDelay")] <- "DEP_DELAY"
colnames(dffull)[which(names(dffull) == "Origin")] <- "ORIGIN"
colnames(dffull)[which(names(dffull) == "Dest")] <- "DEST"
colnames(dffull)[which(names(dffull) == "Distance")] <- "DISTANCE"
colnames(dffull)[which(names(dffull) == "TaxiIn")] <- "TAXI_IN"
colnames(dffull)[which(names(dffull) == "TaxiOut")] <- "TAXI_OUT"
colnames(dffull)[which(names(dffull) == "Cancelled")] <- "CANCELLED"
colnames(dffull)[which(names(dffull) == "CancellationCode")] <- "CANCELLATION_CODE"
colnames(dffull)[which(names(dffull) == "Diverted")] <- "DIVERTED"
colnames(dffull)[which(names(dffull) == "CarrierDelay")] <- "CARRIER_DELAY"
colnames(dffull)[which(names(dffull) == "WeatherDelay")] <- "WEATHER_DELAY"
colnames(dffull)[which(names(dffull) == "NASDelay")] <- "NAS_DELAY"
colnames(dffull)[which(names(dffull) == "SecurityDelay")] <- "SECURITY_DELAY"
colnames(dffull)[which(names(dffull) == "LateAircraftDelay")] <- "LATE_AIRCRAFT_DELAY"

#Create the dataframe and write it to csv for easier reading in
df <- rbind(df2009, df2012, df2015, dffull)

#Take out the majority of the years so that the dataframe is manageable
df <- df[df$YEAR == 2015 | df$YEAR == 2006 | df$YEAR == 2000 | df$YEAR == 1994,]

#Take out the cancelled flights because they have NAs for all columns
df <- df[df$CANCELLED == '0',]
saveRDS(df, file = 'dataframe.rds', ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
