#HYPOTHESIS 3: To understand whether the distance between airports influences the magnitude of a flight’s gain time 
#(gain takes into account both the arrival and departure delays and is the difference between the two)

library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(plyr)
library(RColorBrewer)

setwd('~/Documents/MSDS/Linear_Regression')

#reading in dataframe
df <- readRDS('dataframe.rds') 

#removing cancellation code column
df <- subset(df, select = -19) #removed cancellation code column

#turning some of the variables into factors
df$UNIQUE_CARRIER <- as.factor(df$UNIQUE_CARRIER)
df$TAIL_NUM <- as.factor(df$TAIL_NUM)
df$ORIGIN <- as.factor(df$ORIGIN)
df$DEST <- as.factor(df$DEST)

#To the main dataframe adding several variables that provide useful information for this analysis
#reading in a file of information for carriers
airline_codes <- read_csv('carriers.csv') 
colnames(airline_codes)[1] <- 'UNIQUE_CARRIER'
#match on the carrier code and merge
df1 <- merge(df, airline_codes, by='UNIQUE_CARRIER', all.x = TRUE) #doing a left outer join
#23 unique airlines (American Airlines, Virgin America, etc.)

#combine the month, day and year into 1 column (from package lubridate)
df1$Time <- mdy(sprintf('%s %s %s', df1$MONTH, df1$DAY_OF_MONTH, df1$YEAR))

#can also combine the city and state columns to the dataframe
airports <- read_csv('airports.csv')
keeps <- c('iata', 'city', 'state') 
airportcity <- subset(airports, select=keeps)
colnames(airportcity)[1] <- 'ORIGIN'
#mergining  on origin airport code
df1 <- merge(df1, airportcity, by='ORIGIN', all.x = TRUE)
#change it to the origin city
colnames(df1)[32] <- 'ORIGIN_CITY'
#adding a new column that has both the city and state
df1$ORIGIN_LOC <- paste(df1$city, df1$ORIGIN_CITY, sep=",")
#drop the city and ORIGIN_CITY columns
df1 <- subset(df1, select = c(-31, -32))

#do the same and get the destination city and state
colnames(airportcity)[1] <- 'DEST'
#merge the destination with df1
df1 <- merge(df1, airportcity, by='DEST', all.x=TRUE)
#adding a new column with both the city and state
df1$DEST_LOC <- paste(df1$city, df1$state, sep=",")
df1 <- subset(df1, select = c(-32, -33))
colnames(df1)[29] <- "CARRIER_NAME"

#merge the latitude and longitude to the main dataframe for visual purposes
keeps <- c('iata', 'lat', 'long')
airportcord <- subset(airports, select=keeps)
colnames(airportcord)[1] <- 'ORIGIN'
df1 <- merge(df1, airportcord, by='ORIGIN', all.x = TRUE)
colnames(df1)[33] <- 'ORIGIN_LAT.Y'
colnames(df1)[34] <- 'ORIGIN_LONG.X'

colnames(airportcord)[1] <- 'DEST'
df1 <- merge(df1, airportcord, by='DEST', all.x = TRUE)
colnames(df1)[35] <- 'DEST_LAT.Y'
colnames(df1)[36] <- 'DEST_LONG.X'

#creating a map
library(geosphere)
library(maps)
# xlim = c(-125, -62.5)
# map('state', col = '#f2f2f2', fill = T, xlim = xlim, boundary = T, lty = 0)
# inter <- gcIntermediate(c(df1$ORIGIN_LONG.X, df1$ORIGIN_LAT.Y), c(df1$DEST_LONG.X, df1$DEST_LAT.Y), n=50, addStartEnd=TRUE)
# lines(inter, col = 'red', lwd = 2)
# text(df1$ORIGIN_LONG.X, df1$ORIGIN_LAT.Y, df1$ORIGIN_LOC, col = 'blue', adj = c(-0.1, 1.25))
# text(df1$DEST_LONG.X, df1$DEST_LAT.Y, df1$DEST_LOC, col = 'blue', adj = c(-0.1, 1.25))
# points(df1$DEST_LONG.X, df1$DEST_LAT.Y, cex = 1.5)
# points(df1$ORIGIN_LONG.X, df1$ORIGIN_LAT.Y, cex = 1.5)

###FIGURE THIS OUT
map('state', col = '#f2f2f2', fill = T, xlim = xlim, boundary = T, lty = 0)
fsub <- flights[flights$airline == "AA",]
for (j in 1:length(fsub$airline)) {
  air1 <- airports[airports$iata == fsub[j,]$airport1,]
  air2 <- airports[airports$iata == fsub[j,]$airport2,]
  
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
  
  lines(inter, col="black", lwd=0.8)
}


####now subsetting the dataframe by the year
#converting the YEAR column into a factor
df1$YEAR <- as.factor(df1$YEAR)
dflist <- split(df1, df1$YEAR)
names(dflist) <- c("df1994", "df2000", "df2006", "df2015")
list2env(dflist, envir = .GlobalEnv) #saving each of the dfs in list to global environment

#subsetting dataframe to those flights departing from NY area airports (JFK, LGA, EWR)
dfNY <- subset(df1, df1$ORIGIN_LOC == "New York,NY" | df1$ORIGIN_LOC == "Newark,NJ") 
dfNY <- dfNY[!is.na(dfNY$ARR_DELAY), ] #removing any NA values in the Arrival Delay column
dfNY <- mutate(dfNY, gain = DEP_DELAY - ARR_DELAY) #adding a gain column that calculates diffrence between departure and arrival delay


#running a linear regerssion, regressing distance and unique carrier on the gain
#model explains only 14.3% variation but carrier and distance are significant and do seem to have some relation to the gain
arr_NY <- lm(gain~DISTANCE+UNIQUE_CARRIER, data=dfNY)
summary(arr_NY)
# Call:
#   lm(formula = gain ~ DISTANCE + UNIQUE_CARRIER, data = dfNY)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1586.9    -8.2     2.8    11.9   123.4 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       7.52e-02   6.68e-02    1.12   0.2607    
# DISTANCE          1.18e-03   3.14e-05   37.46  < 2e-16 ***
#   UNIQUE_CARRIERAS  4.33e+00   5.46e-01    7.93  2.2e-15 ***
#   UNIQUE_CARRIERB6  5.65e-01   8.32e-02    6.79  1.1e-11 ***
#   UNIQUE_CARRIERCO -8.64e-01   7.15e-02  -12.08  < 2e-16 ***
#   UNIQUE_CARRIERDL  2.26e+00   7.37e-02   30.70  < 2e-16 ***
#   UNIQUE_CARRIEREV  4.06e+00   1.17e-01   34.60  < 2e-16 ***
#   UNIQUE_CARRIERF9 -5.17e-01   5.06e-01   -1.02   0.3071    
# UNIQUE_CARRIERFL  2.99e-01   2.43e-01    1.23   0.2180    
# UNIQUE_CARRIERHA -1.21e+01   1.20e+00  -10.09  < 2e-16 ***
#   UNIQUE_CARRIERHP -6.08e-01   2.23e-01   -2.73   0.0062 ** 
#   UNIQUE_CARRIERMQ -9.56e-01   1.20e-01   -8.00  1.3e-15 ***
#   UNIQUE_CARRIERNK  5.85e-01   3.49e-01    1.68   0.0938 .  
# UNIQUE_CARRIERNW  4.23e-02   1.22e-01    0.35   0.7290    
# UNIQUE_CARRIEROH -4.34e+00   1.39e-01  -31.24  < 2e-16 ***
#   UNIQUE_CARRIEROO  1.34e+01   2.10e+00    6.40  1.6e-10 ***
#   UNIQUE_CARRIERTW -1.31e+00   1.25e-01  -10.45  < 2e-16 ***
#   UNIQUE_CARRIERTZ -7.01e+00   4.06e-01  -17.27  < 2e-16 ***
#   UNIQUE_CARRIERUA  2.89e+00   8.07e-02   35.81  < 2e-16 ***
#   UNIQUE_CARRIERUS  8.69e-01   8.55e-02   10.16  < 2e-16 ***
#   UNIQUE_CARRIERVX  7.16e+00   2.59e-01   27.61  < 2e-16 ***
#   UNIQUE_CARRIERWN  1.24e+01   1.74e-01   71.23  < 2e-16 ***
#   UNIQUE_CARRIERXE -2.22e+00   1.06e-01  -21.01  < 2e-16 ***
#   UNIQUE_CARRIERYV  5.32e+00   2.77e-01   19.23  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.3 on 1205620 degrees of freedom
# (123 observations deleted due to missingness)
# Multiple R-squared:  0.0143,	Adjusted R-squared:  0.0143 
# F-statistic:  761 on 23 and 1205620 DF,  p-value: <2e-16

anova(arr_NY)
# Analysis of Variance Table
# 
# Response: gain
# Df   Sum Sq Mean Sq F value Pr(>F)    
# DISTANCE             1 1.43e+06 1432384    3149 <2e-16 ***
# UNIQUE_CARRIER      22 6.53e+06  296807     652 <2e-16 ***
# Residuals      1205620 5.48e+08     455                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
##significant p-values there is a difference between these groups

#grouping and summarizing a table giving the unique carrier with their average gain time
NYdelayarr <- dfNY %>%
  group_by(UNIQUE_CARRIER) %>%
  dplyr::summarise(gain = mean(gain, na.rm = TRUE))

#creating a new palette of colors that is not limited to 11 but can encompass all 23 of the carriers
#this palette is needed for ggplot
colourCount = length(unique(NYdelayarr$UNIQUE_CARRIER))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#plotting carrier vs the gain
ggplot(NYdelayarr, aes(UNIQUE_CARRIER, gain, fill=UNIQUE_CARRIER)) + ylab('gain (minutes)') + xlab('carrier') +
  geom_bar(stat="identity") +
  scale_fill_manual(values = getPalette(colourCount)) +
  ggtitle("Gain by Carrier for Outbound NYC flights") +
  theme(axis.text.x = element_text(angle = 90))

carrierdf <- dfNY %>%
  group_by(CARRIER_NAME) %>%
  dplyr::summarise(gain = mean(gain, na.rm = TRUE)) %>%
  mutate(CARRIER_NAME = substr(CARRIER_NAME, 1, 25))

ggplot(carrierdf, aes(gain, reorder(CARRIER_NAME, gain))) + 
geom_point() +
labs(title = 'Gain', x = 'Minutes', y = '')

#grouping and summarizing a table that also takes into the account the average distance travelled for each carriers in addition to the gain
NYdelaygain <- dfNY %>%
  group_by(UNIQUE_CARRIER) %>%
  dplyr::summarise(gain = mean(gain, na.rm = TRUE),
                   dist = mean(DISTANCE, na.rm = TRUE))

#plotting distance vs gain by carrier
ggplot(NYdelaygain, aes(dist, gain)) + ylab('gain (minutes)') + xlab('distance (miles)') +
  geom_point(aes(color=UNIQUE_CARRIER)) +
  geom_smooth() +
  ggtitle("Distance vs. Gain for Outbound NYC flights")
