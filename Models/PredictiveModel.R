###MULTIPLE LINEAR REGRESSION MODEL TO PREDICT ARRIVAL DELAY OF OUTBOUND FLIGHTS IN NY AREA AIRPORTS FOR 2015

library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(plyr)
library(RColorBrewer)
library(DAAG)
library(car)

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

####now subsetting the dataframe by the year
#converting the YEAR column into a factor
df1$YEAR <- as.factor(df1$YEAR)
dflist <- split(df1, df1$YEAR)
names(dflist) <- c("df1994", "df2000", "df2006", "df2015")
list2env(dflist, envir = .GlobalEnv) #saving each of the dfs in list to global environment

#arrival delay distribution
h <- hist(na.omit(df2015$ARR_DELAY), breaks=100,plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, xlab='Arrival Delay (minutes)', main='2015 Arrival Delay Distribution', col='red')

#departure delay distribution
h2 <- hist(na.omit(df2015$DEP_DELAY), breaks=100, plot=FALSE)
h2$counts=h2$counts/sum(h2$counts)
plot(h2, xlab='Departure Delay (minutes)', main='2015 Departure Delay Distribution', col='blue')

#wider distribution for arrival delays and shorter distribution for departure delays

#################################################MODELS#########################################################
#CONCLUSION: LOOKING AT THE ARRIVAL DELAY FOR THE RESPONSE VARIABLE
#variables of interest: ARRDelay
#GOAL: our response variable is ARRDelay and want to predict the amount of arrival delay for flights that depart from NY airports in 2015

#removing the NA values in the arr_delay column
arrdel2015 <- df2015[!is.na(df2015$ARR_DELAY), ]

#subset for flights leaving NY to get JFK, LGA and EWR
arrdel2015 <- arrdel2015[(arrdel2015$ORIGIN_LOC == "New York,NY") | (arrdel2015$ORIGIN_LOC == "Newark,NJ"),]


# library(polycor)
# library(corrplot)
# library(corrgram)
# corM <- hetcor(arrdel2015) #correlation matrix for mixed variable type
# corrplot(corM$correlations, type="upper")
# corrgram(corM$correlations, order=TRUE, lower.panel=panel.pts,
#          upper.panel=panel.pts, text.panel=panel.txt)


#PREDICTOR VARIABLES OF INTEREST 
# Month - Yes
# DayofMonth - Yes
# DayofWeek - Yes
# UniqueCarrier - Yes
# OriginAirportID - Yes
# DestAirportID - Yes
# CRSDepTime (the local time the plane was scheduled to depart) - Yes
# CRSArrTime (the local time the plane was scheduled to arrive) - Yes
# ArrDelay (how early/late the plane was at its final destination in minutes: our target variable)
# Distance (how far did the plane travel for the route) - Yes
# CRSElapsedTime (the scheduled difference between departure and arrival) - Yes

#If actually want to predict the delay time in minutes want to use variables that a user could potentially input to figure out the amount of delay
#should be concerned about data leakage (variables that could potentially bias the model to predicting too well)
#CRSDepTime is of interest because it is the local time the plane was scheduled to depart
#CRSArrTime is of interest because it is the local time the plane was scheduled to arrive

arrdel2015 <- subset(arrdel2015, select=c(1:3, 5:7, 10, 15, 23, 20, 17))
summary(arrdel2015) #getting summary stats for variables of interest
str(arrdel2015)
#removing unused levels from factor variables
arrdel2015$DEST <- droplevels(arrdel2015$DEST)
arrdel2015$ORIGIN <- droplevels(arrdel2015$ORIGIN)
arrdel2015$UNIQUE_CARRIER <- droplevels(arrdel2015$UNIQUE_CARRIER)
#change the times to numeric rather than keeping them as characters
arrdel2015$CRS_DEP_TIME <- as.numeric(arrdel2015$CRS_DEP_TIME)
arrdel2015$CRS_ARR_TIME <- as.numeric(arrdel2015$CRS_ARR_TIME)

#also need to bin the departure and arrival times to the nearest hour and cuts off the minutes
arrdel2015$ARR_HOUR <- trunc(arrdel2015$CRS_ARR_TIME/100) 
arrdel2015$DEP_HOUR <- trunc(arrdel2015$CRS_DEP_TIME/100)
head(arrdel2015)

#drop CRS ARR TIME and CRS DEP TIME
arrdel2015 <- subset(arrdel2015, select=c(-7, -8))


####exploratory analysis for select variables

#grouping and creating a separate table to get the mean arrival delay for each month
delay2015arr <- arrdel2015 %>%
  group_by(as.factor(MONTH)) %>%
  dplyr::summarise(delay = mean(ARR_DELAY, na.rm = TRUE))
#rename column name
colnames(delay2015arr)[1] <- 'MONTH'

#getting specific color palette that can encompass all 12 months for plotting
colourCount = length(unique(delay2015arr$MONTH))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(delay2015arr, aes(MONTH, delay, fill=MONTH)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = getPalette(colourCount)) +
  ylab('average arrival delay (minutes)') + xlab('month') +
  ggtitle("Average Arrival Delay by Month")
#February most frequent month by average delay

#creating a table for average arrival delay by day of week
week2015arr <- arrdel2015 %>%
  group_by(as.factor(DAY_OF_WEEK)) %>%
  dplyr::summarise(delay = mean(ARR_DELAY, na.rm = TRUE))

#rename the column name
colnames(week2015arr)[1] <- 'DAY_OF_WEEK'

ggplot(week2015arr, aes(DAY_OF_WEEK, delay, fill=DAY_OF_WEEK)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  ylab('average arrival delay (minutes)') + xlab('day of week') +
  ggtitle("Average Arrival Delay by Day of Week")
#seems Thursday on average has the largest average delay

#table for average arrival delay by day of month
daymonth2015arr <- arrdel2015 %>%
  group_by(as.factor(DAY_OF_MONTH)) %>%
  dplyr::summarise(delay = mean(ARR_DELAY, na.rm = TRUE))

#rename the column name
colnames(daymonth2015arr)[1] <- 'DAY_OF_MONTH'

#setting color palette
colourCount = length(unique(daymonth2015arr$DAY_OF_MONTH))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

#can change the day of week by having the actual name of the day of week
ggplot(daymonth2015arr, aes(DAY_OF_MONTH, delay, fill=DAY_OF_MONTH)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = getPalette(colourCount)) +
  ylab('average arrival delay (minutes)') + xlab('day of week') +
  ggtitle("Average Arrival Delay by Day of Month") + 
  theme(axis.text.x = element_text(angle = 45))

###########################################MODEL BUILDING

#First need to encode several of the factors as dummy variables
dummy_DEST <- model.matrix(~DEST, data=arrdel2015)[,-1]    #dummy for DEST
dummy_ORIGIN <- model.matrix(~ORIGIN, data=arrdel2015)[,-1]  #dummies for ORIGIN
dummy_CARRIER <- model.matrix(~UNIQUE_CARRIER, data=arrdel2015)[,-1] #dummies for CARRIER
ardummy <- arrdel2015
ardummy <- Filter(is.numeric,ardummy) #exclude factors from original dataset
ardummy <- cbind(ardummy,dummy_DEST,dummy_ORIGIN, dummy_CARRIER)  #add the dummies instead

######################splitting data into training and testing############################
set.seed(15071)
#splitting into 70% training and 30% testing
spl1 <- sample(nrow(ardummy), 0.7*nrow(ardummy))
AirlinesTrain <- ardummy[spl1, ]
AirlinesTest <- ardummy[-spl1, ]

#Training the model with the 70% training set
modfull <- lm(ARR_DELAY~., data=AirlinesTrain)
summary(modfull) #full summary 
# Call:
#   lm(formula = ARR_DELAY ~ ., data = AirlinesTrain)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -88.0  -22.8   -9.9    7.5 1443.9 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       123.2645    41.0674    3.00  0.00269 ** 
#   MONTH              -0.9050     0.0292  -31.02  < 2e-16 ***
#   DAY_OF_MONTH       -0.1020     0.0109   -9.37  < 2e-16 ***
#   DAY_OF_WEEK        -0.8797     0.0484  -18.17  < 2e-16 ***
#   DISTANCE           -0.0379     0.0228   -1.67  0.09574 .  
# CRS_ELAPSED_TIME   -0.2447     0.0122  -20.09  < 2e-16 ***
#   ARR_HOUR            0.0669     0.0284    2.35  0.01864 *  
#   DEP_HOUR            1.4525     0.0308   47.18  < 2e-16 ***
#   DESTACK          -105.2353    37.0127   -2.84  0.00447 ** 
#   DESTAGS          -100.6013    51.6035   -1.95  0.05124 .  
# DESTALB          -112.6675    38.0630   -2.96  0.00308 ** 
#   DESTANC           109.4687    39.7324    2.76  0.00587 ** 
#   DESTATL           -66.5194    24.2958   -2.74  0.00618 ** 
#   DESTAUS           -15.3725     7.6877   -2.00  0.04554 *  
#   DESTAVL           -78.5055    28.2369   -2.78  0.00543 ** 
#   DESTAVP          -123.6483    39.6350   -3.12  0.00181 ** 
#   DESTBDL          -120.4572    38.5703   -3.12  0.00179 ** 
#   DESTBGR          -111.6988    33.4926   -3.34  0.00085 ***
#   DESTBHM           -56.9346    22.0609   -2.58  0.00986 ** 
#   DESTBNA           -64.4496    24.2708   -2.66  0.00792 ** 
#   DESTBOS          -104.1863    37.0391   -2.81  0.00491 ** 
#   DESTBQN           -14.7090     6.6056   -2.23  0.02597 *  
#   DESTBTV          -102.9935    35.3129   -2.92  0.00354 ** 
#   DESTBUF           -99.6827    34.6640   -2.88  0.00403 ** 
#   DESTBUR            41.6662    15.0578    2.77  0.00566 ** 
#   DESTBWI          -108.9624    37.3465   -2.92  0.00353 ** 
#   DESTBZN            28.0616     7.4590    3.76  0.00017 ***
#   DESTCAE           -69.3963    27.6420   -2.51  0.01206 *  
#   DESTCAK           -90.1212    32.5302   -2.77  0.00560 ** 
#   DESTCHO           -95.8083    34.6580   -2.76  0.00570 ** 
#   DESTCHS           -75.4712    27.0362   -2.79  0.00525 ** 
#   DESTCLE           -94.8569    31.9862   -2.97  0.00302 ** 
#   DESTCLT           -79.2188    29.1658   -2.72  0.00660 ** 
#   DESTCMH           -85.9023    30.6579   -2.80  0.00508 ** 
#   DESTCVG           -78.7356    28.3057   -2.78  0.00541 ** 
#   DESTDAL           -27.7480    10.6148   -2.61  0.00895 ** 
#   DESTDAY           -79.8989    29.1425   -2.74  0.00611 ** 
#   DESTDCA          -102.8893    36.5746   -2.81  0.00491 ** 
#   DESTDEN            -7.1539     5.6867   -1.26  0.20840    
# DESTDFW           -21.9541    10.4276   -2.11  0.03526 *  
#   DESTDSM           -44.5008    18.6217   -2.39  0.01686 *  
#   DESTDTW           -80.9074    30.1124   -2.69  0.00721 ** 
#   DESTEGE            18.7374     5.2511    3.57  0.00036 ***
#   DESTEYW                 NA         NA      NA       NA    
# DESTFLL           -43.9440    17.2959   -2.54  0.01106 *  
#   DESTGRR           -74.6257    27.5851   -2.71  0.00683 ** 
#   DESTGSO           -86.0670    31.0786   -2.77  0.00562 ** 
#   DESTGSP           -76.7703    27.8059   -2.76  0.00576 ** 
#   DESTHDN            38.4087    13.9262    2.76  0.00582 ** 
#   DESTHNL           227.1971    71.3265    3.19  0.00145 ** 
#   DESTHOU           -19.8245     9.6598   -2.05  0.04014 *  
#   DESTHYA          -107.2887    37.4228   -2.87  0.00415 ** 
#   DESTIAD          -100.5912    36.2996   -2.77  0.00559 ** 
#   DESTIAH           -19.7415     9.8771   -2.00  0.04564 *  
#   DESTILM           -83.1536    30.7459   -2.70  0.00684 ** 
#   DESTIND           -71.0522    26.6157   -2.67  0.00760 ** 
#   DESTJAC            24.3153     9.2440    2.63  0.00853 ** 
#   DESTJAX           -59.6949    22.7460   -2.62  0.00868 ** 
#   DESTLAS            33.4709    10.0357    3.34  0.00085 ***
#   DESTLAX            47.6758    14.9811    3.18  0.00146 ** 
#   DESTLEX           -76.2056    28.5389   -2.67  0.00758 ** 
#   DESTLGB            37.5224    14.9436    2.51  0.01204 *  
#   DESTLIT             5.7134    18.4759    0.31  0.75714    
# DESTMCI           -40.3948    16.6510   -2.43  0.01527 *  
#   DESTMCO           -52.6669    20.1086   -2.62  0.00882 ** 
#   DESTMDW           -66.4262    25.1222   -2.64  0.00819 ** 
#   DESTMEM           -53.5167    19.9239   -2.69  0.00723 ** 
#   DESTMHT          -107.5032    36.5167   -2.94  0.00324 ** 
#   DESTMIA           -43.0216    16.8375   -2.56  0.01062 *  
#   DESTMKE           -66.8172    24.8251   -2.69  0.00711 ** 
#   DESTMSN           -64.2252    23.2686   -2.76  0.00578 ** 
#   DESTMSP           -46.6720    18.4588   -2.53  0.01146 *  
#   DESTMSY           -38.9684    14.9472   -2.61  0.00913 ** 
#   DESTMTJ            25.1994    10.0200    2.51  0.01191 *  
#   DESTMVY          -118.4258    37.7716   -3.14  0.00172 ** 
#   DESTMYR           -76.3825    28.8287   -2.65  0.00806 ** 
#   DESTOAK            55.0184    17.5413    3.14  0.00171 ** 
#   DESTOKC           -17.6521    12.1749   -1.45  0.14709    
# DESTOMA           -37.0704    15.7919   -2.35  0.01890 *  
#   DESTORD           -64.4861    24.9155   -2.59  0.00965 ** 
#   DESTORF           -99.7518    34.7745   -2.87  0.00412 ** 
#   DESTPBI           -46.3354    18.2010   -2.55  0.01090 *  
#   DESTPDX            50.1247    14.5935    3.43  0.00059 ***
#   DESTPHL          -114.3739    39.3852   -2.90  0.00368 ** 
#   DESTPHX            29.0772     8.0403    3.62  0.00030 ***
#   DESTPIT           -95.6617    33.9160   -2.82  0.00479 ** 
#   DESTPSE           -22.6599     6.3887   -3.55  0.00039 ***
#   DESTPSP            51.1918    15.9076    3.22  0.00129 ** 
#   DESTPVD          -107.1952    37.5875   -2.85  0.00435 ** 
#   DESTPWM          -102.2793    35.0788   -2.92  0.00355 ** 
#   DESTRDU           -88.5924    31.7609   -2.79  0.00528 ** 
#   DESTRIC           -97.3365    34.8648   -2.79  0.00524 ** 
#   DESTRNO            40.4438    14.1455    2.86  0.00425 ** 
#   DESTROC          -105.3396    35.5205   -2.97  0.00302 ** 
#   DESTRSW           -43.7136    17.2259   -2.54  0.01116 *  
#   DESTSAN            43.3328    14.3494    3.02  0.00253 ** 
#   DESTSAT           -19.0001     6.7108   -2.83  0.00464 ** 
#   DESTSAV           -68.8758    25.2269   -2.73  0.00633 ** 
#   DESTSBN           -78.5044    26.9289   -2.92  0.00355 ** 
#   DESTSDF           -67.0963    26.7075   -2.51  0.01200 *  
#   DESTSEA            50.5660    13.8394    3.65  0.00026 ***
#   DESTSFO            57.1155    17.4188    3.28  0.00104 ** 
#   DESTSJC            44.2363    17.3479    2.55  0.01077 *  
#   DESTSJU           -13.1777     5.9928   -2.20  0.02788 *  
#   DESTSLC            17.8338     5.0053    3.56  0.00037 ***
#   DESTSMF            51.0819    16.3144    3.13  0.00174 ** 
#   DESTSNA            40.0408    14.5083    2.76  0.00578 ** 
#   DESTSRQ           -42.8301    18.0346   -2.37  0.01756 *  
#   DESTSTL           -57.1727    21.5085   -2.66  0.00786 ** 
#   DESTSTT            -4.5129     5.7719   -0.78  0.43429    
# DESTSYR          -105.4407    36.7187   -2.87  0.00408 ** 
#   DESTTPA           -49.5403    18.7669   -2.64  0.00830 ** 
#   DESTTUL           -35.3251    46.6037   -0.76  0.44846    
# DESTTVC           -77.6044    27.4073   -2.83  0.00463 ** 
#   DESTTYS           -66.8983    27.0345   -2.47  0.01334 *  
#   DESTXNA           -40.9598    15.8500   -2.58  0.00976 ** 
#   ORIGINJFK           3.0133     0.4473    6.74  1.6e-11 ***
#   ORIGINLGA           3.4672     0.4253    8.15  3.6e-16 ***
#   UNIQUE_CARRIERAS   -9.3578     2.1390   -4.37  1.2e-05 ***
#   UNIQUE_CARRIERB6    3.1737     0.4520    7.02  2.2e-12 ***
#   UNIQUE_CARRIERDL    0.6592     0.4261    1.55  0.12182    
# UNIQUE_CARRIEREV    3.2376     0.5950    5.44  5.3e-08 ***
#   UNIQUE_CARRIERF9   12.8577     1.8446    6.97  3.2e-12 ***
#   UNIQUE_CARRIERHA   13.2607     4.0460    3.28  0.00105 ** 
#   UNIQUE_CARRIERMQ    3.8863     0.7091    5.48  4.2e-08 ***
#   UNIQUE_CARRIERNK    6.2862     1.0477    6.00  2.0e-09 ***
#   UNIQUE_CARRIEROO   -0.4425     5.5402   -0.08  0.93634    
# UNIQUE_CARRIERUA    4.0534     0.4591    8.83  < 2e-16 ***
#   UNIQUE_CARRIERUS   -1.3670     0.7038   -1.94  0.05210 .  
# UNIQUE_CARRIERVX    0.4427     0.7782    0.57  0.56942    
# UNIQUE_CARRIERWN    0.1546     0.7321    0.21  0.83277    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 44.5 on 217603 degrees of freedom
# Multiple R-squared:  0.0363,	Adjusted R-squared:  0.0358 
# F-statistic: 63.6 on 129 and 217603 DF,  p-value: <2e-16

###PREDICTIONS
Airpred <- predict(modfull, newdata = AirlinesTest)
MSE <- mean((Airpred - AirlinesTest$ARR_DELAY)^2) #1958
#can compare this model with cross-validated prediction error 

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#Training Error
rmse(resid(modfull)) #the rmse gives more weight to larger errors
#44.5 minutes

#Testing Error
rmse((AirlinesTest$ARR_DELAY - Airpred))
#44.3 <- testing error

#running a 5-fold cross-validation
kfoldCV <- cv.lm(ardummy, formula(ARR_DELAY~.), m=5)
sqrt(attr(kfoldCV, "ms"))
#cross-validated  RMSE is 47.4 minutes

#residuals of cross-validated predictions
rCV <- ardummy$ARR_DELAY - kfoldCV$cvpred
rmse(rCV)

#checking diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) 
plot(modfull)

#getting the residuals from the trained model
resid <- resid(modfull)

#want to check certain assumptions
par(mfrow=c(1,2))
plot(resid~modfull$fitted.values, pch=16, col="red", main="Residuals")
abline(0,0)
plot(st.resid~modfull$fitted.values, pch=16, col="blue", main="Standardized Residuals")
abline(0,0)
#residual plots against the fitted values indicate clustering around zero but find many outliers and extreme observations in the postive range

qqnorm(resid)
qqline(resid)
#non-normality displayed in qqplots, heavy negative skew

#To combat non-normality have to apply a transformation to the response variable (ARR_DELAY)
#cannout use boxcox becuase of negative values in response variable
#Instead using Yeo-Johnson Power Transformation which allows negative values in response


modAll <- lm(ARR_DELAY~., data=ardummy) #running a linear model on the entire dataframe

library(MASS)
# Ridge regression using independent variables
ridge <- lm.ridge(ARR_DELAY ~ ., AirlinesTrain, lambda = seq(0, .1, .001))
summary(ridge)
plot(ridge)

# Selection of constant is at endpoint.  Extend endpoint and try again
ridgec <- lm.ridge (ARR_DELAY ~ ., AirlinesTrain, lambda = seq(0, 1, .1))
plot(ridgec)
select(ridgec)

# Selection of constant is at endpoint.  Extend endpoint and try again
Atrain <- AirlinesTrain[,which(colSums(abs(AirlinesTrain)) !=0)]
ridgec <- lm.ridge (ARR_DELAY ~ ., Atrain, lambda = seq(0, 10, .01))
plot(ridgec)
select(ridgec)
#choosing lambda=4

ridge.final <- lm.ridge (ARR_DELAY ~ ., Atrain, lambda = 4)
ridge.final
summary(ridge.final)


pred.ridge <- coef(ridge.final)[1] + coef(ridge.final)[2]*test[,1] + 
  coef(ridge.final)[3]*test[,2] + coef(ridge.final)[4]*test[,4]

betas.scaled <- as.matrix(as.vector(coef()[,80]), 1, 7)
# trasform the coefficients
betas.transformed <- scaleBack.lm(scaledX, scaledY, betas.scaled)

#Ridge Regression
# library(glmnet)
# library(R1magic)
# scaledX <- subset(ardummy, select = -ARR_DELAY)
# scaledY <- ardummy["ARR_DELAY"]
# dfScaleX <- scale(scaledX)
# dfScaleY <- scale(scaledY)
# glm.fit.s <- glmnet(dfScaleX,dfScaleY, alpha=0)
# betas.scaled <- as.matrix(as.vector(coef(glm.fit.s)[,80]), 1, 7)
# # trasform the coefficients 
# betas.transformed <- scaleBack.lm(scaledX, scaledY, betas.scaled)
# # Now verify the correctness of scaled coefficients: 
# # ridge regression & coefficients
# glm.fit    <- glmnet(x=scaledX, y=scaledY, alpha=0)
# betas.fit  <- as.matrix(as.vector(coef(glm.fit)[,80]), 1, 7)

#Should do transformation on all the data instead
lambda.fm1 <- car::boxCox(modAll, family="yjPower", plotit=FALSE)
lambda.max <- lambda.fm1$x[which.max(lambda.fm1$y)] #lambda value: 0.747
m1 <- lm(yjPower(ardummy$ARR_DELAY, lambda.max) ~ ., ardummy)
summary(m1) #improvement in adjusted R-squared 0.0623 (but cannot use this as a metric to compare this model to the untransformed model as the response variables are on different scales)
transformdf <- ardummy
transformdf$trans <- yjPower(transformdf$ARR_DELAY, lambda.max)
#transformdf <- subset(transformdf, select= -c(ARR_DELAY)) #dropping ARR_DELAY

#testing on training on transformed set
#splitting into 70% training and 30% testing
ind <- sample(nrow(transformdf), 0.7*nrow(transformdf))
dfTrain <- transformdf[ind, ]
dfTest <- transformdf[-ind, ]

modTrain <- lm(trans~.-ARR_DELAY, data=dfTrain) #bulding training model with the transformed data

#Training RMSE
bttrain <- lambtransform(as.complex(modTrain$fitted.values))
rmse(dfTrain$ARR_DELAY - bttrain) #41.5

#compare the two residual plots side-by-side (Untransformed and Transformed)
par(mfrow=c(1,2))
qqnorm(resid, ylab="residuals", main="untransformed")
qqline(resid, col="red")
qqnorm(residYJ, ylab="residuals",main="yjPower transformed")
qqline(residYJ, col="red")
#can see there is an improvement, less deviation from normality in transformed plot

#predictions
YJpred <- predict(modTrain, newdata = dfTest)

#in order to accurately calculate the error need to back-transform the predictions using the function below
lambtransform <- function(data) {
  lambpower <- 1/(lambda.max)
  backtransform <- ((lambda.max*(data)) + 1)
  backtransform <- (backtransform)^lambpower
  return(backtransform)
}

#back-transforming the predictions
bt <- lambtransform(as.complex(YJpred))

#Testing RMSE (transformed model) -> but lower than training which should actually be larger than the training RMSE
rmse(dfTest$ARR_DELAY - bt) #40.8 minutes after applying back-transformation (improvement from untransformed testing error)

#running a 5-fold cross-validation
yjCV <- cv.lm(transformdf, formula(trans ~ . -ARR_DELAY), m=5, plotit = FALSE)
sqrt(attr(yjCV, "ms"))

yjcvpreds <- subset(yjCV, select=c(ARR_DELAY, cvpred))

#kfoldCV <- cv.lm(ardummy, formula(ARR_DELAY~.), m=5)

#back-transforming the predictions
btpred <- lambtransform(as.complex(yjCV$Predicted))

#back-transforming the cross-validated predictions
btCV <- lambtransform(as.complex(yjcvpreds$cvpred))

btCV <- data.frame(unlist(btCV))

#residuals of predictions
resid.pred <- yjCV$ARR_DELAY - btpred
#residuals of cross-validated predictions
resid.CV <- yjcvpreds$ARR_DELAY - btCV

#Testing RMSE (same as before but using a different method, getting predictions from CV)
rmse(resid.pred) #41.3 minutes 
#5-fold CV RMSE
rmse(resid.CV)  #57.9 minutes (worse than in the transformed model)

# library(caret)
# tc <- trainControl("cv",5,savePred=T)
# fit <- train(trans~. -ARR_DELAY,data=transformdf,method="lm",trControl=tc)
# fitpreds <- fit$pred
# #get the row indices
# fitindex <- fitpreds$rowIndex
# #extract from tranasformdf
# tdfcv <- transformdf[fitindex,]
# #back-transforming the cross-validated predictions
# btCV <- lambtransform(as.complex(fitpreds$pred))
# #residuals of cross-validated predictions
# resid.CV <- tdfcv$ARR_DELAY - btCV
# #RMSE
# rmse(resid.CV) #41.3
# 
# yjcvpreds <- data.frame(unlist(yjCV$cvpred))

