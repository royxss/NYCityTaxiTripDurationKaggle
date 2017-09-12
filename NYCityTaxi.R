setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\NY City Taxi Trip Duration")
rm(list=ls())
seedVal = 17869
#load("NYCityTaxi.RData")
#save.image("NYCityTaxi.RData")

sample_sub <- read.csv2("sample_submission.csv", header = TRUE, sep = ',')
test <- read.csv2("test.csv", header = TRUE, sep = ',')
train <- read.csv2("train.csv", header = TRUE, sep = ',')

# test and train columns does not match
testcols <- names(test)
traincols <- names(train)
setdiff(traincols, testcols)
# "dropoff_datetime" "trip_duration"
# dropoff_datetime doesn't exist in test.

# Check meaningless values if any
apply(train, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" |  x == "-" | 
                                           x == " ")))

# Check structure
str(train)
# Lots of datatype inconsistencies
options(digits = 20)
train$vendor_id <- as.factor(train$vendor_id)
train$pickup_datetime <- strptime(train$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
train$dropoff_datetime <- strptime(train$dropoff_datetime, format = "%Y-%m-%d %H:%M:%S")
train$pickup_longitude <- as.numeric(as.character(train$pickup_longitude))
train$pickup_latitude <- as.numeric(as.character(train$pickup_latitude))
train$dropoff_longitude <- as.numeric(as.character(train$dropoff_longitude))
train$dropoff_latitude <- as.numeric(as.character(train$dropoff_latitude))

#Test data
test$vendor_id <- as.factor(test$vendor_id)
test$pickup_datetime <- strptime(test$pickup_datetime, format = "%Y-%m-%d %H:%M:%S")
test$pickup_longitude <- as.numeric(as.character(test$pickup_longitude))
test$pickup_latitude <- as.numeric(as.character(test$pickup_latitude))
test$dropoff_longitude <- as.numeric(as.character(test$dropoff_longitude))
test$dropoff_latitude <- as.numeric(as.character(test$dropoff_latitude))

library(Amelia)
missmap <- missmap(train)
summary(missmap)

# Split pickup dates
library(lubridate)
train$pickup_yr <- year(train[,'pickup_datetime'])
train$pickup_month <- month(train[,'pickup_datetime'])
train$pickup_hour <- hour(train[,'pickup_datetime'])
train$pickup_wday <- wday(train[,'pickup_datetime'])
train$pickup_day <- day(train[,'pickup_datetime'])

# test Data
test$pickup_yr <- year(test[,'pickup_datetime'])
test$pickup_month <- month(test[,'pickup_datetime'])
test$pickup_hour <- hour(test[,'pickup_datetime'])
test$pickup_wday <- wday(test[,'pickup_datetime'])
test$pickup_day <- day(test[,'pickup_datetime'])

# Check duplicate id
stopifnot(length(unique(train$id)) == nrow(train))

# Check year. if only 2016 remove column
unique(train$pickup_yr)
# only 2016 data. exclude it below

# update datatype of wday and hour to factor
train$pickup_hour <- as.factor(train$pickup_hour)
train$pickup_month <- as.factor(train$pickup_month)
train$pickup_wday <- as.factor(train$pickup_wday)
train$pickup_day <- as.factor(train$pickup_day)

# test data
test$pickup_hour <- as.factor(test$pickup_hour)
test$pickup_month <- as.factor(test$pickup_month)
test$pickup_wday <- as.factor(test$pickup_wday)
test$pickup_day <- as.factor(test$pickup_day)

# Use microsoft bing tile system to key lat and long
# Converts a point from latitude/longitude WGS-84 coordinates (in degrees) 
# into pixel XY coordinates at a specified level of detail.

MinLatitude = -85.05112878
MaxLatitude = 85.05112878
MinLongitude = -180
MaxLongitude = 180
levelOfDetail = 22

clip <- function(n, minValue, maxValue){
  return(min(max(n, minValue), maxValue))}

latLongToTileXY <- function(latitude, longitude, levelOfDetail = 22){

  lat <- clip(latitude, MinLatitude, MaxLatitude)
  lon <- clip(longitude, MinLongitude, MaxLongitude)
  
  x <- (lon + 180) / 360
  sinLatitude <- sin(lat * pi / 180)
  y <- 0.5 - log((1 + sinLatitude) / (1 - sinLatitude)) / (4 * pi)
  
  mapsize <- bitwShiftL(256 , levelOfDetail)
  pixelXLon <- clip(x * mapsize + 0.5, 0, mapsize - 1)
  pixelYLat <- clip(y * mapsize + 0.5, 0, mapsize - 1)
  tileXLon <- as.integer(pixelXLon / 256)
  tileYLat <- as.integer(pixelYLat / 256) 
  
  return(paste0(tileXLon,'|',tileYLat))
}

# Convert lat long to Bing tile for better approximation
train$pickupLonLat <- mapply(latLongToTileXY, train$pickup_latitude, train$pickup_longitude)
train$dropoffLonLat <- mapply(latLongToTileXY, train$dropoff_latitude, train$dropoff_longitude)

# test data
test$pickupLonLat <- mapply(latLongToTileXY, test$pickup_latitude, test$pickup_longitude)
test$dropoffLonLat <- mapply(latLongToTileXY, test$dropoff_latitude, test$dropoff_longitude)

# Create new columns out of the above
library('tidyr')
train <- separate(train, pickupLonLat, 
                 c('pickupLonTile', 'pickupLatTile'),
                 sep="\\|",
                 remove=FALSE)
train <- separate(train, dropoffLonLat, 
                  c('dropoffLonTile', 'dropoffLatTile'),
                  sep="\\|",
                  remove=FALSE)
# test data
test <- separate(test, pickupLonLat, 
                 c('pickupLonTile', 'pickupLatTile'),
                 sep="\\|",
                 remove=FALSE)
test <- separate(test, dropoffLonLat, 
                 c('dropoffLonTile', 'dropoffLatTile'),
                 sep="\\|",
                 remove=FALSE)

# Convert new column into integers
train$pickupLonTile <- as.integer(train$pickupLonTile) 
train$pickupLatTile <- as.integer(train$pickupLatTile)
train$dropoffLonTile <- as.integer(train$dropoffLonTile)
train$dropoffLatTile <- as.integer(train$dropoffLatTile)

# test data
test$pickupLonTile <- as.integer(test$pickupLonTile) 
test$pickupLatTile <- as.integer(test$pickupLatTile)
test$dropoffLonTile <- as.integer(test$dropoffLonTile)
test$dropoffLatTile <- as.integer(test$dropoffLatTile)

# Create a tile euclidian dist.
tileToEuc <- function(pickupLonTile, pickupLatTile, dropoffLonTile, dropoffLatTile){
  d <- sqrt(((pickupLonTile - dropoffLonTile)^2) + ((pickupLatTile - dropoffLatTile)^2))
  return(d)
}

train$mercatileDist <- mapply(tileToEuc, train$pickupLonTile, 
                              train$pickupLatTile, train$dropoffLonTile, train$dropoffLatTile)
test$mercatileDist <- mapply(tileToEuc, test$pickupLonTile, 
                             test$pickupLatTile, test$dropoffLonTile, test$dropoffLatTile)

########################### filter records ##################

# Check statistics
library(ggplot2)
set.seed(seedVal)

# Remove extreme trip durations
nrow(train[train$trip_duration < 60,])*100/nrow(train) #0.5% trips are less tha 60 secs
train <- train[train$trip_duration > 60,]   #Min trip time is 60 secs

nrow(train[train$trip_duration > 4*60*60,])*100/nrow(train) #0.14% trips are mor than 4 hrs
train <- train[train$trip_duration < 4*60*60,]   #Max trip time is 12 hrs
summary(train$trip_duration)

#VendorId
cor(as.numeric(train$vendor_id), train$trip_duration)
#No correlation. Must remove it

#Passengercount
nrow(train[train$passenger_count > 6,])*100/nrow(train) #0.0001%
#Only 3 records and mercatile dist is 0. Let's remove it
train <- train[train$passenger_count <= 6,]
cor(train$passenger_count, train$trip_duration)
# almost 0 correlation. Must remove it

# Check mercatile distance to trip duration
ggplot(train, aes(trip_duration, mercatileDist)) + geom_point()
Incorr <- train[train$mercatileDist < 1 & train$trip_duration > 60*60, 'id']
# same location for more than an hour? Let's remove it
train <- subset(train, !(train$id %in% Incorr))

# Store and forward flag
cor(ifelse(train$store_and_fwd_flag == 'N', 0, 1), train$trip_duration)
# does not contribute

# Apply scaling function
train$mercatileDist <- scale(train$mercatileDist)
train$pickupLatTile <- scale(train$pickupLatTile)
train$pickupLonTile <- scale(train$pickupLonTile)
train$dropoffLatTile <- scale(train$dropoffLatTile)
train$dropoffLonTile <- scale(train$dropoffLonTile)
train$passenger_count <- scale(train$passenger_count)
train$trip_duration <- log(train$trip_duration + 1)

# test data
test$mercatileDist <- scale(test$mercatileDist)
test$pickupLatTile <- scale(test$pickupLatTile)
test$pickupLonTile <- scale(test$pickupLonTile)
test$dropoffLatTile <- scale(test$dropoffLatTile)
test$dropoffLonTile <- scale(test$dropoffLonTile)
test$passenger_count <- scale(test$passenger_count)

################################### Weather Data ########################################
# Add weather data
url = 'https://www.wunderground.com/history/airport/KNYC/2016/1/1/CustomHistory.html?dayend=31&monthend=7&yearend=2016&req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&MR=1'

library("readxl")
wd <- read_xlsx("weatherdata.xlsx", col_names = TRUE)
wd[is.na(wd$Events),'Events'] <- 'NoEvent'
wd <- wd[, c("Month", "Day", "AvgTemp", "AvgVisib", "Events")]
wd$Events <- trimws(wd$Events)

wd$Month <- as.factor(wd$Month)
wd$Day <- as.factor(wd$Day)

wd$AvgVisib <- as.numeric(wd$AvgVisib)
wd[is.na(wd$AvgVisib),'AvgVisib'] <- 0
wd[wd$AvgVisib == 0,'AvgVisib'] <- ceiling(mean(wd$AvgVisib))

# Check meaningless values if any
apply(wd, 2, function(x) length(which(x == "" | is.na(x) | x == "NA" |  x == "-" | 
                                        x == " ")))

library("dummies")
wd <- cbind(wd, dummy(wd$Events))
colnames(wd) <- c("Month", "Day", "AvgTemp", "AvgVisib", "Events", "Fog_Rain",
                  "Fog_Rain_Snow", "Fog_Snow", "NoEvent", "Rain", "Rain_Snow", "Snow")
wd <- wd[,c("Month", "Day", "AvgTemp", "AvgVisib", "Fog_Rain",
            "Fog_Rain_Snow", "Fog_Snow", "NoEvent", "Rain", "Rain_Snow", "Snow")]

# wd$Fog_Rain <- as.factor(wd$Fog_Rain)
# wd$Fog_Rain_Snow <- as.factor(wd$Fog_Rain_Snow)
# wd$Fog_Snow <- as.factor(wd$Fog_Snow)
# wd$NoEvent <- as.factor(wd$NoEvent)
# wd$Rain <- as.factor(wd$Rain)
# wd$Rain_Snow <- as.factor(wd$Rain_Snow)
# wd$Snow <- as.factor(wd$Snow)

wd$Fog_Rain <- as.logical(wd$Fog_Rain)
wd$Fog_Rain_Snow <- as.logical(wd$Fog_Rain_Snow)
wd$Fog_Snow <- as.logical(wd$Fog_Snow)
wd$NoEvent <- as.logical(wd$NoEvent)
wd$Rain <- as.logical(wd$Rain)
wd$Rain_Snow <- as.logical(wd$Rain_Snow)
wd$Snow <- as.logical(wd$Snow)

train <- merge(x = train, y = wd, by.x = c("pickup_month", "pickup_day"), 
           by.y = c("Month", "Day"), all.x = TRUE)
test <- merge(x = test, y = wd, by.x = c("pickup_month", "pickup_day"), 
               by.y = c("Month", "Day"), all.x = TRUE)

train$AvgTemp <- scale(train$AvgTemp)
train$AvgVisib <- scale(train$AvgVisib)
test$AvgTemp <- scale(test$AvgTemp)
test$AvgVisib <- scale(test$AvgVisib)

##** saved

################################### End Weather Data ########################################

# exclude columns not required.
wdList <- c("AvgTemp", "AvgVisib", "Fog_Rain", "Fog_Rain_Snow", 
            "Fog_Snow", "NoEvent", "Rain", "Rain_Snow", "Snow")

excludeList <- c("id", "dropoff_datetime", "pickup_datetime", "pickup_yr", "pickup_day",
                 "pickup_latitude", "pickup_longitude" ,"dropoff_latitude","dropoff_longitude",
                 "pickupLonLat", "dropoffLonLat", "store_and_fwd_flag", 
                 "passenger_count", "vendor_id") 
                 #"pickupLonTile", "pickupLatTile", "dropoffLonTile", "dropoffLatTile")
yVar <- "trip_duration"
includeList <- names(train)[!names(train) %in% c(excludeList, yVar)]

str(train[,c(includeList)])

#############################################################

# #h2o frame will not take logical or factors ops. Let's make it integers
# transformList <- c("pickup_month", "pickup_hour", "pickup_wday",
#                    "Fog_Rain","Fog_Rain_Snow","Fog_Snow","NoEvent","Rain","Rain_Snow","Snow")
# for (i in (transformList)){
#   train[,i] <- as.integer(train[,i])
# }

# stratefied sampling
library(caret)
set.seed(seedVal)
trainPct <- .8
testPct <- 1 - trainPct
inTrain <- createDataPartition(y = train[,c(yVar)], p = trainPct, list = FALSE)
traindata <- train[inTrain, ]
testdata <- train[-inTrain, ]
stopifnot(nrow(traindata) + nrow(testdata) == nrow(train))

# ########################### Deep learning ##################################
# library(h2o)
# 
# h2o.init(nthreads=-1, max_mem_size="2G")
# h2o.removeAll()
# 
# tr_frame <- as.h2o(as.matrix(traindata[,c(includeList, yVar)]))
# #tr_frame[1:2,]
# validation_frame <- as.h2o(as.matrix(testdata[,c(includeList, yVar)]))
# 
# # # Create model matrix
# # mtrain <- model.matrix(~.+0,data = traindata[,c(includeList, yVar)]) 
# # mval <- model.matrix(~.+0,data = testdata[,c(includeList, yVar)])
# # tr_frame <- as.h2o(mtrain)
# # validation_frame <- as.h2o(mval)
# # 
# # # Create training frames
# # a <- traindata[1:100,c(includeList, yVar)]
# # h2o.init()
# # tr_frame <- as.h2o(as.matrix(a))
# # # provided column type matrix is unknown
# # # h2o frame will not take logical or factors ops. Let's make it integers
# # tlist <- c("Fog_Rain","Fog_Rain_Snow","Fog_Snow","NoEvent","Rain","Rain_Snow","Snow")
# # for (i in (tlist)){
# #   a[,i] <- as.integer(a[,i])
# # }
# 
# 
# modelDL <- h2o.deeplearning(  training_frame=tr_frame, 
#                               validation_frame=validation_frame,
#                               x=1:17,
#                               y=18,
#                               nfolds = 5,
#                               epochs=10,
#                               variable_importances=T,
#                               stopping_metric="MSE",
#                               seed = seedVal
# )
# summary(modelDL)
# 
# # Transform test matrix
# for (i in (transformList)){
#   test[,i] <- as.integer(test[,i])
# }
# 
# # Create test matrix
# mtestFrame <- as.h2o(as.matrix(test[,includeList]))
# 
# # Predict on new data 
# newTestPredScaled <- predict(modelDL, newdata = mtestFrame)
# newTestPred <- exp(newTestPredScaled) -1
# 
# h2o.shutdown(prompt = FALSE)


########################### XGboost ##################################
library("xgboost")
# Prepare matrix
mtrain <- model.matrix(~.+0,data = traindata[,includeList]) 
mtest <- model.matrix(~.+0,data = testdata[,includeList])
dtrain <- xgb.DMatrix(data = mtrain,label = traindata[,yVar])
dtest <- xgb.DMatrix(data = mtest,label=testdata[,yVar])

#0.3, 10, 12, 0.8, 1 = 0.399 best one
#0.3, 10, 1, 0.8, 1 = 0.377
# 0.3, 10, 12, 0.798, 0.967 = 
params <- list(booster = "gbtree", objective = "reg:linear", 
               eta=0.3, gamma=0, max_depth=10, min_child_weight=1,
               seed = seedVal, nthread = 4,
               subsample=0.8, colsample_bytree=1)

# Calculate the best round
xgbcv <- xgb.cv(params = params, data = dtrain, nrounds = 100, nfold = 5, 
                 showsd = T, stratified = T, print_every_n = 20, 
                 early_stopping_rounds = 20, maximize = F)

# Train model with the best round
xgbmodel <- xgb.train (params = params, data = dtrain, nrounds = 150, #xgbcv$best_iteration, 
                       watchlist = list(val=dtest,train=dtrain), 
                       print_every_n = 10, early_stopping_rounds = 10, 
                       maximize = F , eval_metric = "rmse")

summary(xgbmodel)

# Run prediction
# Create model matrix
mtestNew <- model.matrix(~.+0,data = test[,includeList])
dtestNew <- xgb.DMatrix(data = mtestNew)

# Predict new instances
newTestPredScaled <- predict(xgbmodel, dtestNew)
newTestPred <- exp(newTestPredScaled) -1

################################ Create o/p File #################################

# Append values to test
test$id <- as.character(test$id)
submission <- test[,'id']
submission <- data.frame(cbind(submission, as.integer(newTestPred)))
names(submission) <- c("id","trip_duration")

# Export to file
expData <- submission[,c("id","trip_duration")]
write.table(expData, file = "Output.csv", quote = FALSE, row.names=FALSE, sep=",")
