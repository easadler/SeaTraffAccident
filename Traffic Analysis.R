setwd('/Users/evansadler/Desktop/Traffic Accidents')

data <- read.csv("Traffic_Accidents.csv")


#Remove unnecessary rows
data <- data[,-c(1:7)]
names(data) <- c("Clearance_DateTime", "Hundred_Block", "District_Sector", "Zone", "Tract","Longitude", "Latitude")

#Convert clearance datetime string to POSIXlt
data$Clearance_DateTime <- strptime(data$Clearance_DateTime, format = "%m/%d/%Y %I:%M:%S %p")


# Grab features
data$weekday <- weekdays(data$Clearance_DateTime)
data$quarter <- quarters(data$Clearance_DateTime)
data$month <- months(data$Clearance_DateTime)
data$hour <- substr(data$Clearance_DateTime, 12, 13)
data$year <- substr(data$Clearance_DateTime,1 , 4)
#Convert to factor
nms <- c("weekday", "quarter", "month", "hour", "Zone", "year") 
data[nms] <- lapply(data[nms], as.factor)

#Census tract to factor
data$Tract <- floor(as.numeric(data$Tract))
data$Tract <- as.factor(data$Tract)

#Create Intersection Variable, change from list to factor
data$Intersection <- sapply(data$Hundred_Block, function(x){grepl('/',x)})
data$Intersection[data$Intersection == TRUE] <- "Yes"
data$Intersection[data$Intersection == FALSE] <- "No"
data$Intersection <- factor(data$Intersection)


#Split intersections

split_intersections <- sapply(data$Hundred_Block, function(x){strsplit(x, "/", fixed = TRUE)})
split_intersections_logicals <- grepl('/', data$Hundred_Block)


data$Street_1 <- unname(sapply(split_intersections, function(x){ x[1]}))
data$Street_1[split_intersections_logicals] <- sapply(data$Street_1[split_intersections_logicals], function(x){ gsub(".OF ","", x)})


data$Street_1 <- gsub("^.*OF ","", data$Street_1)

data$Street_2 <- unname(sapply(split_intersections, function(x){ gsub("^([ \t\n\r\f\v]+)", "",x[2])}))

#Find close accidents

function(FALSE){
library(sp)
library(rgeos)

sp.mydata <- mydata
coordinates(sp.mydata) <- ~long+lat

class(sp.mydata)
attr(,"package")

d <- gDistance(sp.mydata, byid=T)

min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])

}


## Weather Data
weather <- read.csv('weatherdata.csv', header = T)


### Pull out Sand Point station (closest to down town)
l <- c("SEATTLE SAND PT")
weather <- subset(weather, grepl(l,weather$STATION_NAME))

#Change -9999 to NA
weather[weather == -9999] <- NA

#Grab revelent columns and remove row names
weather <- weather[, 3:6]
row.names(weather) <- NULL

#Change DATE to R datetime format to merger dataset
weather$DATE <- strptime(weather$DATE, format = "%Y%m%d")

data$DATE <- format(data$Clearance_DateTime, "%Y-%m-%d")

# Merge data sets
total <- merge(data, weather, by = "DATE")
names(total)[names(total) == 'Street_1'] <- "STNAME"






# Attempt at getting street traffic data. To many missing values


if(FALSE){
# Get street data
traffic <- read.csv("Streets.csv")

unik <- !duplicated(traffic$STNAME)  ## logical vector of unique values
traffic$unik <- unik
traffic <- traffic[traffic$unik == TRUE,]
traffic <- subset(traffic, STNAME != "")
traffic <- traffic[,c("STNAME", "AAWDT", "DOWNTOWN")]
row.names(traffic) <- NULL

ww <- merge(total, traffic, by='STNAME', all.x = TRUE)


#Merge for second street info
#traffic2 <- traffic[,1:2]
#names(traffic2) <- c("Street_2", "AAWST_2")

#final <- merge(total, traffic2, by = 'Street_2', all.x = TRUE)

}




