data <- read.csv("Traffic_Accidents.csv", header = TRUE)


#Remove unnecessary rows
data <- data[,-c(1:7)]
names(data) <- c("Clearance_DateTime", "Hundred_Block", "District_Sector", "Zone", "Tract","Longitude", "Latitude")

#Convert clearance datetime string to POSIXlt
data$test <- strptime(data$Clearance_DateTime, format = "%m/%d/%Y %I:%M:%S %p")
data$Clearance_DateTime <- data$test

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

