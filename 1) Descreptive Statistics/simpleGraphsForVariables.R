#Cars manufacters Barplot 
library("here") # library to work with relative paths
library("readxl") # library to read excel files (better than csv)
cars <-read_excel(here("Dataset", "93Cars_values.xlsx")) # read excel

#dev.off(dev.list()["RStudioGD"])       # uncomment to clear all plots

par(mar=c(2.3,2.3,1.3,1.3)) # change params for normal-sized plotting
par(mfrow = c(4, 4))  # Set up a 4 x 4 plotting space


counts <- table(cars$Type)
barplot(counts, main="Car Distribution",xlab="Cars Type")

#Cars Price Density and hist
minimum_prices = cars$MinimumPrice
maximum_prices = cars$MaximumPrice
mid_prices = cars$MidrangePrice
prices <- c(minimum_prices, maximum_prices, mid_prices)
hist(prices,breaks=20,main="Prices of Cars",prob = TRUE, xlab="Prices",col="aquamarine2")
lines(density(prices), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(prices),col ="red",lty=2,lwd=2)

#City Mpg Density
crValues = cars$CityMPG
hist(crValues,breaks=15,main="City MPG",prob = TRUE, xlab="MPG",col="aquamarine2")
lines(density(crValues), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues),col ="red",lty=2,lwd=2)


#Highway Mpg Density
crValues = cars$HighwayMPG
hist(crValues,breaks=15,main="Highway Mpg",prob = TRUE, xlab="MPG",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)

#Engine Size Density 
crValues = cars$EngineSize
hist(crValues,breaks=15,main="Engine Size",prob = TRUE, xlab="Liters",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)


#Horse Power Density 
crValues = cars$Horsepower
hist(crValues,breaks=15,main="Horse Power",prob = TRUE, xlab="Power in  HP",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)

#RPM Density
crValues = cars$RPM
hist(crValues,breaks=15,main="RPM",prob = TRUE, xlab="Revolution per Minutes",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)

#ERM Density
crValues = cars$ERM
hist(crValues,breaks=15,main="ERM",prob = TRUE, xlab="Engine revolutions per mile",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)


#Fuel Capacity
crValues = cars$FuelTankCapacity
hist(crValues,breaks=15,main="Fuel Tank Capacity",prob = TRUE, xlab="Capacity in Gallons",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)

#Passengers Pie
crValues = cars$Passengers
pie(table(crValues),main = "Passengers Per Car",radius=3)

#Length Density
crValues = cars$Length
hist(crValues,breaks=15,main="Car Length",prob = TRUE, xlab="Length in Inches",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)

#Width Density
crValues = cars$Width
hist(crValues,breaks=15,main="Car Width",prob = TRUE, xlab="Width in Inches",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)


#Weight Density
crValues = cars$Weight
hist(crValues,breaks=15,main="Car Weights",prob = TRUE, xlab="Weights in Pounds",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)

#Total AvgMPG
crValues = cars$TotalAvgMPG
hist(crValues,breaks=15,main="Total Average MPG",prob = TRUE, xlab="Average City & Highway Miles per gallon",col="aquamarine2")
lines(density(crValues,na.rm=TRUE), # density plot
      lwd = 2, # thickness of line
      col = "gold1")
abline(v=mean(crValues, na.rm=TRUE),col ="red",lty=2,lwd=2)

#Domestic Pie
crValues = cars$Domestic
labels <- c("non-U.S. manufacturer ", "U.S. manufacturer")
pie(table(crValues),main = "U.S. Manufacters",labels,radius=3)

  