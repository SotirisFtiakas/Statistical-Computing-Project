#install.packages("DescTools") # install for posthoc test
#install.packages("olsrr") # install for stepwise variable selection

#Cars manufacters Barplot 
library("here") # library to work with relative paths
library("readxl") # library to read excel files (better than csv)
library("DescTools") #library for posthoc test
library("olsrr") #library for stepwise variable selection

cars <-read_excel(here("Dataset", "93Cars_values.xlsx")) # read excel

dev.off(dev.list()["RStudioGD"])       # uncomment to clear all plots

par(mar=c(5,4.5,1.3,1.3)) # change params for normal-sized plotting


columns <- colnames(cars)  # save column names as a list

minPrice <- cars$MinimumPrice



#      PLOTS      #



# STEP 1


# Scatter Plots of 2 variables and their log()

# There are 4 plots for each set of 2 variables.
# 1) X - Y
# 2) log(X) - Y
# 3) X - log(Y)
# 4) log(X) - log(Y)

par(mfrow = c(3, 4))  # Set up a 3 x 4 plotting space
for (i in list(7,8,12,13,14,15,17,19,20,21,22,23,24,25,27)) {
  
  plot(cars[[columns[i]]], minPrice, xlab = columns[i], ylab = "MinimumPrice")
  plot(log(cars[[columns[i]]]), minPrice, xlab = paste("Log(",columns[i],")"), ylab = "MinimumPrice")
  plot(cars[[columns[i]]], log(minPrice), xlab = columns[i], ylab = "Log(MinimumPrice)")
  plot(log(cars[[columns[i]]]), log(minPrice), xlab = paste("Log(",columns[i],")"), ylab = "Log(MinimumPrice)")
  
}


# Box Plots of 2 variables and their log()

# There are 2 plots for each set of 2 variables.
# 1) X - Y
# 2) X - log(Y)

par(mfrow = c(3, 4))  # Set up a 3 x 4 plotting space
for (i in list(9,10,11,16,18,26)) {
  
  boxplot(minPrice~cars[[columns[i]]], xlab = columns[i], ylab = "MinimumPrice")
  boxplot(log(minPrice)~cars[[columns[i]]], xlab = columns[i], ylab = "Log(MinimumPrice)")
}



# STEP 2

# No need to order any factor levels



# STEP 3

# Check for spearman correlation


for (i in list(7,8,12,13,14,15,17,19,20,21,22,23,24,25,27)) { 
  
  print(cor.test(cars[[columns[i]]], minPrice, method="spearman"))
  
}



# STEP 4

# Use ANOVA in categorical features

for (i in list(9,10,11,16,18,26)) {
  
  print(summary(aov(minPrice~factor(cars[[columns[i]]]))))
  print(summary(aov(log(minPrice)~factor(cars[[columns[i]]]))))
  
}



# STEP 5

# Use PostHocTest to find levels that are significantly different

for (i in list(9,10,11,16,18)) {
  
  mod.var <- aov(minPrice~factor(cars[[columns[i]]]))
  mod.logvar <-aov(log(minPrice)~factor(cars[[columns[i]]]))
  print(PostHocTest(mod.var,method="hsd"))
  print(PostHocTest(mod.logvar,method="hsd"))
}



# STEP 6

# Combine levels

# AirBags
airBags <- factor(cars$AirBags)
levels(airBags)
levels(airBags) [ c(2,3) ] <- "1,2"
levels(airBags)
mod.airBags <- aov(minPrice~airBags)
print(PostHocTest(mod.airBags,method="hsd"))

# DriveTrainType
driveTrainType <- factor(cars$DriveTrainType)
levels(driveTrainType)
levels(driveTrainType) [ c(2,3) ] <- "1,2"
levels(driveTrainType)
mod.driveTrainType <- aov(minPrice~driveTrainType)
print(PostHocTest(mod.driveTrainType,method="hsd"))

# NumberOfCylinders
cylinders <- factor(cars$NumberOfCylinders)
levels(cylinders)
levels(cylinders) [ c(1,2) ] <- "3,4"
levels(cylinders)
levels(cylinders) [ c(2,3) ] <- "5,6"
levels(cylinders)
mod.cylinders <- aov(minPrice~cylinders)
print(PostHocTest(mod.cylinders,method="hsd"))

# Passengers
passengers <- factor(cars$Passengers)
levels(passengers)
levels(passengers) [ c(2,3) ] <- "4,5"
levels(passengers)
levels(passengers) [ c(3,4,5) ] <- "6,7,8"
levels(passengers)
mod.passengers <- aov(minPrice~passengers)
print(PostHocTest(mod.passengers,method="hsd"))



# STEP 7

# Build Models

# Model 1 = CityMPG vs MinimumPrice
cityMPG = cars$CityMPG
model1 <- lm(minPrice~cityMPG+airBags+driveTrainType+cylinders+passengers+airBags:driveTrainType+airBags:passengers)
             #+driveTrainType+cylinders+passengers+airBags:driveTrainType+airBags:cylinders+airBags:passengers+driveTrainType:cylinders+driveTrainType:passengers+cylinders:passengers)
summary.lm(model1)

model.dim<-ols_step_both_p(model1)
model.dim

summary(model.dim$model)

plot(cityMPG,minPrice,ylab="MinimumPrice",xlab="CityMPG")
abline(22.8074,-0.4610)
abline(22.8074+4.7288,-0.4610,col='red')
