#install.packages("DescTools") # install for posthoc test

#Cars manufacters Barplot 
library("here") # library to work with relative paths
library("readxl") # library to read excel files (better than csv)
library("DescTools") #library for posthoc test
library("olsrr") #library for stepwise variable selection

cars <-read_excel(here("Dataset", "93Cars_values.xlsx")) # read excel

dev.off(dev.list()["RStudioGD"])       # uncomment to clear all plots

par(mar=c(5,4.5,1.3,1.3)) # change params for normal-sized plotting


columns <- colnames(cars)  # save column names as a list

midPrice <- cars$MidrangePrice



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
  
  plot(cars[[columns[i]]], midPrice, xlab = columns[i], ylab = "MidrangePrice")
  plot(log(cars[[columns[i]]]), midPrice, xlab = paste("Log(",columns[i],")"), ylab = "MidrangePrice")
  plot(cars[[columns[i]]], log(midPrice), xlab = columns[i], ylab = "Log(MidrangePrice)")
  plot(log(cars[[columns[i]]]), log(midPrice), xlab = paste("Log(",columns[i],")"), ylab = "Log(MidrangePrice)")
  
}


# Box Plots of 2 variables and their log()

# There are 2 plots for each set of 2 variables.
# 1) X - Y
# 2) X - log(Y)

par(mfrow = c(3, 4))  # Set up a 3 x 4 plotting space
for (i in list(9,10,11,16,18,26)) {
  
  boxplot(midPrice~cars[[columns[i]]], xlab = columns[i], ylab = "MidrangePrice")
  boxplot(log(midPrice)~cars[[columns[i]]], xlab = columns[i], ylab = "Log(MidrangePrice)")
}



# STEP 2

# No need to order any factor levels



# STEP 3

# Check for spearman correlation


for (i in list(7,8,12,13,14,15,17,19,20,21,22,23,24,25,27)) { 
  
  print(cor.test(cars[[columns[i]]], midPrice, method="spearman"))
  
}



# STEP 4

# Use ANOVA in categorical features

for (i in list(9,10,11,16,18,26)) {
  
  print(summary(aov(midPrice~factor(cars[[columns[i]]]))))
  print(summary(aov(log(midPrice)~factor(cars[[columns[i]]]))))
  
}



# STEP 5

# Use PostHocTest to find levels that are significantly different

for (i in list(9,10,11,16,18)) {
  
  mod.var <- aov(midPrice~factor(cars[[columns[i]]]))
  mod.logvar <-aov(log(midPrice)~factor(cars[[columns[i]]]))
  print(PostHocTest(mod.var,method="hsd"))
  print(PostHocTest(mod.logvar,method="hsd"))
}

