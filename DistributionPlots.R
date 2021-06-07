# download any needed libraries
library("here") # library to work with relative paths
library("readxl") # library to read excel files (better than csv)
library("ggplot2") # library for plotting 
par(mar=c(2.3,2.3,1.3,1.3)) # change params for normal-sized plotting

#dev.off(dev.list()["RStudioGD"])       # uncomment to clear all plots

cars <-read_excel(here("Dataset", "93Cars_values.xlsx")) # read excel
View(cars)      # view dataset


columns <- colnames(cars)  # save column names as a list



                        #      PLOTS      #


# Histograms

par(mfrow = c(4, 3))  # Set up a 4 x 3 plotting space
for (i in columns[4:27]) { # Loop over 24 columns 4-27 (1-3 are categorical)
  
  # Plot histogram of x
  plot(cars[[i]],main = paste("Histogram of" , i))
}



# Box Plots

par(mfrow = c(4, 3))  # Set up a 4 x 3 plotting space
for (i in columns[4:27]) { # Loop over 24 columns 4-27 (1-3 are categorical)
  
  # Plot box plot of x
  boxplot(cars[[i]],main = paste("Box plot of" , i))
}

