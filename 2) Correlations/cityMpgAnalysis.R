install.packages("tree")
library(tree)
#read csv
carsCsv = read.csv("93Cars_values.csv",sep = ';',stringsAsFactors=FALSE, dec=",")

namess = names(carsCsv)
cityMpg <-carsCsv[[namess[7]]]
# STEP 1
par(mfrow=c(3,4))
for(i in list(4,5,6,12,13,14)){
  dta <- carsCsv[c(i,7)]
  plot(dta)
  dta <- log(carsCsv[c(i,7)])
  plot(dta,xlab=paste("Log(",namess[i],")"),ylab = paste("Log(",namess[7],")"))
}

par(mfrow=c(2,4))
for(i in list(15,19,20,25)){
  dta <- log(carsCsv[c(i,7)])
  plot(dta)
  plot(dta,xlab=paste("Log(",namess[i],")"),ylab = paste("Log(",namess[7],")"))
  
}

par(mfrow=c(2,4))
for(i in list(3,10,11,26)){
  dta <- carsCsv[c(i,7)]
  boxplot(cityMpg~carsCsv[[namess[i]]],xlab=namess[i],ylab ="City MPG")
  boxplot(log(cityMpg)~carsCsv[[namess[i]]],xlab=namess[i],ylab = "Log(City MPG)")
  
}
#mini analysis with other variables

#STEP 2

#STEP 3
  
for (i in list(4,5,6,12,13,14,15,19,20,25)) { 
 
  spear<-cor.test(carsCsv[[namess[i]]],cityMpg, method="spearman")
  pvalue <- spear$p.value
  estimate <- spear$estimate
  print(namess[i])
  print(pvalue)
  print(estimate) 
}

#STEP 4
for (i in list(3,10,11,16,18,26)) {
  print(namess[i])
  print(summary(aov(cityMpg~factor(carsCsv[[namess[i]]]))))
  print(summary(aov(log(cityMpg)~factor(carsCsv[[namess[i]]]))))
}
#STEP 5

#STEP 6 

#STEP 7

#Minimum Price vs City Mpg
minimumPrice <- carsCsv$MinimumPrice
plot(minimumPrice,cityMpg)
model1 <- lm(cityMpg~minimumPrice)
summary.lm(model1)
abline(model1)

#Minimum Price vs City Mpg
minimumPrice <- carsCsv$MinimumPrice
plot(log(minimumPrice),log(cityMpg))
model1 <- lm(log(cityMpg)~log(minimumPrice))
summary.lm(model1)
abline(model1)

# Engine Size vs City Mpg
engineSize <- carsCsv$EngineSize
plot(engineSize,cityMpg)
model1 <- lm(cityMpg~engineSize)
summary.lm(model1)
abline(model1)


#Engine Size vs City Mpg

plot(log(engineSize),log(cityMpg))
model1 <- lm(log(cityMpg)~log(engineSize))
summary.lm(model1)
abline(model1)


#HorsePower vs City Mpg
horsePower<- carsCsv$Horsepower
plot(horsePower,cityMpg)
model1 <- lm(cityMpg~horsePower)
summary.lm(model1)
abline(model1)

#HorsePower vs City Mpg
horsePower<- carsCsv$Horsepower
plot(log(horsePower),log(cityMpg))
model1 <- lm(log(cityMpg)~log(horsePower))
summary.lm(model1)
abline(model1)


#Weight vs City Mpg
weight<- carsCsv$Weight
plot(weight,cityMpg)
model1 <- lm(cityMpg~weight)
summary.lm(model1)
abline(model1)

#Weight vs City Mpg
weight<- carsCsv$Horsepower
plot(log(weight),log(cityMpg))
model1 <- lm(log(cityMpg)~log(weight))
summary.lm(model1)
abline(model1)

selectedCollumns<- c("CityMPG","MinimumPrice","EngineSize","Horsepower","Weight")
data <- subset(carsCsv, select = selectedCollumns)
pairs(data,panel=panel.smooth)

model<-tree(CityMPG~.,data=data)
plot(model)
text(model)


model<- lm(cityMpg~weight + I(weight^2)+ engineSize + I(engineSize^2)+ minimumPrice+I(minimumPrice^2))
summary(model)

model2 <- update(model,~.-minimumPrice-I(minimumPrice^2))
summary(model2)


model3<- lm(cityMpg~weight*engineSize*minimumPrice + I(weight^2)+I(engineSize^2)+I(minimumPrice^2))
summary(model3)

model4 <-update(model3,~.-I(weight^2) - I(engineSize^2) - I(minimumPrice^2))
summary(model4)

par(mfrow=c(2,2))
plot(model4)


