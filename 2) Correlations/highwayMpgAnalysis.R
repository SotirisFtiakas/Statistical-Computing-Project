#read csv
carsCsv = read.csv("93Cars_values.csv",sep = ';',stringsAsFactors=FALSE, dec=",")

namess = names(carsCsv)
highwayMpg <-carsCsv[[namess[8]]]
# STEP 1
par(mfrow=c(3,4))
for(i in list(4,5,6,12,13,14)){
  dta <- carsCsv[c(i,8)]
  plot(dta)
  dta <- log(carsCsv[c(i,8)])
  plot(dta,xlab=paste("Log(",namess[i],")"),ylab = "Log(Highway MPG)")
}

par(mfrow=c(2,4))
for(i in list(15,19,20,25)){
  dta <- log(carsCsv[c(i,8)])
  plot(dta)
  plot(dta,xlab=paste("Log(",namess[i],")"),ylab = "Log(Highway MPG)")
  
}

par(mfrow=c(2,4))
for(i in list(3,10,11,26)){
  dta <- carsCsv[c(i,8)]
  boxplot(carsCsv[[namess[8]]]~carsCsv[[namess[i]]],xlab=namess[i],ylab ="Highway MPG")
  boxplot(log(carsCsv[[namess[8]]])~carsCsv[[namess[i]]],xlab=namess[i],ylab = "Log(Highway MPG)")
  
}
#STEP @
#STEP 3

for (i in list(4,5,6,12,13,14,15,19,20,25)) { 
  
  spear<-cor.test(carsCsv[[namess[i]]],highwayMpg, method="spearman")
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