#read csv
carsCsv = read.csv("93Cars_values.csv",sep = ';',stringsAsFactors=FALSE, dec=",")

namess = names(carsCsv)
rpm<-carsCsv[[namess[14]]]
# STEP 1
par(mfrow=c(3,4))
for(i in list(4,5,6,12,13,14)){
  dta <- carsCsv[c(i,14)]
  plot(dta)
  dta <- log(carsCsv[c(i,14)])
  plot(dta,xlab=paste("Log(",namess[i],")"),ylab = paste("Log(RPM)"))
}

par(mfrow=c(2,4))
for(i in list(15,19,20,25)){
  dta <- log(carsCsv[c(i,14)])
  plot(dta)
  plot(dta,xlab=paste("Log(",namess[i],")"),ylab = paste("Log(RPM)"))
  
}

par(mfrow=c(2,4))
for(i in list(3,10,11,26)){
  dta <- carsCsv[c(i,9)]
  boxplot(carsCsv[[namess[9]]]~carsCsv[[namess[i]]],xlab=namess[i],ylab ="RPM")
  boxplot(log(carsCsv[[namess[9]]])~carsCsv[[namess[i]]],xlab=namess[i],ylab = "Log(RPM)")
  
}

#STEP 2
#STEP 3

for (i in list(4,5,6,12,13,7,15,19,20,25)) { 
  
  spear<-cor.test(carsCsv[[namess[i]]],rpm, method="spearman")
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



