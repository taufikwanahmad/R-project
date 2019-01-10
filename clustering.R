attach(crime_data)
summary(crime_data)
str(crime_data)

colnames(crime_data)[1]<-"Place"
summary(crime_data)

normalizedata<-scale(crime_data[,2:5])
d<-dist(normalizedata,method = "euclidean")
#test tes test

fit<-hclust(d,method = "complete")
plot(fit)
plot(fit, hang = -1)

group<-cutree(fit,k=5)
membership<-as.matrix(group)

finalx<-data.frame(membership,crime_data)
View(finalx)
