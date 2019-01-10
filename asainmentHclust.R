attach(EastWestAirlines)
airline<-EastWestAirlines[-1]


str(airline)
summary(airline)
#airlinecluster<-hclust(airline)
#plot(airlinecluster)

airlinecluster<-hclust(airline)
plot(airlinecluster)
