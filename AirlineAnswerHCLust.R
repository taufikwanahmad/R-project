library(cluster)
suppressPackageStartupMessages(library(dendextend))
library(factoextra)
library(ggplot2)

library(fpc)
library(NbClust)

attach(EastWestAirlines)
airline<-EastWestAirlines[-1]
str(airline)

airline$cc1_miles = ifelse(airline$cc1_miles==1,2500,
                              ifelse(airline$cc1_miles==2,7500,
                                     ifelse(airline$cc1_miles==3,17500,
                                            ifelse(airline$cc1_miles==4,32500,
                                                   ifelse(airline$cc1_miles==5,50000,0)))))

airline$cc2_miles = ifelse(airline$cc2_miles==1,2500,
                              ifelse(airline$cc2_miles==2,7500,
                                     ifelse(airline$cc2_miles==3,17500,
                                            ifelse(airline$cc2_miles==4,32500,
                                                   ifelse(airline$cc2_miles==5,50000,0)))))

airline$cc3_miles = ifelse(airline$cc3_miles==1,2500,
                              ifelse(airline$cc3_miles==2,7500,
                                     ifelse(airline$cc3_miles==3,17500,
                                            ifelse(airline$cc3_miles==4,32500,
                                                   ifelse(airline$cc3_miles==5,50000,0)))))



data = scale(airline)
d <- dist(airline, method = "euclidean") 


fit <- hclust(d, method="ward.D2")
fit <- as.dendrogram(fit)
cd = color_branches(fit,k=3) #Coloured dendrogram branches
plot(cd)



groups <- cutree(fit, k=3) 
table(groups)

g1 = aggregate(airline,list(groups),median)
data.frame(Cluster=g1[,1],Freq=as.vector(table(groups)),g1)
