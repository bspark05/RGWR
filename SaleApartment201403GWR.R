library(GWmodel)

d = read.csv("D:/RProject/GWR_git/RGWR/SaleApartment201403.csv")
d = SpatialPointsDataFrame(cbind(d$XCoord, d$YCoord), d)

DM = gw.dist(dp.locat = coordinates(d))

formula1 = PRICE~AGE+COMPLEX+COMDIST+SUBWAY+MAJROAD+AGEAVE+POPDEN+BLSPOPF+FIRENO+TAX+AREA+FLOOR1
#formula2 = PRICE~AGE+COMPLEX+COMDIST+SUBWAY+MAJROAD+AGEAVE+POPDEN+BLSPOPF+FIRENO+TAX+AREA+FLOOR1+CAR2012R

## specify an optimum bandwidth by cross-validation appraoch
bw1<-bw.gwr(formula1, approach = "AIC", adaptive =TRUE, data=d, kernel = "bisquare",dMat=DM)

## implement GWR
gwr.res1<-gwr.basic(formula1, data=d, bw=bw1,adaptive=TRUE, kernel = "bisquare", dMat=DM)
writeGWR(gwr.res1)

## local collinearity diagostics
gwr.cdno<-gwr.collin.diagno(formula1, data=d, bw=bw1, kernel="bisquare",adaptive=TRUE, p=2, theta=0, dMat=DM)
gwr.cdno["local_CN"]

#write.table(gwr.cdno["local_CN"], "CN.csv", sep=",", col.names = F, row.names = T)
write.table(gwr.cdno["VIF"], "VIF.csv", sep=",", col.names = T, row.names = T)
