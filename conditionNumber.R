library(GWmodel)

d = read.csv("D:/RProject/GWR_git/RGWR/SaleApartment201403.csv")
d = SpatialPointsDataFrame(cbind(d$XCoord, d$YCoord), d)

DM = gw.dist(dp.locat = coordinates(d))

formula = PRICE~AGE+COMPLEX+COMDIST+SUBWAY+MAJROAD+AGEAVE+POPDEN+BLSPOPF+FIRENO+TAX+AREA+FLOOR1

## specify an optimum bandwidth by cross-validation appraoch
bw1<-bw.gwr(formula, approach = "AIC", adaptive =TRUE, data=d, kernel = "bisquare",dMat=DM)
gwr.res1<-gwr.basic(formula, data=d, bw=bw1,adaptive=TRUE, kernel = "bisquare", dMat=DM)
#gwr.res1
gwr.cdno<-gwr.collin.diagno(formula, data=d, bw=bw1, kernel="bisquare",adaptive=TRUE, p=2, theta=0, dMat=DM)
gwr.cdno["local_CN"]

