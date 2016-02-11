library(GWmodel)

d = read.csv("D:/RProject/GWR_git/RGWR/Variable02052016_DallasFortWorthMetroplex.csv")
d = SpatialPointsDataFrame(cbind(d$XCoord, d$YCoord), d)

DM = gw.dist(dp.locat = coordinates(d))

formula1 = DISTANCE1~WHITE+HISP+BLWPOV+MEDINC+UPTOHIGH+INS+MEDICARE+MEDICAID

## specify an optimum bandwidth by cross-validation appraoch
bw1<-bw.gwr(formula1, approach = "AIC", adaptive =TRUE, data=d, kernel = "bisquare",dMat=DM)

## implement GWR
gwr.res1<-gwr.basic(formula1, data=d, bw=bw1,adaptive=TRUE, kernel = "bisquare", dMat=DM)
writeGWR(gwr.res1,fn="Model1_DallasFortWorth")

## local collinearity diagostics
gwr.cdno<-gwr.collin.diagno(formula1, data=d, bw=bw1, kernel="bisquare",adaptive=TRUE, p=2, theta=0, dMat=DM)
write.table(gwr.cdno["VIF"], "VIF.csv", sep=",", col.names = T, row.names = T)
