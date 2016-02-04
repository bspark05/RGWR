library(GWmodel)

data(LondonHP)

DM<-gw.dist(dp.locat=coordinates(londonhp))

bw1<-bw.gwr(PURCHASE~FLOORSZ, data=londonhp, kernel = "gaussian",dMat=DM)
gwr.res1<-gwr.basic(PURCHASE~FLOORSZ, data=londonhp, bw=bw1,kernel = "gaussian",dMat=DM)
gwr.res1

# Saving the result
#writeGWR(gwr.res1, fn="GWRresult")

## local collinearity diagostics
gwr.cdno<-gwr.collin.diagno(PURCHASE~FLOORSZ, data=londonhp, bw=bw1, kernel = "gaussian", dMat=DM)

gwr.cdno["local_CN"]

data(LondonBorough)
nsa = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(510000,200000), scale = 10000, fill=c("transparent","black"))

## Not run: 
if(require("RColorBrewer"))
{
  mypalette<-brewer.pal(6,"Spectral")
  x11()
  spplot(gwr.res1$SDF, "FLOORSZ", key.space = "right", cex=1.5, cuts=10,
         ylim=c(155840.8,200933.9), xlim=c(503568.2,561957.5),
         main="GWR estimated coefficients for FLOORSZ with a fixed bandwidth", 
         col.regions=mypalette, sp.layout=list(nsa, londonborough))}

## End(Not run)
## Not run: 
bw2<-bw.gwr(PURCHASE~FLOORSZ,approach="aic",adaptive=TRUE, data=londonhp, 
            kernel = "gaussian", dMat=DM)
gwr.res2<-gwr.basic(PURCHASE~FLOORSZ, data=londonhp, bw=bw2,adaptive=TRUE,
                    kernel = "gaussian", dMat=DM)
gwr.res2
if(require("RColorBrewer"))
{
  x11()
  spplot(gwr.res2$SDF, "FLOORSZ", key.space = "right", cex=1.5, cuts=10,
         ylim=c(155840.8,200933.9), xlim=c(503568.2,561957.5),
         main="GWR estimated coefficients for FLOORSZ with an adaptive bandwidth", 
         col.regions=mypalette, sp.layout=list(nsa, londonborough))}

## End(Not run)
