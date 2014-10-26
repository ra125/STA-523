library(rgdal)
nybb = readOGR(path.expand("/home/vis/cr173/Sta523/data/parking/nybb/"),"nybb",stringsAsFactors=FALSE)
manh = nybb[2,]

true_p=c(1,5,6,7,9,10,13,14,17,18,19,20,22,23,24,25,26,28,30,32,33,34)

names(z)[2]='x'
names(z)[3]='y'

library(e1071)

z_small=z[1:1000,2:4]

z_true=subset(z_small,(z_small$Violation.Precinct %in% true_p))

k=svm(as.factor(Violation.Precinct)~.,data=z_true,cross=1)
#plot(k,data=s)

library(raster)
r = rasterize(manh, raster(ncols=500,nrows=1000,ext=extent(bbox(manh))))

cells = which(!is.na(r[]))
crds = xyFromCell(r,cells)

w = predict(k,crds)

r[cells] = as.numeric(as.character(w))

preci = sort(unique(z_true$Violation.Precinct[1:1000]))

l=list()
for(i in seq_along(preci))
{
  l[[i]] = rasterToPolygons(r, function(x) x==preci[i], dissolve=TRUE)
  l[[i]]@polygons[[1]]@ID = as.character(preci[i])
  rownames(l[[i]]@data) = preci[i]
  colnames(l[[i]]@data) = "Precin"
}

pd = do.call(rbind, l)

writeOGR(pd, "./out", "", driver="GeoJSON")
file.rename("./out", "./district_svm.json")

cols = c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0","#f0027f","#bf5b17","#bf5b25")

# SVM Prediction
default_plot("SVM")
plot(pd, col = 1, add=TRUE)

#cols[pd@data$Precinct]

#rm(k,r,w,l,preci,pd)