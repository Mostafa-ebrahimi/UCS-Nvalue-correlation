

rm=(list=ls())


A=file.choose()
Data=read.csv(A,header=TRUE)
Data1=Data[,-c(1,5)]

library(car)
scatterplotMatrix(Data1)

library("PerformanceAnalytics")
chart.Correlation(Data1, histogram=TRUE, pch=10)


#install.packages("corrplot")
library(corrplot)
corrplot(cor(Data1),type="upper",tl.col="black",tl.srt=45)


#install.packages("lattice")
library(lattice)
ord <- order.dendrogram(as.dendrogram(hclust(dist(cor(Data1)))))  
panel.corrgram <-    
    function(x, y, z, subscripts, at,  
             level = 0.9, label = FALSE, ...) 
{
    require("ellipse", quietly = TRUE)
    x <- as.numeric(x)[subscripts]   
    y <- as.numeric(y)[subscripts]     
    z <- as.numeric(z)[subscripts]   
    zcol <- level.colors(z, at = at, col.regions = grey.colors, ...)   
    for (i in seq(along = z)) {
        ell <- ellipse(z[i], level = level, npoints = 50,   
                       scale = c(.2, .2), centre = c(x[i], y[i]))
        panel.polygon(ell, col = zcol[i], border = zcol[i], ...)
    }
    if (label)  
        panel.text(x = x, y = y, lab = 100 * round(z, 2), cex = 0.8,
                   col = ifelse(z < 0, "white", "black"))   
}    

print(levelplot(cor(Data1)[ord, ord], at = do.breaks(c(-1.01, 1.01), 20),
          xlab = NULL, ylab = NULL, colorkey = list(space = "top"), 
          scales = list(x = list(rot = 90)),
          panel = panel.corrgram, label = TRUE))



###
cor(Data1)

##
names(Data1)
head(Data1)
Model=lm(Data1[,1]~Data1[,2]+Data1[,3],data=Data1)
summary(Model)
anova(Model)


confint(Model)

par(mfrow=c(2,2))
plot(Model)

















