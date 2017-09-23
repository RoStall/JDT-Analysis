### Demo for presentation materials orienting audience to the plots.

library(glmnet)
library(RColorBrewer)
library(tibble)
library(corrgram)
library(glmnet)
library(dplyr)
library(plyr) # load order with above might matter

setwd("~/dropbox/NASA_stretch/Force_features/")

# correlogram
png(filename = 'mtcars_corrgram.png', width=13.3, height=7.5, units = "in", res = 150)
corrgram(mtcars, upper.panel = panel.pts)
title(main = "Gas Mileage Data and Factors")
dev.off()

palette(brewer.pal(12,"Paired"))

mod = glmnet(as.matrix(mtcars[-1]), mtcars[,1])
vn = colnames(mtcars[2:11])

# lasso plot
png(filename = 'mtcars_lasso.png', width = 8.85, height = 6.2, units= "in", res = 150)
plot(mod, lwd =2)
vnat = coef(mod)
vnat = vnat[-1, ncol(vnat)]
axis(4, at=vnat, line=-.5, las=1, tick=FALSE, cex.axis=0.7, label = vn)
title(main = "Gas Mileage Factors\n")
par(xpd=F)
legend('topleft', legend=vn, col=1:length(vn), lty=1, cex = .6, lwd=2)
mtext("Degrees of Freedom", side=3, line=0)
dev.off()

# cv plot
png(filename = 'mtcars_cvlasso.png', width = 7.27, height = 6.09, units = "in", res = 150)
cvfit = cv.glmnet(as.matrix(mtcars[-1]), mtcars[,1])
plot(cvfit)
title(main = "Cross-Validation Gas Mileage ", line=2)
dev.off()
