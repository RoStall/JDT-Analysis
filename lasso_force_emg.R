# Purpose: Perform lasso regression (iterated, single-response) using emg features as observed
# variables and force features as response variables. These variables are calculated and stored in
# a table in a MATLAB script. More work needed there.

# Immediate goal (08/17/17) is to get a script working that will fill-in the analytic pipeline.

#### Begin
library(tibble)
library(gridExtra)
library(corrgram)
library(glmnet)
library(dplyr)
library(plyr) # load order with above might matter
setwd('~/dropbox/nasa_stretch/force_features')
data = read.csv('~/dropbox/nasa_stretch/force_features/force_emg_expl.csv')

# calculate the bilateral ratios (4 total)
data$bmg_wav =(data$lmg_airsum + data$rmg_airsum)/(data$lmg_lsrsum + data$rmg_lsrsum)
data$bmg_iemg =(data$lmg_iemg_air + data$rmg_iemg_air)/(data$lmg_iemg_lnd + data$rmg_iemg_lnd)
data$bta_wav =(data$lta_airsum +data$rta_airsum)/(data$lta_lsrsum+ data$rta_lsrsum)
data$bta_iemg = (data$lta_iemg_air + data$rta_iemg_air)/(data$lta_iemg_lnd + data$rta_iemg_lnd)

data = data[,c(1:4,5:20, 31:34, 21:30)]
# TODO: come back later and standardize the data -- see if it helps
#bilat = data.frame(bmg_wav, bmg_iemg, bta_wav, bta_iemg)
#data = 
#add_column(data, bmg_wav = bmg_wav, bmg_iemg = bmg_iemg, bta_wav = bta_wav, bta_iemg = bta_iemg, after = 20)
# split data frames by platform

sep_plat = dlply(data, "Platform", identity)

df.control = sep_plat[[1]]
df.flywheel = sep_plat[[2]]
df.iss = sep_plat[[3]]
df.shuttle = sep_plat[[4]]
df.treatmenta = sep_plat[[5]]
df.treatmentb = sep_plat[[6]]

# lets examine only ISS case for now.

# Split ISS cases
day_split_iss = dlply(df.iss,"normTime", identity)
# 1-A; 2-B; 3-C; 4-E; 5-F; 6-G

##  output all images to files

pdf("data_output.pdf", height=11, width=8.5)
grid.table(df.iss)
dev.off()
# study correlograms for various cases, see if they very based on JDT days.
png(filename = 'corrs/corrgram.png', width=13.3, height=7.5, units = "in", res = 150)
corrgram(df.iss[,5:24])
title(main = "Design Matrix Correlogram ISS All")
dev.off()

for(k in 1:6){
  png(filename = paste('corrs/correlogram_ISS_', names(day_split_iss)[k], ".png", sep="", collapse=""),
      width = 13.3, height=7.5, units ="in", res = 150)
  corrgram(day_split_iss[[k]][,5:24])
  title(main = paste("Design Matrix Correlogram ISS", names(day_split_iss)[k], collapse=""))
  dev.off()
}

# lasso loops for entire ISS data (days unseparated)
for(i in 25:34) {
  fit = glmnet(data.matrix(df.iss[,5:24]), data.matrix(df.iss[,i]), 
               lambda = cv.glmnet(data.matrix(df.iss[,5:24]), data.matrix(df.iss[,i]))$lambda.3se)
  
  png(filename = paste("lassoplots/ResponseVariable_", colnames(df.iss)[i] ,".png", sep="", collapse=""),
      res =150, width=6.5, height=6.5, units="in")             
  
  plot(fit)
  par(mar=c(4.5,4.5,1,4))
  vn=colnames(data)[5:24]
  vnat=coef(fit)
  vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
  axis(4, at=vnat,line=-.5,label=vn,las=1,tick=FALSE, cex.axis=0.5) 
  title(main = paste("Response Variable ", colnames(df.iss)[i]))
  dev.off()
  
  png(filename = paste("crossvals/CV_", colnames(df.iss)[i],".png", sep = "", collapse=""),
      res=150, width=6.5, height=6.5, units="in")
  cvfit = cv.glmnet(data.matrix(df.iss[,5:24]), data.matrix(df.iss[,i]))
  plot(cvfit)
  title(main = paste("Cross-Validation Response Variable ", colnames(df.iss)[i]),line=2)
  dev.off()
}

# lasso loops for normTime
for(k in 1:6){
  for(i in 25:34){
    fit = glmnet(data.matrix(day_split_iss[[k]][,5:24]), data.matrix(day_split_iss[[k]][,i]), 
                 lambda = cv.glmnet(data.matrix(day_split_iss[[k]][,5:24]), data.matrix(day_split_iss[[k]][,i]))$lambda.3se)
    
    png(filename = paste("lassoplots/ResponseVariable_", colnames(day_split_iss[[k]])[i],"_ISS ",names(day_split_iss)[k],".png", sep="", collapse=""),
        res =150, width=6.5, height=6.5, units="in")             
    
    plot(fit)
    par(mar=c(4.5,4.5,1,4))
    vn=colnames(data)[5:24]
    vnat=coef(fit)
    vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
    axis(4, at=vnat,line=-.5,label=vn,las=1,tick=FALSE, cex.axis=0.5) 
    title(main = paste("Response Variable ", colnames(day_split_iss[[k]])[i],"_ISS ", names(day_split_iss[k])))
    dev.off()
    
    png(filename = paste("crossvals/CV_", colnames(day_split_iss[[k]])[i],"ISS ",names(day_split_iss[k]),".png", sep = "", collapse=""),
        res=150, width=6.5, height=6.5, units="in")
    cvfit = cv.glmnet(data.matrix(day_split_iss[[k]][,5:24]), data.matrix(day_split_iss[[k]][,i]))
    plot(cvfit)
    title(main = paste("Cross-Validation Response Variable ", colnames(day_split_iss[[k]])[i], "_ISS ", names(day_split_iss[k])), line=2)
    dev.off()
    
  }
}
# Scatter charts?
#png(file = "rmg_iemg_air_vs_t1.png")

# 
#plot(data$T1,data$rmg_iemg_air,col = "blue",main = "Height & Weight Regression",
 #    abline(lm(data$rmg_iemg_air~data$T1)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")

# Save the file.
#dev.off()



# can we plot some actual data points and show a linear regression? no
# matrix of regressions?

# Above is the total system -- what if we break it up by groups -- like days, or jumps?

# finally, how about a classifier? SVM? What else?

