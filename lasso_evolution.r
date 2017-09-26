# Objective: Plot evolution of lasso coefficients (eventually want to tweak the lasso itself?)

# Method: Plot each lasso as before for response vars etc. but grey out irrelevant vars
# Take top performers and stack them in a bar chart(?) with relative percent contributions
# non-top performers lumped in as grey maybe?


library(RColorBrewer)
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

# Split ISS cases
day_split_iss = dlply(df.iss,"normTime", identity)
# 1-A; 2-B; 3-C; 4-E; 5-F; 6-G

# Now, collect lasso information for each variation and construct dataframe
dflas = data.frame()
for(k in 1:6){
  for(i in 25:34){
    fit = glmnet(data.matrix(day_split_iss[[k]][,5:24]), data.matrix(day_split_iss[[k]][,i]), 
                 lambda = cv.glmnet(data.matrix(day_split_iss[[k]][,5:24]), data.matrix(day_split_iss[[k]][,i]))$lambda.3se)
    lambda_min = tail(fit$lambda, 1)
    dflas[,k] = as.data.frame(as.matrix(coef(fit, s = lambda_min)))
  }
}

