# Objective: Plot evolution of lasso coefficients (eventually want to tweak the lasso itself?)

# Method: Plot each lasso as before for response vars etc. but grey out irrelevant vars
# Take top performers and stack them in a bar chart(?) with relative percent contributions
# non-top performers lumped in as grey maybe?


library(RColorBrewer)
library(broom)
library(glmnet)
library(plyr) # Watch for conflicts with dplyr
library(dplyr)
library(ggplot2)

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
laslist = list()
for(k in 1:6){
  for(i in 25:34){
    fit = glmnet(data.matrix(day_split_iss[[k]][,5:24]), data.matrix(day_split_iss[[k]][,i]), 
                 lambda = cv.glmnet(data.matrix(day_split_iss[[k]][,5:24]), data.matrix(day_split_iss[[k]][,i]))$lambda.3se)
    lambda_min = tail(fit$lambda, 1) # i.e., min lambda that generates largest L1 norm error in range
    df.las = tidy(coef(fit, s = lambda_min)) #tidies glmnet object into dataframe, broom function
    df.las$k = k #tracks k index
    laslist[[k]] = df.las #stores corresponding day into list of dataframes
  }
}
df.las = Reduce(function(...) merge(..., all=T), laslist)
df.las = subset(df.las, row!="(Intercept)")
df.las = arrange(df.las, k)
colnames(df.las)[1] = "Feature"

# split df.las into various k vals
#sep_las = dlply(df.las, "k", identity) # surely a 'tidyr' way of doing this? too many excerpts
# in fact see below, ddply. Want to verify that it makes sense, that I can make sense of it
df.las = ddply(df.las, .(k), mutate, PercentContribution = value/sum(abs(value))*100)

#check that sum of percent contributions adds to 100 for given group
group_by(df.las, k) %>% summarize(Perctotal = sum(abs(PercentContribution))) # should yield 6x2 of 100 <db>
#Heed warning on plyr, dplyr! lots of time wasted.

ggplot(df.las, aes(x = k, y = PercentContribution, fill = Feature)) + geom_bar(stat = 'identity')
