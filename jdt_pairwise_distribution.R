# Next, statistical significance of medians by jump -- testing, are these statistically different
# crude test only


library(tibble)
library(gridExtra)
library(ggplot2)
library(plyr)
library(dplyr)
library(stats)

data = read.csv('~/dropbox/nasa_stretch/force_features/force_emg_expl.csv')

# this script outputs graphics. Build directory

dir.create("~/dropbox/nasa_stretch/jdt-analysis/graphics") 
# creates folder. Warns if already there.
graphicpath = "~/dropbox/nasa_stretch/jdt-analysis/graphics"

# calculate the bilateral ratios (4 total)
data$bmg_wav =(data$lmg_airsum + data$rmg_airsum)/(data$lmg_lsrsum + data$rmg_lsrsum)
data$bmg_iemg =(data$lmg_iemg_air + data$rmg_iemg_air)/(data$lmg_iemg_lnd + data$rmg_iemg_lnd)
data$bta_wav =(data$lta_airsum +data$rta_airsum)/(data$lta_lsrsum+ data$rta_lsrsum)
data$bta_iemg = (data$lta_iemg_air + data$rta_iemg_air)/(data$lta_iemg_lnd + data$rta_iemg_lnd)

data = data[,c(1:4,5:20, 31:34, 21:30)]

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
# First, some subjects have more than 3 jumps in a given experiment day. Remove these.
df.iss = filter(df.iss, jumpNo <= 3)
# Split ISS cases
day_split_iss = dlply(df.iss,"normTime", identity)
# 1-A; 2-B; 3-C; 4-E; 5-F; 6-G

# loop over variables, with normTime

pwtest = list()
for (i in 5:24) {
  pwtest[[k]] = pairwise.wilcox.test(df.iss[[i]], df.iss$normTime, p.adjust.method = "BH")
  k = k + 1
}
