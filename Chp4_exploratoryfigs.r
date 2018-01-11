# Purpose: Produce figures for exploratory plots for chapter 4 of dissertation.

# Want violin plots for norm time, ISS comparisons, normtime Jump #'s per day, forces


#### Begin
library(RColorBrewer)
library(tibble)
library(gridExtra)
library(corrgram)
library(ggplot2)
library(plyr)
library(dplyr)
 # remember load order (plyr first)

setwd('~/dropbox/nasa_stretch/force_features')
data = read.csv('~/dropbox/nasa_stretch/force_features/force_emg_expl.csv')

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

# Split ISS cases
day_split_iss = dlply(df.iss,"normTime", identity)
# 1-A; 2-B; 3-C; 4-E; 5-F; 6-G

stat_img_iss_plot = ggplot(df.iss, aes(x=normTime, y=bmg_iemg)) + 
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) + 
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  coord_cartesian(ylim=c(-.2,6)) +
  labs(x="Day", y="Bilateral MG iEMG") +
  ggtitle("Bilateral MG Statistic (iEMG) by Day") +
  theme(plot.title = element_text(hjust = 0.5))
  
# truncating y axis at 6 to control for outliers.

# jump and day splits for ISS


