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
library(fitdistrplus)

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

# Probability density function of transformed features.
# convert 0's to 0.01 for lognormal attempt
df.iss.eps = df.iss
df.iss.eps[df.iss.eps == 0] = 0.01

png(filename="~/Dropbox/nasa_stretch/JDT-Analysis/graphics/c_f_bmg_iemg.png",
    height=6, width=7, units="in", res=300)
descdist(df.iss.eps$bmg_iemg)
dev.off()

descdist(df.iss.eps$bmg_wav)
descdist(df.iss.eps$bta_iemg)
descdist(df.iss.eps$bta_wav)

png(filename="~/Dropbox/nasa_stretch/JDT-Analysis/graphics/empirical_bmg_iemg.png",
    height=6, width=7, units="in", res=300)
fit.lnorm.bmg_iemg = fitdist(df.iss.eps$bmg_iemg, "lnorm")
plot(fit.lnorm.bmg_iemg)
dev.off()

fit.lnorm.bmg_wav = fitdist(df.iss.eps$bmg_wav,"lnorm")
fit.lnorm.bta_iemg = fitdist(df.iss.eps$bta_iemg,"lnorm")
fit.lnorm.bta_wav = fitdist(df.iss.esp$bta_wav, "lnorm")

# JDT bilateral ratio distributions via violin plot.
stat_img_iss_plot_mg = ggplot(df.iss, aes(x=normTime, y=bmg_iemg)) + 
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) + 
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  coord_cartesian(ylim=c(-.2,6)) +
  labs(x="Day", y="Bilateral MG Index") +
  ggtitle("Bilateral MG Statistic (iEMG) by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("stat_img_iss_plot_mg.png", stat_img_iss_plot_mg,
       path = "~/Dropbox/nasa_stretch/JDT-Analysis/graphics/")
# For stat_img_iss_plot, truncating y axis at 6 to control for outliers.

stat_wav_iss_plot_mg = ggplot(df.iss, aes(x=normTime, y=bmg_wav)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) + 
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  coord_cartesian(ylim=c(-.2,8)) +
  labs(x="Day", y="Bilateral MG Index") +
  ggtitle("Bilateral MG Statistic (Wavelet) by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("stat_wav_iss_plot_mg.png", stat_wav_iss_plot_mg,
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")

stat_img_iss_plot_ta = ggplot(df.iss, aes(x=normTime, y=bta_iemg)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  coord_cartesian(ylim=c(-0.1,1.1)) +
  labs(x="Day", y="Bilateral TA Index") +
  ggtitle("Bilateral TA Statistic (iEMG) by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("stat_img_iss_plot_ta.png", stat_img_iss_plot_ta,
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")


stat_wav_iss_plot_ta = ggplot(df.iss, aes(x=normTime, y=bta_wav)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  coord_cartesian(ylim=c(-0.05,0.65)) +
  labs(x="Day", y="Bilateral TA Index") +
  ggtitle("Bilateral TA Statistic (Wavelet) by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("stat_wav_iss_plot_ta.png", stat_wav_iss_plot_ta,
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")

# jump and day splits for ISS

# facet by day
byjump_bta_iemg = ggplot(df.iss, aes(x=as.factor(jumpNo), y=bta_iemg)) + 
  facet_wrap(~normTime, nrow=3) +
  geom_violin(trim=TRUE, color="darkred", fill="#A4A4A4", alpha=0.4) +
  theme_bw() +
  coord_cartesian(ylim=c(0.0,1.5)) +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Jump Number", y="Bilateral TA Index (iEMG)") +
  ggtitle("Bilateral TA Statistic (iEMG) by Jump") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16),
        strip.text =  element_text(size=16))

ggsave("stat_iemg_iss_byjump_bta.png", byjump_bta_iemg, height=7, width = 6, units='in',
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")

byjump_bta_wav = ggplot(df.iss, aes(x=as.factor(jumpNo), y=bta_wav)) + 
  facet_wrap(~normTime, nrow=3) +
  geom_violin(trim=TRUE, color="darkred", fill="#A4A4A4", alpha=0.4) +
  theme_bw() +
  coord_cartesian(ylim=c(0.0,0.8)) +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Jump Number", y="Bilateral TA Index (Wavelet)") +
  ggtitle("Bilateral TA Statistic (Wavelet) by Jump") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16),
        strip.text =  element_text(size=16))

ggsave("stat_wav_iss_byjump_bta.png", byjump_bta_wav, height=7, width = 6, units='in',
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")

byjump_bmg_iemg = ggplot(df.iss, aes(x=as.factor(jumpNo), y=bmg_iemg)) + 
  facet_wrap(~normTime, nrow=3) +
  geom_violin(trim=TRUE, color="darkred", fill="#A4A4A4", alpha=0.4) +
  theme_bw() +
  coord_cartesian(ylim=c(0.0,6)) +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Jump Number", y="Bilateral MG Index (iEMG)") +
  ggtitle("Bilateral MG Statistic (iEMG) by Jump") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16),
        strip.text =  element_text(size=16))

ggsave("stat_iemg_iss_byjump_bmg.png", byjump_bmg_iemg, height=7, width = 6, units='in',
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")

byjump_bmg_wav = ggplot(df.iss, aes(x=as.factor(jumpNo), y=bmg_wav)) + 
  facet_wrap(~normTime, nrow=3) +
  geom_violin(trim=TRUE, color="darkred", fill="#A4A4A4", alpha=0.4) +
  theme_bw() +
  coord_cartesian(ylim=c(0.0,18)) +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Jump Number", y="Bilateral MG Index (Wavelet)") +
  ggtitle("Bilateral MG Statistic (Wavelet) by Jump") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16),
        strip.text =  element_text(size=16))

ggsave("stat_wav_iss_byjump_bmg.png", byjump_bmg_wav, height=7, width = 6, units='in',
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")

# Force characteristics

## Force measurements alone have variance attributable to distinct nature of subjects (ie weight)
## but let us see

F1_byday = ggplot(df.iss, aes(x=normTime, y=F1)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Day", y="F1 (lbs)") +
  ggtitle("F1 by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("F1_byday.png", F1_byday,
       path="~Dropbox/nasa_stretch/JDT-analysis/graphics/")

F2_byday = ggplot(df.iss, aes(x=normTime, y=F2)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Day", y="F2 (lbs)") +
  ggtitle("F2 by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("F2_byday.png", F2_byday,
       path="~Dropbox/nasa_stretch/JDT-analysis/graphics/")

F3_byday = ggplot(df.iss, aes(x=normTime, y=F3)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Day", y="F3 (lbs)") +
  ggtitle("F3 by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("F3_byday.png", F3_byday,
       path="~Dropbox/nasa_stretch/JDT-analysis/graphics/")


F2F1_byday = ggplot(df.iss, aes(x=normTime, y=F2F1)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Day", y="F2/F1 Ratio") +
  ggtitle("F2/F1 Ratio by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("F2F1_byday.png", F2F1_byday,
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")

W1_byday = ggplot(df.iss, aes(x=normTime, y=W1)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 0.10)) +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Day", y="W1 (s)") +
  ggtitle("W1 (FWHM) by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("W1_byday.png", W2_byday,
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")


W2_byday = ggplot(df.iss, aes(x=normTime, y=W2)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 0.10)) +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Day", y="W2 (s)") +
  ggtitle("W2 (FWHM) by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("W2_byday.png", W2_byday,
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")

W3_byday = ggplot(df.iss, aes(x=normTime, y=W3)) +
  geom_violin(trim=TRUE, color="darkred", fill = '#A4A4A4', alpha=0.4) +
  theme_bw() +
  coord_cartesian(ylim=c(0, 0.10)) +
  stat_summary(fun.y=median, geom="point",fill="red", shape=23, size=2, color="red") +
  labs(x="Day", y="W3 (s)") +
  ggtitle("W3 (FWHM) by Day") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text = element_text(size=16))

ggsave("W3_byday.png", W3_byday,
       path="~/Dropbox/nasa_stretch/JDT-analysis/graphics/")


# Next, statistical significance of medians by jump -- testing, are their medians different? 
