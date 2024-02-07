##################
###USACE Report September 2023
#
#
###author: mack daddy white
#######################################
###goal: generate figures requested by jenn
# Snook CPUE (with smoothers) - DONE
# Bass CPUE (with smoothers) - DONE
# Nonnative CPUE (with smoothers) - DONE
# Sunfish CPUE (with smoothers) - DONE
# Sunfish Biomass (with smoothers) - 

library(lubridate)
library(scales)
library(imputeTS)
library(pracma)
library(car)
library(viridis)
library(reshape2)
library(vegan)
library(tidyquant)
library(glmmTMB)
library(DHARMa)
library(stringr)
library(MuMIn)
library(ggeffects)
library(tidyverse)

# Data Manipulation and Checking ------------------------------------------

dat <- read_csv("data/MAPmaster_yrs1thru19_speciesnames_CLEAN.csv")
#View(dat)
glimpse(dat)
unique(dat$common_name) #all seems accounted for

# helps later on
dat$common_name<-str_replace(dat$common_name, fixed(" "), "_") #first get rid of spaces in species names, it causes issues
unique(dat$common_name)

#formatting date how we like it
dat$s_date=as.Date(paste(dat$s.yr,dat$s.mo,dat$s.day,sep="-")) #fix sample date 
dat$s_date

#factoring of bout and site - dont want as numberical
dat$BOUT<-as.factor(paste(dat$SITE,dat$BOUT,sep="_")) #rename to unique bout name
dat$SITE<-as.factor(dat$SITE) #make site a factor
glimpse(dat)

#make sure all invasives are properly listed
invasives <- dat |> 
  filter(status == "invasive")
unique(invasives$common_name)

# generating catch numbers for species of interest (in this case keeps zeros important for proper CPUE calculations)
catch_all <- dat |>
  filter(CATCHCODE== 'S'|CATCHCODE==  'C'| CATCHCODE == 'NA', #remove irrelevant catch codes (keep NA for empty bouts)
         SITE == 8 |SITE == 9|SITE==10|SITE==11|SITE==13) |> 
  mutate(catchnumber_snook =                                          
           ifelse(common_name!='Snook',
                  CATCHNUMBER * 0, CATCHNUMBER),
         catchnumber_bass =                                          
           ifelse(common_name!='Largemouth_bass',
                  CATCHNUMBER * 0, CATCHNUMBER),
         catchnumber_invasive = 
           ifelse(status!='invasive',
                  CATCHNUMBER * 0, CATCHNUMBER),
         catchnumber_sf =                                          
                  ifelse(common_name!='Redear'& common_name!='Spotted_sunfish'&common_name!='Bluegill'&
                           common_name!='Dollar_sunfish'&common_name!='Warmouth'&common_name!='Sunfishes'&
                           common_name!='Bluespotted_sunfish',CATCHNUMBER * 0, CATCHNUMBER))
           
summary(catch_all)
glimpse(catch_all)

# CPUE calculations -------------------------------------------------------

#general cpue calculations
cpue_summary <- catch_all |>
  group_by(s_date, s.yr, s.mo, SITE, BOUT) |> 
  dplyr::reframe(distance=mean(Distance), #should just be whatever the distance shocked on a given bout is
                   snook_100m=(sum(catchnumber_snook)/distance)*100,
                   bass_100m=(sum(catchnumber_bass)/distance)*100,
                   invasive_100m=(sum(catchnumber_invasive)/distance)*100,
                   sf_100m=(sum(catchnumber_sf)/distance)*100)

#monthly mean cpue for each species of interest
monthly_cpue <- cpue_summary |> 
  group_by(s.yr, s.mo) |> 
  dplyr::reframe(snook_cpue_mean = mean(snook_100m),
                    log_snook_cpue_mean = log(snook_cpue_mean+1),
                    bass_cpue_mean = mean(bass_100m),
                    log_bass_cpue_mean = log(bass_cpue_mean+1),
                    invasive_cpue_mean = mean(invasive_100m),
                    log_invasive_cpue_mean = log(invasive_cpue_mean+1),
                    sf_cpue_mean = mean(sf_100m),
                    log_sf_cpue_mean = log(sf_cpue_mean+1)) |> 
      na.omit() |> 
      mutate(s.day = 01,
             s_date = ymd(paste(s.yr,s.mo,s.day, sep = "-")))

glimpse(monthly_cpue)

# CPUE Monthly Mean Figures -----------------------------------------------

### Common Snook CPUE w smoother
snook_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(snook_cpue_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Common Snook CPUE (n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
snook_plot

# ggsave(filename='plots/snook_cpue_yrs1thru19revised.png', plot = snook_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Largemouth Bass CPUE w smoother
bass_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(bass_cpue_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Florida Largemouth Bass CPUE (n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
bass_plot

# ggsave(filename='plots/bass_cpue_yrs1thru19revised.png', plot = bass_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Invasive Species CPUE w smoother
invasives_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(invasive_cpue_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Invasive Fish Species CPUE (n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
invasives_plot

# ggsave(filename='plots/invasives_cpue_yrs1thru19revised.png', plot = invasives_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Sunfish Species CPUE w smoother
sf_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(sf_cpue_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Lepomis spp. CPUE (n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
sf_plot

# ggsave(filename='plots/sunfish_cpue_yrs1thru19_revised.png', plot = sf_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# Daily CPUE Figures ------------------------------------------------------

### Common Snook CPUE w smoother
snook_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(snook_cpue_mean), group = 1)) +
      geom_line(color = "black", linewidth = 0.5) +
      geom_point(size = 1.0) +
      geom_smooth() +
      labs(x = "Date", 
           y = "Common Snook CPUE (n/100m)") +
      theme(panel.grid.major = element_blank(), 
            # panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            # axis.text = element_text(size=12,face="bold", color = "black"),
            # axis.title = element_text(size=12,face="bold", color = "black"), 
            axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
            panel.grid.minor = element_blank(),legend.position = "none") + 
      scale_x_date(date_labels = "%Y",breaks ='1 year')
snook_plot

# ggsave(filename='plots/snook_cpue_yrs1thru19.png', plot = snook_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Largemouth Bass CPUE w smoother
bass_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(bass_cpue_mean), group = 1)) +
      geom_line(color = "black", linewidth = 0.5) +
      geom_point(size = 1.0) +
      geom_smooth() +
      labs(x = "Date", 
           y = "Florida Largemouth Bass CPUE (n/100m)") +
      theme(panel.grid.major = element_blank(), 
            # panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            # axis.text = element_text(size=12,face="bold", color = "black"),
            # axis.title = element_text(size=12,face="bold", color = "black"), 
            axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
            panel.grid.minor = element_blank(),legend.position = "none") + 
      scale_x_date(date_labels = "%Y",breaks ='1 year')
bass_plot

# ggsave(filename='plots/bass_cpue_yrs1thru19.png', plot = bass_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Invasive Species CPUE w smoother
invasives_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(invasive_cpue_mean), group = 1)) +
      geom_line(color = "black", linewidth = 0.5) +
      geom_point(size = 1.0) +
      geom_smooth() +
      labs(x = "Date", 
           y = "Invasive Fish Species CPUE (n/100m)") +
      theme(panel.grid.major = element_blank(), 
            # panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            # axis.text = element_text(size=12,face="bold", color = "black"),
            # axis.title = element_text(size=12,face="bold", color = "black"), 
            axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
            panel.grid.minor = element_blank(),legend.position = "none") + 
      scale_x_date(date_labels = "%Y",breaks ='1 year')
invasives_plot

# ggsave(filename='plots/invasives_cpue_yrs1thru19.png', plot = invasives_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Sunfish Species CPUE w smoother
sf_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(sf_cpue_mean), group = 1)) +
      geom_line(color = "black", linewidth = 0.5) +
      geom_point(size = 1.0) +
      geom_smooth() +
      labs(x = "Date", 
           y = "Lepomis spp. CPUE (n/100m)") +
      theme(panel.grid.major = element_blank(), 
            # panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
            # axis.text = element_text(size=12,face="bold", color = "black"),
            # axis.title = element_text(size=12,face="bold", color = "black"), 
            axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
            panel.grid.minor = element_blank(),legend.position = "none") + 
      scale_x_date(date_labels = "%Y",breaks ='1 year')
sf_plot

# ggsave(filename='plots/sunfish_cpue_yrs1thru19.png', plot = sf_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# LOG TRANSFORMED FIGURES -------------------------------------------------

### Common Snook CPUE w smoother
LOG_snook_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(log_snook_cpue_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Common Snook Log+1(CPUE; n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
LOG_snook_plot

# ggsave(filename='plots/LOG_snook_cpue_yrs1thru19.png', plot = LOG_snook_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Largemouth Bass CPUE w smoother
LOG_bass_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(log_bass_cpue_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Florida Largemouth Bass Log+1(CPUE; n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
LOG_bass_plot

# ggsave(filename='plots/LOG_bass_cpue_yrs1thru19.png', plot = LOG_bass_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Invasive Species CPUE w smoother
LOG_invasives_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(log_invasive_cpue_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Invasive Fish Species Log+1(CPUE; n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
LOG_invasives_plot

# ggsave(filename='plots/LOG_invasives_cpue_yrs1thru19.png', plot = LOG_invasives_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### Sunfish Species CPUE w smoother
LOG_sf_plot <- ggplot(monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(log_sf_cpue_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Lepomis spp. Log+1(CPUE; n/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
LOG_sf_plot

# ggsave(filename='plots/LOG_sunfish_cpue_yrs1thru19.png', plot = LOG_sf_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

# SUNFISH BIOMASS ---------------------------------------------------------

Sunfish <- dat |> 
  filter(CATCHCODE== 'S'|CATCHCODE==  'C'| CATCHCODE == 'NA', #remove irrelavent catchcodes (keep NA for empty bouts)
                       SITE == 8 |SITE == 9|SITE==10|SITE==11|SITE==13) |> 
  mutate(catchnumber_sf =                                          
           ifelse(common_name!='Redear'& common_name!='Spotted_sunfish'&common_name!='Bluegill'&
                    common_name!='Dollar_sunfish'&common_name!='Warmouth'&common_name!='Sunfishes'&
                    common_name!='Bluespotted_sunfish',CATCHNUMBER * 0, CATCHNUMBER)) # remove extra clmns

summary(Sunfish)
glimpse(Sunfish)

#Calculate L/W relationship#########################

#for Bluegill
ind_metrics_bg <- Sunfish |>
  filter(common_name =='Bluegill' &!is.na(SL) & !is.na(WEIGHT)) |> 
  mutate(wwt_g = WEIGHT*1000)
bgallometry<-lm(log10(wwt_g)~log10(as.numeric(SL)),data=ind_metrics_bg) #find LW relationship
ind_metrics_bg$resid<-resid(bgallometry) #identify residuals
SD2_bg<-2*sd(resid(bgallometry))  # identify 2 sd of resuduals
ind_metrics_bg$Outs<-ifelse(abs(ind_metrics_bg$resid) >SD2_bg, T, F) #remove resids with sd over 2 (outliers)
ind_metrics_bg_out_rm<-ind_metrics_bg%>% filter(Outs == FALSE)
bgallometry_out_rm<-lm(log10(wwt_g)~log10(as.numeric(SL)),data=ind_metrics_bg_out_rm) # rerun with trimmed data
summary(bgallometry_out_rm)
confint(bgallometry_out_rm, level = 0.95)
length(ind_metrics_bg_out_rm$SL)

#Coefs
#a = 10^-1.62583 = 0.02366846, b = 3.18042 , r2= 0.953 , n= 540 -> #updated Sept 2023
summary(ind_metrics_bg_out_rm)

#Redear
ind_metrics_re <- Sunfish |>
  filter(common_name =='Redear' &!is.na(SL) & !is.na(WEIGHT)) |>
  mutate(wwt_g = WEIGHT*1000)
reallometry<-lm(log10(wwt_g)~log10(SL),data=ind_metrics_re) 
ind_metrics_re$resid<-resid(reallometry)
SD2_re<-2*sd(resid(reallometry))
ind_metrics_re$Outs<-ifelse(abs(ind_metrics_re$resid) >SD2_re, T, F)
ind_metrics_re_out_rm<-ind_metrics_re%>% filter(Outs == FALSE)
reallometry_out_rm<-lm(log10(wwt_g)~log10(SL),data=ind_metrics_re_out_rm) 
summary(reallometry_out_rm)
confint(reallometry_out_rm, level = 0.95)
length(ind_metrics_re_out_rm$SL)
#Coefs
#a = 10^-1.61974 = 0.02400269, b = 3.15721, r2= 0.9661 , n = 899 -> #updated Sept 2023

#Spotted sf
ind_metrics_ss <- Sunfish |>
  filter(common_name =='Spotted_sunfish' &!is.na(SL) & !is.na(WEIGHT)) |>
  mutate(wwt_g = WEIGHT*1000)
ssallometry<-lm(log10(wwt_g)~log10(SL),data=ind_metrics_ss) 
ind_metrics_ss$resid<-resid(ssallometry)
SD2_ss<-2*sd(resid(ssallometry))
ind_metrics_ss$Outs<-ifelse(abs(ind_metrics_ss$resid) >SD2_ss, T, F)
ind_metrics_ss_out_rm<-ind_metrics_ss%>% filter(Outs == FALSE)
ssallometry_out_rm<-lm(log10(wwt_g)~log10(SL),data=ind_metrics_ss_out_rm) 
summary(ssallometry_out_rm)
confint(ssallometry_out_rm, level = 0.95)
length(ind_metrics_ss_out_rm$SL)
#Coefs
#a = 10^-1.37079 = 0.04258043, b = 2.98313, r2= 0.9475  ,n = 466 -> updated Sept 2023

#Dollar SF
ind_metrics_ds <- Sunfish |> 
  filter(common_name =='Dollar_sunfish' &!is.na(SL) & !is.na(WEIGHT)) |>
  mutate(wwt_g = WEIGHT*1000)
dsallometry<-lm(log10(wwt_g)~log10(SL),data=ind_metrics_ds) 
ind_metrics_ds$resid<-resid(dsallometry)
SD2_ds<-2*sd(resid(dsallometry))
ind_metrics_ds$Outs<-ifelse(abs(ind_metrics_ds$resid) >SD2_ds, T, F)
ind_metrics_ds_out_rm<-ind_metrics_ds%>% filter(Outs == FALSE)
dsallometry_out_rm<-lm(log10(wwt_g)~log10(SL),data=ind_metrics_ds_out_rm) 
summary(dsallometry_out_rm)
confint(dsallometry_out_rm, level = 0.95)
length(ind_metrics_ds_out_rm$SL)
#Coefs
#a = 10^-1.2841 = 0.05198763, b = 2.7825, r2= 0.7592, n = 111 -> updated Sept 2023
# Klassen et al 0.1111/jai.12406 has better r2 - data below - still does!
#a = 0.0250, b = 3.12, r2= 0.976 ,n = 93

#Warmouth
ind_metrics_wm <- Sunfish |>
  filter(common_name =='Warmouth' &!is.na(SL) & !is.na(WEIGHT)) |>
  mutate(wwt_g = WEIGHT*1000)
wmallometry<-lm(log10(wwt_g)~log10(SL),data=ind_metrics_wm) 
ind_metrics_wm$resid<-resid(wmallometry)
SD2_wm<-2*sd(resid(wmallometry))
ind_metrics_wm$Outs<-ifelse(abs(ind_metrics_wm$resid) >SD2_wm, T, F)
ind_metrics_wm_out_rm<-ind_metrics_wm%>% filter(Outs == FALSE)
wmallometry_out_rm<-lm(log10(wwt_g)~log10(SL),data=ind_metrics_wm_out_rm) 
summary(wmallometry_out_rm)
confint(wmallometry_out_rm, level = 0.95)
length(ind_metrics_wm_out_rm$SL)
#a = 10^-1.55447= 0.02789523, b = 3.15226 , r2=  0.9621, n = 62 -> updated Sept 2023

#Bluespotted_sunfish
# Klassen et al https://doi.org/10.1111/jai.12406 has better r2 - data below
#a = 0.0299, b = 2.99, r2= 0.975 ,n = 0.975

# Bluegill - #a = 10^-1.62583 = 0.02366846, b = 3.18042 , r2= 0.953 , n= 540 -> #updated Sept 2023
# Redear - #a = 10^-1.61974 = 0.02400269, b = 3.15721, r2= 0.9661 , n = 899 -> #updated Sept 2023
# Spotted Sunfish - #a = 10^-1.37079 = 0.04258043, b = 2.98313, r2= 0.9475  ,n = 466 -> updated Sept 2023
# Dollar Sunfish - #a = 0.0250, b = 3.12, r2= 0.976 ,n = 93 -> Klassen et al
# Warmouth - #a = 10^-1.55447= 0.02789523, b = 3.15226 , r2=  0.9621, n = 62 -> updated Sept 2023
# Bluespotted Sunfish - #a = 0.0299, b = 2.99, r2= 0.975 ,n = 0.975 -> Klassen et al

#Apply WL relationships to sf with only SL values, convert kg to g while your at it
SunfishWLbm<-Sunfish%>%filter(common_name=='Redear') |>
  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT), WEIGHT*1000, 0.02400269*SL^3.15721))
SunfishWLbm1<-Sunfish%>%filter(common_name=='Bluegill') |>
  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT), WEIGHT*1000, 0.02366846*SL^3.18042))
SunfishWLbm2<-Sunfish%>%filter(common_name=='Spotted_sunfish') |>
  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT), WEIGHT*1000,0.04258043*SL^2.98313))
SunfishWLbm3<-Sunfish%>%filter(common_name=='Dollar_sunfish') |>
  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT), WEIGHT*1000,0.0250*SL^3.12))
SunfishWLbm4<-Sunfish%>%filter(common_name=='Warmouth') |>
  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT),  WEIGHT*1000,0.02789523*SL^3.15226))
SunfishWLbm5<-Sunfish%>%filter(common_name=='Bluespotted_sunfish') |>
  mutate(wetbiomass_g = ifelse(!is.na(WEIGHT),  WEIGHT*1000,0.0299*SL^2.99))

#bindrows of all species
Allobind<-bind_rows(SunfishWLbm,SunfishWLbm1,SunfishWLbm2,SunfishWLbm3,SunfishWLbm4,SunfishWLbm5)

#select dataset without sf species
Allorm<-Sunfish%>% 
  filter(common_name!='Redear'&common_name!='Bluegill'&common_name!='Spotted_sunfish'&common_name!='Dollar_sunfish'&common_name!='Warmouth'&common_name!='Bluespotted_sunfish') 

#mean SL of each species
summary(Allobind)
Allobind |>
  filter(!is.na(SL)) |>
  group_by(common_name) |>
  summarize(n=n(),SL_m=mean(SL,na.rm = T), SL_se=sd(SL,na.rm = T)/n()^0.5)


#bind sf biomass data with map data to have right number of 0s
SunfishWLbm6<-bind_rows(Allobind,Allorm)

# boxplot(SL~s_date,data=SunfishWLbm2)
# boxplot(SL~s_date,data=SunfishWLbm3)
# boxplot(SL~s_date,data=SunfishWLbm)

# impute missing data -----------------------------------------------------

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))# function for mean imputation

#impute based on grouping factors, for this we will impute based on the mean of the same species in the same hydroyear and season
#we could do it by site or bout if we have individuals of same to get a mean for each level of grouping factor.

sf_mean_imp_spec <- SunfishWLbm6 |>  
  group_by(HYDROYEAR, common_name) |> 
  mutate(wetbiomass_g = na_if(wetbiomass_g,0)*catchnumber_sf,WEIGHT=na_if(WEIGHT,0)*catchnumber_sf, #multiply by catchnumber because some rows include several individuals shocked
         wetbiomass_g_imp = impute.mean(wetbiomass_g)*catchnumber_sf) |> 
  filter(wetbiomass_g_imp <= 1000) #6 observations which made it through the outlier test somehow, just got rid of
glimpse(sf_mean_imp_spec)
summary(sf_mean_imp_spec)

sf_mean_imp_forviewing <-data.frame(sf_mean_imp_spec) |> 
  select(HYDROYEAR,s_date,catchnumber_sf,SL,TL,SITE,BOUT,SEASON,species,latin_name,common_name,
         wetbiomass_g,wetbiomass_g_imp) |> 
  filter(catchnumber_sf!=0)

summary(sf_mean_imp_forviewing) #make sure imputations didn't generate crazy-ass weights

#CPUE by date/bout for ind sf species
sf_bout_sum_sp <- sf_mean_imp_spec |>
  group_by(s_date,s.yr,s.mo,SITE,BOUT,common_name) |>
  summarise(distance1=mean(Distance),
            catch = sum(as.numeric(catchnumber_sf), na.rm = T),
            wt_biomass_g=sum(wetbiomass_g_imp),
            cpue_biomass_100m=sum(wt_biomass_g/distance1*100),
            cpue_100m=sum((catch/distance1)*100),
            temp_C=mean(as.numeric(TempC),na.rm=T),
            sal=mean(as.numeric(SALINITY),na.rm=T))
glimpse(sf_bout_sum_sp)
summary(sf_bout_sum_sp)

# sf_bout_sum <- sf_mean_imp_spec |>
#       group_by(s_date, s.yr, s.mo, SITE, BOUT) |> 
#       dplyr::reframe(distance=mean(Distance), #should just be whatever the distance shocked on a given bout is
#                      catch = sum(as.numeric(catchnumber_sf), na.rm = T),
#                      wt_biomass_g = sum(wetbiomass_g_imp),
#                      cpue_biomass_100m = (sum(wt_biomass_g)/distance),
#                      cpue_100m = (sum(catch)/distance)*100)

# # CPUE by date/bout for pooled sf species
sf_bout_sum <- sf_mean_imp_spec |>
  group_by(s_date,s.yr,s.mo,SITE,BOUT) |>
  summarise(distance1=mean(Distance),
            catch = sum(as.numeric(catchnumber_sf), na.rm = T),
            wt_biomass_g=sum(wetbiomass_g_imp),
            cpue_biomass_100m=sum(wt_biomass_g/distance1*100),
            cpue_100m=sum((catch/distance1)*100),
            temp_C=mean(as.numeric(TempC),na.rm=T),
            sal=mean(as.numeric(SALINITY),na.rm=T))
glimpse(sf_bout_sum)
summary(sf_bout_sum)

#monthly mean cpue for each species of interest
# sf_biomass_cpue_monthly_mean <- sf_bout_sum |> 
#       group_by(s.yr, s.mo) |> 
#       dplyr::reframe(cpue_biomass_100m_mean = mean(cpue_biomass_100m)) |> 
#       na.omit() |> 
#       mutate(s.day = 01,
#              s_date = ymd(paste(s.yr,s.mo,s.day, sep = "-")))
# 
# glimpse(monthly_cpue)

# #monthly mean cpue for pooled sf species
sf_biomass_cpue_monthly_mean <- sf_bout_sum |>
  group_by(s.yr, s.mo) |>
  dplyr:: summarise(cpue_biomass_100m_mean = mean(cpue_biomass_100m),
                    LOG_cpue_biomass_100m_mean = log(cpue_biomass_100m_mean),
                    LOGplus1_cpue_biomass_100m_mean = 1+log(cpue_biomass_100m_mean))

summary(sf_biomass_cpue_monthly_mean)

#weird na in dates, so getting rid of it
sf_biomass_cpue_monthly_mean_clean <- na.omit(sf_biomass_cpue_monthly_mean)

#adding date back in for plotting
sf_biomass_monthly_cpue <- sf_biomass_cpue_monthly_mean_clean |>
  mutate(s.day = 01,
         s_date = ymd(paste(s.yr,s.mo,s.day, sep = "-")))

glimpse(sf_biomass_monthly_cpue)

### sunfish biomass/100m
sf_biomass_plot <- ggplot(sf_biomass_monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(cpue_biomass_100m_mean ), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Lepomis spp. Biomass CPUE (g/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
sf_biomass_plot

# ggsave(filename='plots/sf_biomass_cpue_yrs1thru19revised.png', plot = sf_biomass_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### sunfish log biomass/100m
LOG_sf_biomass_plot <- ggplot(sf_biomass_monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(LOG_cpue_biomass_100m_mean ), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Lepomis spp. Biomass Log(CPUE; g/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
LOG_sf_biomass_plot

# ggsave(filename='plots/LOG_sf_biomass_cpue_yrs1thru19.png', plot = LOG_sf_biomass_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)

### sunfish log + 1 biomass/100m
LOGplus1_sf_biomass_plot <- ggplot(sf_biomass_monthly_cpue, aes(x=as.Date(s_date), y = as.numeric(LOGplus1_cpue_biomass_100m_mean), group = 1)) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(size = 1.0) +
  geom_smooth() +
  labs(x = "Date", 
       y = "Lepomis spp. Biomass Log+1(CPUE; g/100m)") +
  theme(panel.grid.major = element_blank(), 
        # panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # plot.title = element_text(hjust = 0.5, size=14, face="bold", color = "black"),
        # axis.text = element_text(size=12,face="bold", color = "black"),
        # axis.title = element_text(size=12,face="bold", color = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1., vjust = 1.1),axis.text = element_text(color="black"),
        panel.grid.minor = element_blank(),legend.position = "none") + 
  scale_x_date(date_labels = "%Y",breaks ='1 year')
LOGplus1_sf_biomass_plot

# ggsave(filename='plots/LOGplus1_sf_biomass_cpue_yrs1thru19.png', plot = LOGplus1_sf_biomass_plot,
#        scale = 2.5,
#        width = 10,
#        height = 5,
#        units = c("cm"),
#        dpi = 300)
