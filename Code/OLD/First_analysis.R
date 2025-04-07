library(tidyverse)

# Plot Dry wt, LOI, MHg, THg, %MHg
# Plot together to understand relationships 
# ID drivers of each


Data_ALL <- read_csv("Formatted_Data/AllLakes_AllVariables_final_2021-05-19.csv")

# Calculate Percent SMHG
Data_ALL$Percent_SMHG <- Data_ALL$SMHG_ng_g/Data_ALL$STHG_ng_g


# Isotope data fairly normal, could use MVM lasso
hist(Data_ALL$d202_Avg)
hist(Data_ALL$D199_Avg)
hist(Data_ALL$D200_Avg)
hist(Data_ALL$D201_Avg)
hist(Data_ALL$D204_Avg)

hist((Data_ALL$LOI_PERCENT)) # beta
hist(log10(Data_ALL$LOI_PERCENT)) # log-normal
sum(Data_ALL$LOI_PERCENT==0, na.rm=T) # 0

hist((Data_ALL$DRY_WEIGHT_PERCENT)) # beta
hist(log10(Data_ALL$DRY_WEIGHT_PERCENT+1)) # log-normal
sum(Data_ALL$DRY_WEIGHT_PERCENT==0, na.rm=T) # 1 obs is 0

hist(log10(Data_ALL$STHG_ng_g)) # log-normal
hist(log10(Data_ALL$SMHG_ng_g)) # log-normal
hist(log10(Data_ALL$Percent_SMHG)) # beta or log-normal







sum(!is.na(Data_ALL$LOI_PERCENT )) # 1092
sum(!is.na(Data_ALL$Lake_Area_HA )) # 1124
sum(!is.na(Data_ALL$Omernik_II )) # 1124

names(Data_ALL)


# DRY_WEIGHT_PERCENT, LOI_PERCENT, SMHG_ng_g, STHG_ng_g, Percent_SMHG


# LOI and dry weight Should be highly correlated. ####
# Increase in dry weight means sediment holds less water
# LOI is estimate of C. Higher C means more prone to hold water
# So LOI and dry weight should be inversely related

# Dry wt vs LOI
Data_ALL %>% ggplot(aes(x=LOI_PERCENT, y=DRY_WEIGHT_PERCENT)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20))
ggsave("Figures/LOI_DryWt_MHg_THg/DryWt_vs_LOI.png", width=8, height=6)


# Dry wt vs LOI: Linear on Log-Log scale (power-law)
Data_ALL %>% ggplot(aes(x=LOI_PERCENT, y=DRY_WEIGHT_PERCENT+1)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/DryWt_vs_LOI_loglog.png", width=8, height=6)

# Note: DRY_WEIGHT_PERCENT has 1 zero value, but LOI_PERCENT does not have any zeroes


# THg vs. LOI%
Data_ALL %>% ggplot(aes(x=LOI_PERCENT, y= STHG_ng_g)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/THg_vs_LOI_loglog.png", width=8, height=6)


# MeHg should have relationship to LOI and THg. ####

# MeHg vs. LOI%: log-log relationship (power-law)
Data_ALL %>% ggplot(aes(x=LOI_PERCENT, y= SMHG_ng_g)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/MeHg_vs_LOI_loglog.png", width=8, height=6)

# % MeHg vs. LOI%: log-log relationship (power-law)
Data_ALL %>% ggplot(aes(x=LOI_PERCENT, y= Percent_SMHG)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/PercentMeHg_vs_LOI_loglog.png", width=8, height=6)


# MeHg vs. THg: log-log relationship (power-law)
Data_ALL %>% ggplot(aes(x=STHG_ng_g, y= SMHG_ng_g)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/MeHg_vs_THg_loglog.png", width=8, height=6)

# % MeHg vs. THg: log-log relationship (power-law)
Data_ALL %>% ggplot(aes(x=STHG_ng_g, y= Percent_SMHG)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/PercentMeHg_vs_THg_loglog.png", width=8, height=6)

# % MeHg vs. MeHg: log-log relationship (power-law)
Data_ALL %>% ggplot(aes(x=SMHG_ng_g, y= Percent_SMHG)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/PercentMeHg_vs_MeHg_loglog.png", width=8, height=6)


# Rest of figures - of interest?

# THg vs. DRY_WEIGHT_PERCENT
Data_ALL %>% ggplot(aes(x=DRY_WEIGHT_PERCENT+1, y= STHG_ng_g)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/THg_vs_DryWt_loglog.png", width=8, height=6)


# MeHg vs. DRY_WEIGHT_PERCENT: log-log relationship (power-law)
Data_ALL %>% ggplot(aes(x=DRY_WEIGHT_PERCENT+1, y= SMHG_ng_g)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/MeHg_vs_DryWt_loglog.png", width=8, height=6)

# % MeHg vs. DRY_WEIGHT_PERCENT: log-log relationship (power-law)
Data_ALL %>% ggplot(aes(x=DRY_WEIGHT_PERCENT+1, y= Percent_SMHG)) +
  geom_point() +
  theme_bw() +
  theme(text=element_text(size=20)) +
  scale_x_log10() +
  scale_y_log10()
ggsave("Figures/LOI_DryWt_MHg_THg/PercentMeHg_vs_DryWt_loglog.png", width=8, height=6)



sum(Data_ALL$DRY_WEIGHT_PERCENT==0, na.rm = T)





# H0: %MHg and and LOI will be higher in urban areas ####

# Percent Urban 2006 catchment
# PctUrbOp2006Cat	% of catchment area classified as developed, open space land use (NLCD 2006 class 21)	
# PctUrbLo2006Cat	% of catchment area classified as developed, low-intensity land use (NLCD 2006 class 22)	
# PctUrbMd2006Cat	% of catchment area classified as developed, medium-intensity land use (NLCD 2006 class 23)	
# PctUrbHi2006Cat	% of catchment area classified as developed, high-intensity land use (NLCD 2006 class 24)

# Percent Urban 2006 watershed
# "PctUrbOp2006Ws"                                                                          
# "PctUrbLo2006Ws"                                                                          
# "PctUrbMd2006Ws"                                                                          
# "PctUrbHi2006Ws" 

# Percent urban 2011 catchment
# "PctUrbOp2011Cat"                                                                         
# "PctUrbLo2011Cat"                                                                         
# "PctUrbMd2011Cat"                                                                         
# "PctUrbHi2011Cat"  

# Percent Urban 2011 watershed
# "PctUrbOp2011Ws"                                                                          
# "PctUrbLo2011Ws"                                                                          
# "PctUrbMd2011Ws"                                                                          
# "PctUrbHi2011Ws" 


# PopDen2010Ws -- asked for this one
# PopDen2010Cat




# H0: LOI and dry weight have same drivers ####

