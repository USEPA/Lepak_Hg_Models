# Make Predicted Iso vs. Predictor (obs) spaghetti plots for final isotope model
# By HUC 2 or Omernik ER 2 


library(tidyverse)
library(maps)
library(colorspace)
library(stringr)

output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(fig_dir, "LOESS/D202/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "LOESS/D199/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "LOESS/D200/"), recursive=T, showWarnings = FALSE) 

# Read in predictions from Predict_ISO_MVRF_subset.R
df <- read.csv(paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24.csv"))
df <- df %>% rename(WetLossConv=WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s)

preds <- names(df)[2:11]

head(df)
str(df)

# Pred_D202_origUnits, Pred_D199_origUnits, Pred_D200_origUnits

df$HUC2 <- factor(df$HUC2)
table(df$HUC2) # 18

df$Omernik_II <- factor(df$Omernik_II) # 19
sort(table(df$Omernik_II))

# df$Omernik_III <- factor(df$Omernik_III)
# sort(table(df$Omernik_III)) # Too few per ecoregion


MainStates <- map_data("state")

cols1 <- qualitative_hcl(13, palette = "Dark 3")
cols2 <- sequential_hcl(5, palette = "Light Grays")
set.seed(17)
cols <- sample(c(cols1, cols2))

cols3 <- qualitative_hcl(14, palette = "Dark 3")
set.seed(17) 
cols4 <- sample(c(cols3, cols2))



# Plot HUC2
centroid <- df %>% group_by(HUC2) %>% summarise(lon_mean=mean(LON_DD83), lat_mean=mean(LAT_DD83))
ggplot(df, aes(fill=HUC2, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=4, col="gray70", shape=21) + 
  theme_void() +
  scale_fill_manual(values = cols) +
  geom_text(data = centroid, mapping = aes(x=lon_mean , y=lat_mean, label=HUC2), size=8)
  # scale_fill_continuous_divergingx(palette = 'RdBu', mid = 0, alpha=1, rev=T, breaks=seq(-3,3,1), limits=c(-3,3))
ggsave(paste0(fig_dir, "/HUC2.png"), width=10, height=6)



# Omernik II
  ggplot(df, aes(fill=Omernik_II, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=5, col="gray70", shape=21) + 
  theme_void() +
  scale_fill_manual(values = cols4)
ggsave(paste0(fig_dir, "/OmernikII.png"), width=15, height=6)

# Abbreviate OmernikII
df$Omernik_II_code <- str_split_i(as.character(df$Omernik_II), "\\s+", 1)
eco_centroid <- df %>% group_by(Omernik_II_code) %>% summarise(lon_mean=mean(LON_DD83), lat_mean=mean(LAT_DD83), lon_median=median(LON_DD83), lat_median=median(LAT_DD83))

ggplot(df, aes(fill=Omernik_II_code, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=5, col="gray70", shape=21) + 
  theme_void() +
  scale_fill_manual(values = cols4)+
  geom_label(data = eco_centroid, mapping = aes(x=lon_mean , y=lat_mean, fill=Omernik_II_code, label=Omernik_II_code), size=6, alpha=0.85, colour="black", show.legend = F) +
  scale_colour_manual(values = cols4)
ggsave(paste0(fig_dir, "/OmernikIIcode.png"), width=10, height=6)

O2_kp <- names(table(df$Omernik_II_code)[table(df$Omernik_II_code)>9]) 
names(table(df$Omernik_II_code)[table(df$Omernik_II_code)<10]) # "10.2" "12.1" "15.4" "9.5"





# Plot smoothed relationship between each predictor and each isotope by Omernik II

### D202 #### 

# First plot all lakes together
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D202/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(size=2) + 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])

  ggsave(paste0(fig_dir, "LOESS/D202/", preds[j], "/D202_", preds[j], "_ALL.png"), width=10, height=6)
}

# Then by ecoregion
for(i in 1:length(unique(df$Omernik_II_code))){
  
  omer_i <- sort(unique(df$Omernik_II_code))[i]
  omer_i <- str_replace(omer_i, "[.]", "_")
  
  for(j in 1:length(preds)){
    if(i==1) dir.create(paste0(fig_dir, "LOESS/D202/", preds[j]), recursive=T, showWarnings = FALSE) 
    
    df %>% filter(Omernik_II_code %in% names(table(df$Omernik_II_code))[i]) %>% 
            ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits, colour = Omernik_II_code, group = Omernik_II_code)) + 
            theme_minimal() +
            theme(text=element_text(size=20)) +
            geom_point(size=2) + 
            geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
            scale_colour_manual(values = cols4[i])+
        xlab(preds[j])
    # print(d202_plot)
      
    ggsave(paste0(fig_dir, "LOESS/D202/", preds[j], "/D202_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}



# Fixed scales
# First plot all lakes together
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D202_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(pred_j) , y = Pred_D202_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(size=2) + 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D202_origUnits))
  
  ggsave(paste0(fig_dir, "LOESS/D202_fixed/", preds[j], "/D202_", preds[j], "_ALL.png"), width=10, height=6)
}

# Then by ecoregion
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D202_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  for(i in 1:length(unique(df$Omernik_II_code))){
  
  omer_i <- sort(unique(df$Omernik_II_code))[i]
  omer_i <- str_replace(omer_i, "[.]", "_")
  
    df %>% filter(Omernik_II_code %in% names(table(df$Omernik_II_code))[i]) %>% 
      ggplot(aes(x =  get(pred_j) , y = Pred_D202_origUnits, colour = Omernik_II_code, group = Omernik_II_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(size=2) + 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j]) +
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D202_origUnits))
    # print(d202_plot)
    
    ggsave(paste0(fig_dir, "LOESS/D202_fixed/", preds[j], "/D202_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}


######## D200 ########
# First plot all lakes together
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D200/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(size=2) + 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])
  
  ggsave(paste0(fig_dir, "LOESS/D200/", preds[j], "/D200_", preds[j], "_ALL.png"), width=10, height=6)
}

# Then by ecoregion
for(i in 1:length(unique(df$Omernik_II_code))){
  
  omer_i <- sort(unique(df$Omernik_II_code))[i]
  omer_i <- str_replace(omer_i, "[.]", "_")
  
  for(j in 1:length(preds)){
    if(i==1) dir.create(paste0(fig_dir, "LOESS/D200/", preds[j]), recursive=T, showWarnings = FALSE) 
    
    df %>% filter(Omernik_II_code %in% names(table(df$Omernik_II_code))[i]) %>% 
      ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits, colour = Omernik_II_code, group = Omernik_II_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(size=2) + 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j])
    # print(D200_plot)
    
    ggsave(paste0(fig_dir, "LOESS/D200/", preds[j], "/D200_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}

# Fixed scales
# First plot all lakes together
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D200_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(pred_j) , y = Pred_D200_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(size=2) + 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D200_origUnits))
  
  ggsave(paste0(fig_dir, "LOESS/D200_fixed/", preds[j], "/D200_", preds[j], "_ALL.png"), width=10, height=6)
}

# Then by ecoregion
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D200_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  for(i in 1:length(unique(df$Omernik_II_code))){
    
    omer_i <- sort(unique(df$Omernik_II_code))[i]
    omer_i <- str_replace(omer_i, "[.]", "_")
    
    df %>% filter(Omernik_II_code %in% names(table(df$Omernik_II_code))[i]) %>% 
      ggplot(aes(x =  get(pred_j) , y = Pred_D200_origUnits, colour = Omernik_II_code, group = Omernik_II_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(size=2) + 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j]) +
      coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D200_origUnits))
    # print(D200_plot)
    
    ggsave(paste0(fig_dir, "LOESS/D200_fixed/", preds[j], "/D200_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}



######## D199 ########
# First plot all lakes together
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D199/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(size=2) + 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])
  
  ggsave(paste0(fig_dir, "LOESS/D199/", preds[j], "/D199_", preds[j], "_ALL.png"), width=10, height=6)
}

# Then by ecoregion
for(i in 1:length(unique(df$Omernik_II_code))){
  
  omer_i <- sort(unique(df$Omernik_II_code))[i]
  omer_i <- str_replace(omer_i, "[.]", "_")
  
  for(j in 1:length(preds)){
    if(i==1) dir.create(paste0(fig_dir, "LOESS/D199/", preds[j]), recursive=T, showWarnings = FALSE) 
    
    df %>% filter(Omernik_II_code %in% names(table(df$Omernik_II_code))[i]) %>% 
      ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits, colour = Omernik_II_code, group = Omernik_II_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(size=2) + 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j])
    # print(D199_plot)
    
    ggsave(paste0(fig_dir, "LOESS/D199/", preds[j], "/D199_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}

# Fixed scales
# First plot all lakes together
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D199_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(pred_j) , y = Pred_D199_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(size=2) + 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D199_origUnits))
  
  ggsave(paste0(fig_dir, "LOESS/D199_fixed/", preds[j], "/D199_", preds[j], "_ALL.png"), width=10, height=6)
}

# Then by ecoregion
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS/D199_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  for(i in 1:length(unique(df$Omernik_II_code))){
    
    omer_i <- sort(unique(df$Omernik_II_code))[i]
    omer_i <- str_replace(omer_i, "[.]", "_")
    
    df %>% filter(Omernik_II_code %in% names(table(df$Omernik_II_code))[i]) %>% 
      ggplot(aes(x =  get(pred_j) , y = Pred_D199_origUnits, colour = Omernik_II_code, group = Omernik_II_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(size=2) + 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j]) +
      coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D199_origUnits))
    # print(D199_plot)
    
    ggsave(paste0(fig_dir, "LOESS/D199_fixed/", preds[j], "/D199_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}














HUC2_kp <- names(table(df$HUC2)[table(df$HUC2)>9])
names(table(df$HUC2)[table(df$HUC2)<10]) # "6"  "13"

df %>% group_by(HUC2) %>% summarise(cor(Pred_D202_origUnits, Precip8110Cat , method="spearman"))
# HUC 10, 11 nonlinear


# Just points
df %>% filter(HUC2 %in% HUC2_kp[1:4]) %>% 
  ggplot( aes(x = Precip8110Cat , y = Pred_D202_origUnits, colour = HUC2, group = HUC2)) + 
  theme_minimal() +
  theme(text=element_text(size=20)) +
  geom_point(size=2)# + 
  # geom_smooth(se=F, linewidth=1.2)

# Do 3 at a time:
# for(i in 1:length(HUC2_kp)){
# print(df %>% filter(HUC2 %in% HUC2_kp[i]) %>% 
#   ggplot(aes(x = Precip8110Cat , y = Pred_D202_origUnits, colour = HUC2, group = HUC2)) + 
#   theme_minimal() +
#   theme(text=element_text(size=20)) +
#   geom_point(size=2) + 
#   geom_smooth(span=1, se=F, linewidth=1.2)+
#   scale_colour_manual(values = cols[i]))
# }

# **** Stopped here - using these to look at D202-precip relationships by HUC ****
for(i in 1:length(unique(df$HUC2))){
  print(df %>% filter(HUC2 %in% names(table(df$HUC2))[i]) %>% 
          ggplot(aes(x = Precip8110Cat , y = Pred_D202_origUnits, colour = HUC2, group = HUC2)) + 
          theme_minimal() +
          theme(text=element_text(size=20)) +
          geom_point(size=2) + 
          geom_smooth(span=1, se=F, linewidth=1.2)+
          scale_colour_manual(values = cols[i]))
}




# Points with smoothers
# Use span (0-1) to control wiggliness (higher is smoother)
df %>% filter(HUC2 %in% HUC2_kp) %>% 
  ggplot(aes(x = Precip8110Cat , y = Pred_D202_origUnits, colour = HUC2, group = HUC2)) + 
  theme_minimal() +
  theme(text=element_text(size=20)) +
  geom_point(size=2) + 
  geom_smooth(span=.9, se=F, linewidth=1.2)

# Just smoothers
df %>% filter(HUC2 %in% HUC2_kp) %>% 
  ggplot(aes(x = Hg0DryDep, y = Pred_D202_origUnits, colour = HUC2, group = HUC2)) + 
  theme_minimal() +
  theme(text=element_text(size=20)) +
  geom_smooth(span=.9, se=F, linewidth=1.2)
