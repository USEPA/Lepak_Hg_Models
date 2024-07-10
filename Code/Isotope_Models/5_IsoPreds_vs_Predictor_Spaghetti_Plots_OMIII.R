# Make Predicted Iso vs. Predictor (obs) spaghetti plots for final isotope model
# By Omernik ER 3


library(tidyverse)
library(maps)
library(colorspace)
library(stringr)

output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(fig_dir, "LOESS_OMIII/D202/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "LOESS_OMIII/D199/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "LOESS_OMIII/D200/"), recursive=T, showWarnings = FALSE) 

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

df$Omernik_III <- factor(df$Omernik_III)
sort(table(df$Omernik_III)) # Too few per ecoregion


df$Omernik_III_code <- str_split_i(as.character(df$Omernik_III), "\\s+", 1)
length(unique(df$Omernik_III_code)) # 79

O3_kp <- names(table(df$Omernik_III_code)[table(df$Omernik_III_code)>9]) # 44

names(table(df$Omernik_III_code)[table(df$Omernik_III_code)<10]) # "10.2" "12.1" "15.4" "9.5"
#  [1] "10.1.6" "10.1.7" "10.1.8" "10.2.1" "10.2.2" "11.1.2" "11.1.3" "12.1.1" "15.4.1" "5.3.3"  "6.2.11" "6.2.4" 
# [13] "6.2.5"  "6.2.8"  "6.2.9"  "7.1.8"  "7.1.9"  "8.1.1"  "8.1.10" "8.1.3"  "8.1.5"  "8.2.2"  "8.2.3"  "8.3.1" 
# [25] "8.3.6"  "8.4.2"  "8.4.4"  "8.4.7"  "8.4.8"  "8.4.9"  "8.5.2"  "8.5.4"  "9.2.2"  "9.4.4"  "9.5.1" 

df_save <- df

df <- df %>% filter(Omernik_III_code %in% O3_kp)

MainStates <- map_data("state")

# cols1 <- qualitative_hcl(13, palette = "Dark 3")
cols2 <- sequential_hcl(10, palette = "Light Grays")
set.seed(17)
# cols <- sample(c(cols1, cols2))

cols3 <- qualitative_hcl(34, palette = "Dark 3")
set.seed(21) 
cols4 <- sample(c(cols3, cols2))
# cols5 <- c(cols3, cols2)

# Omernik III
# ggplot(df, aes(fill=Omernik_III, x=LON_DD83, y=LAT_DD83)) + 
#   geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
#   geom_point(size=5, col="gray70", shape=21) + 
#   theme_void() +
#   scale_fill_manual(values = cols4)
# ggsave(paste0(fig_dir, "/OmernikIII.png"), width=15, height=6)

eco_centroid <- df %>% group_by(Omernik_III_code) %>% summarise(lon_mean=mean(LON_DD83), lat_mean=mean(LAT_DD83), lon_median=median(LON_DD83), lat_median=median(LAT_DD83))

ggplot(df, aes(fill=Omernik_III_code, x=LON_DD83, y=LAT_DD83)) + 
  geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=5, col="gray70", shape=21) + 
  theme_void() +
  scale_fill_manual(values = cols4)+
  geom_label(data = eco_centroid, mapping = aes(x=lon_mean , y=lat_mean, fill=Omernik_III_code, label=Omernik_III_code), size=5, alpha=0.85, colour="black", show.legend = F) +
  scale_colour_manual(values = cols5)
ggsave(paste0(fig_dir, "/OmernikIIIcode.png"), width=12, height=6)




# Add lake origin
head(df)
All_Dat <- read.csv("Formatted_Data/ISO_Imputed_Test_Data_ALL_LAKES.csv")
All_Dat$Type <- All_Dat$LAKE_ORIGIN12
All_Dat$Type[All_Dat$Type==1] <- "Artificial"
All_Dat$Type[All_Dat$Type==0] <- "Natural"

All_Dat_join <- All_Dat %>% dplyr::select(NLA12_ID, Type)
# 1 = Manmade/reservoir; 0=natural

df <- left_join(df, All_Dat_join)


# Plot smoothed relationship between each predictor and each isotope by Omernik III

### D202 #### 

for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D202/", preds[j]), recursive=T, showWarnings = FALSE) 
    
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])
  ggsave(paste0(fig_dir, "LOESS_OMIII/D202/", preds[j], "/D202_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Omernik_III_code))){
    
    omer_i <- sort(unique(df$Omernik_III_code))[i]
    omer_i <- str_replace(omer_i, "[.]", "_")
    
    
    df %>% filter(Omernik_III_code %in% names(table(df$Omernik_III_code))[i]) %>% 
      ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits, colour = Omernik_III_code, group = Omernik_III_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(aes(shape=Type), size=2) + 
      scale_shape_manual(values = c(4, 1))+ 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j])
    # print(d202_plot)
    
    ggsave(paste0(fig_dir, "LOESS_OMIII/D202/", preds[j], "/D202_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}



# # Fixed scales
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D202_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D202_origUnits))
  ggsave(paste0(fig_dir, "LOESS_OMIII/D202_fixed/", preds[j], "/D202_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Omernik_III_code))){
    
    omer_i <- sort(unique(df$Omernik_III_code))[i]
    omer_i <- str_replace(omer_i, "[.]", "_")
    
    
    df %>% filter(Omernik_III_code %in% names(table(df$Omernik_III_code))[i]) %>% 
      ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits, colour = Omernik_III_code, group = Omernik_III_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(aes(shape=Type), size=2) + 
      scale_shape_manual(values = c(4, 1))+ 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j])+
      coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D202_origUnits))
    # print(d202_plot)
    
    ggsave(paste0(fig_dir, "LOESS_OMIII/D202_fixed/", preds[j], "/D202_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}


######## D200 ########
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D200/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])
  ggsave(paste0(fig_dir, "LOESS_OMIII/D200/", preds[j], "/D200_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Omernik_III_code))){
    
    omer_i <- sort(unique(df$Omernik_III_code))[i]
    omer_i <- str_replace(omer_i, "[.]", "_")
    
    
    df %>% filter(Omernik_III_code %in% names(table(df$Omernik_III_code))[i]) %>% 
      ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits, colour = Omernik_III_code, group = Omernik_III_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(aes(shape=Type), size=2) + 
      scale_shape_manual(values = c(4, 1))+ 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j])
    # print(d200_plot)
    
    ggsave(paste0(fig_dir, "LOESS_OMIII/D200/", preds[j], "/D200_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}

# # Fixed scales
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D200_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D200_origUnits))
  ggsave(paste0(fig_dir, "LOESS_OMIII/D200_fixed/", preds[j], "/D200_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Omernik_III_code))){
    
    omer_i <- sort(unique(df$Omernik_III_code))[i]
    omer_i <- str_replace(omer_i, "[.]", "_")
    
    
    df %>% filter(Omernik_III_code %in% names(table(df$Omernik_III_code))[i]) %>% 
      ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits, colour = Omernik_III_code, group = Omernik_III_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(aes(shape=Type), size=2) + 
      scale_shape_manual(values = c(4, 1))+ 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j])+
      coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D200_origUnits))
    # print(d200_plot)
    
    ggsave(paste0(fig_dir, "LOESS_OMIII/D200_fixed/", preds[j], "/D200_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}



######## D199 ########
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D199/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])
  ggsave(paste0(fig_dir, "LOESS_OMIII/D199/", preds[j], "/D199_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Omernik_III_code))){
    
    omer_i <- sort(unique(df$Omernik_III_code))[i]
    omer_i <- str_replace(omer_i, "[.]", "_")
    
    
    df %>% filter(Omernik_III_code %in% names(table(df$Omernik_III_code))[i]) %>% 
      ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits, colour = Omernik_III_code, group = Omernik_III_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(aes(shape=Type), size=2) + 
      scale_shape_manual(values = c(4, 1))+ 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j])
    # print(d199_plot)
    
    ggsave(paste0(fig_dir, "LOESS_OMIII/D199/", preds[j], "/D199_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}

# # Fixed scales
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D199_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=T, linewidth=2, alpha=0.3, col="red", method="loess")+
    # scale_colour_manual(values = cols4[i])+
    xlab(preds[j])+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D199_origUnits))
  ggsave(paste0(fig_dir, "LOESS_OMIII/D199_fixed/", preds[j], "/D199_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Omernik_III_code))){
    
    omer_i <- sort(unique(df$Omernik_III_code))[i]
    omer_i <- str_replace(omer_i, "[.]", "_")
    
    
    df %>% filter(Omernik_III_code %in% names(table(df$Omernik_III_code))[i]) %>% 
      ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits, colour = Omernik_III_code, group = Omernik_III_code)) + 
      theme_minimal() +
      theme(text=element_text(size=20)) +
      geom_point(aes(shape=Type), size=2) + 
      scale_shape_manual(values = c(4, 1))+ 
      geom_smooth(span=1, se=T, linewidth=1.2, alpha=0.2)+
      scale_colour_manual(values = cols4[i])+
      xlab(preds[j])+
      coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D199_origUnits))
    # print(d199_plot)
    
    ggsave(paste0(fig_dir, "LOESS_OMIII/D199_fixed/", preds[j], "/D199_", preds[j], "_", omer_i, ".png"), width=10, height=6)
  }
}








## Look at overall relationships by artificial/natural
## *** Note: These aren't being saved currently, just plotting them for exploration.
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D202/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  print(df %>%
          ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits)) + 
          theme_minimal() +
          theme(text=element_text(size=20)) +
          geom_point(aes(shape=Type), size=2) + 
          scale_shape_manual(values = c(4, 1))+ 
          geom_smooth(aes(col=Type), span=.8, se=T, linewidth=2, alpha=0.3,  method="loess")+ # col="red",
          # scale_colour_manual(values = cols4[i])+
          xlab(preds[j]))
}

for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D200/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  print(df %>%
          ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits)) + 
          theme_minimal() +
          theme(text=element_text(size=20)) +
          geom_point(aes(shape=Type), size=2) + 
          scale_shape_manual(values = c(4, 1))+ 
          geom_smooth(aes(col=Type), span=.8, se=T, linewidth=2, alpha=0.3,  method="loess")+ # col="red",
          # scale_colour_manual(values = cols4[i])+
          xlab(preds[j]))
}

for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_OMIII/D199/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  print(df %>%
          ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits)) + 
          theme_minimal() +
          theme(text=element_text(size=20)) +
          geom_point(aes(shape=Type), size=2) + 
          scale_shape_manual(values = c(4, 1))+ 
          geom_smooth(aes(col=Type), span=.8, se=T, linewidth=2, alpha=0.3,  method="loess")+ # col="red",
          # scale_colour_manual(values = cols4[i])+
          xlab(preds[j]))
}