# Make Predicted Iso vs. Predictor (obs) spaghetti plots for final isotope model
# By SKATER SPATIAL CLUSTERS FOR PREDICTORS


library(tidyverse)
library(maps)
library(colorspace)
library(stringr)

output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(fig_dir, "LOESS_SKATER20/D202/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "LOESS_SKATER20/D199/"), recursive=T, showWarnings = FALSE) 
dir.create(paste0(fig_dir, "LOESS_SKATER20/D200/"), recursive=T, showWarnings = FALSE) 

# Read in predictions (from Predict_ISO_MVRF_subset.R) with SKATER-20 clusters added (from 4_IsoPreds_Voronoi.R)
df <- read.csv(paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv"))
# df <- df %>% rename(WetLossConv=WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s)

preds <- names(df)[2:11]

head(df)
str(df)

# Pred_D202_origUnits, Pred_D199_origUnits, Pred_D200_origUnits

df$Cluster <- factor(df$Maha20)

Cl_kp <- names(table(df$Cluster)[table(df$Cluster)>9]) # 15 clusters with at least 10 lakes

names(table(df$Cluster)[table(df$Cluster)<10]) # "12" "13" "17" "18" "20"

# df_save <- df

# df <- df %>% filter(Cluster %in% Cl_kp)


## Need sf objects from 4_IsoPreds_Voronoi.R
# Or just use figures created in that script.

# 20 colors for plotting
cols2 <- sequential_hcl(5, palette = "Light Grays")
cols3 <- qualitative_hcl(17, palette = "Dark 3")
set.seed(13) # 5
cols <- sample(c(cols3, cols2[-c(4,5)]))

df.cols <- data.frame(Cluster=factor(1:20), cols=cols)

df <- left_join(df, df.cols)



# "#C98027" "#989600" "#00ACAA" "#6290E5" "#739F00" "#D965C6" "#B48C00" "#E264A9" "#009EDA" "#00AA63" "#A8A8A8"
# "#E16A86" "#00A7C5" "#D8755E" "#7A7A7A" "#9E7FE5" "#00AD89" "#36A631" "#474747" "#C36FDA"

# MainStates <- map_data("state")

# This type of figure already done in 4_IsoPreds_Voronoi.R
# eco_centroid <- df %>% group_by(Omernik_III_code) %>% summarise(lon_mean=mean(LON_DD83), lat_mean=mean(LAT_DD83), lon_median=median(LON_DD83), lat_median=median(LAT_DD83))
# 
# ggplot(df, aes(fill=Omernik_III_code, x=LON_DD83, y=LAT_DD83)) + 
#   geom_polygon( data=MainStates, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
#   geom_point(size=5, col="gray70", shape=21) + 
#   theme_void() +
#   scale_fill_manual(values = cols4)+
#   geom_label(data = eco_centroid, mapping = aes(x=lon_mean , y=lat_mean, fill=Omernik_III_code, label=Omernik_III_code), size=5, alpha=0.85, colour="black", show.legend = F) +
#   scale_colour_manual(values = cols5)
# ggsave(paste0(fig_dir, "/OmernikIIIcode.png"), width=12, height=6)




# Add lake origin
head(df)
All_Dat <- read.csv("Formatted_Data/ISO_Imputed_Test_Data_ALL_LAKES.csv")
All_Dat$Type <- All_Dat$LAKE_ORIGIN12
All_Dat$Type[All_Dat$Type==1] <- "Artificial"
All_Dat$Type[All_Dat$Type==0] <- "Natural"

All_Dat_join <- All_Dat %>% dplyr::select(NLA12_ID, Type)
# 1 = Manmade/reservoir; 0=natural

df <- left_join(df, All_Dat_join)







# Plot smoothed relationship between each predictor and each isotope by predictor cluster

# Tried making separate smoothers for artificial/natural in each cluster, but too busy
# Tried spaghetti plots, but too busy


### D202 #### 

for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_SKATER20/D202/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="red", method="loess")+
    geom_smooth(aes(group=Type, linetype=Type), span=.8, se=F, linewidth=1, alpha=0.2, col="red")+
    scale_linetype_manual(name="Type", 
                          values = c("Artificial" = "dashed", "Natural" = "dotted"),
                          breaks=c("Artificial", "Natural")) +
    xlab(preds[j])+ 
    theme(legend.key.size = unit(2,"line"))
  ggsave(paste0(fig_dir, "LOESS_SKATER20/D202/", preds[j], "/D202_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Maha20))){
    
    skater_i <- sort(unique(df$Maha20))[i]

    if(skater_i %in% Cl_kp){
    
      df_plot <- df %>% filter(Maha20 %in% skater_i)
      
      
      # If at least 10 artificial and 10 natural, plot additional smoothers for them
      # if(sum(df_plot$Type=="Natural")>9 & sum(df_plot$Type=="Artificial")>9){
      # 
      #   df_plot %>% 
      #     ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits,  group = Maha20)) + 
      #     theme_minimal() +
      #     theme(text=element_text(size=20)) +
      #     geom_point(aes(shape=Type), size=2, col=cols[i]) + 
      #     scale_shape_manual(values = c(4, 1))+ 
      #     geom_smooth(span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
      #     geom_smooth(aes(group=Type, linetype=Type), span=1, se=F, linewidth=1, alpha=0.2, col=cols[i])+
      #     scale_linetype_manual(name="Type", 
      #                             values = c("Artificial" = "dashed", "Natural" = "dotted"),
      #                             breaks=c("Artificial", "Natural")) +
      #     xlab(preds[j]) + 
      #     theme(legend.key.size = unit(2,"line"))
      #   # print(d202_plot)
      # } else {
        df_plot %>% 
          ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits,  group = Cluster)) + 
          theme_minimal() +
          theme(text=element_text(size=20)) +
          geom_point(aes(shape=Type), size=2, col=cols[i]) + 
          scale_shape_manual(values = c(4, 1))+ 
          geom_smooth(span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
          xlab(preds[j])
        
      # }
     
      ggsave(paste0(fig_dir, "LOESS_SKATER20/D202/", preds[j], "/D202_", preds[j], "_Cluster", skater_i, ".png"), width=10, height=6)
    }
  }
}



# D202 fixed axes
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_SKATER20/D202_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="red", method="loess")+
    geom_smooth(aes(group=Type, linetype=Type), span=.8, se=F, linewidth=1, alpha=0.2, col="red")+
    scale_linetype_manual(name="Type", 
                          values = c("Artificial" = "dashed", "Natural" = "dotted"),
                          breaks=c("Artificial", "Natural")) +
    xlab(preds[j])+ 
    theme(legend.key.size = unit(2,"line"))+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D202_origUnits))
  ggsave(paste0(fig_dir, "LOESS_SKATER20/D202_fixed/", preds[j], "/D202_", preds[j], "_ALL.png"), width=10, height=6)
  
  # Try spaghetti plot
  # df_kp <- df %>% filter(Maha20 %in% Cl_kp)
  # 
  # df %>%
  #   ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits)) + 
  #   theme_minimal() +
  #   theme(text=element_text(size=20)) +
  #   geom_point(aes(shape=Type), size=2) + 
  #   scale_shape_manual(values = c(4, 1))+ 
  #   geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="black", method="loess")+
  #   geom_smooth(data=df_kp, aes(group=Cluster, col=Cluster), span=1, se=F, linewidth=1, alpha=0.2)+
  #   xlab(preds[j])+ 
  #   theme(legend.key.size = unit(2,"line"))+
  #   coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D202_origUnits))
  # 
  
  
  for(i in 1:length(unique(df$Maha20))){
    
    skater_i <- sort(unique(df$Maha20))[i]
    
    if(skater_i %in% Cl_kp){
      
      df_plot <- df %>% filter(Maha20 %in% skater_i)
      
      df_plot %>% 
        ggplot(aes(x =  get(preds[j]) , y = Pred_D202_origUnits)) + 
        theme_minimal() +
        coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D202_origUnits))+
        theme(text=element_text(size=20)) +
        geom_smooth(data=df, span=.8, se=F, linewidth=2, alpha=0.3, col="gray85", method="loess")+
        
        geom_point(aes(shape=Type), size=2, col=cols[i]) + 
        scale_shape_manual(values = c(4, 1))+ 
        geom_smooth(aes(group = Maha20), span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
        xlab(preds[j])
      
      # }
      
      ggsave(paste0(fig_dir, "LOESS_SKATER20/D202_fixed/", preds[j], "/D202_", preds[j], "_Cluster", skater_i, ".png"), width=10, height=6)
    }
  }
}













######## D200 ########

for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_SKATER20/D200/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="red", method="loess")+
    geom_smooth(aes(group=Type, linetype=Type), span=.8, se=F, linewidth=1, alpha=0.2, col="red")+
    scale_linetype_manual(name="Type", 
                          values = c("Artificial" = "dashed", "Natural" = "dotted"),
                          breaks=c("Artificial", "Natural")) +
    xlab(preds[j])+ 
    theme(legend.key.size = unit(2,"line"))
  ggsave(paste0(fig_dir, "LOESS_SKATER20/D200/", preds[j], "/D200_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Maha20))){
    
    skater_i <- sort(unique(df$Maha20))[i]
    
    if(skater_i %in% Cl_kp){
      
      df_plot <- df %>% filter(Maha20 %in% skater_i)
      
      
      # If at least 10 artificial and 10 natural, plot additional smoothers for them
      # if(sum(df_plot$Type=="Natural")>9 & sum(df_plot$Type=="Artificial")>9){
      # 
      #   df_plot %>% 
      #     ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits,  group = Maha20)) + 
      #     theme_minimal() +
      #     theme(text=element_text(size=20)) +
      #     geom_point(aes(shape=Type), size=2, col=cols[i]) + 
      #     scale_shape_manual(values = c(4, 1))+ 
      #     geom_smooth(span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
      #     geom_smooth(aes(group=Type, linetype=Type), span=1, se=F, linewidth=1, alpha=0.2, col=cols[i])+
      #     scale_linetype_manual(name="Type", 
      #                             values = c("Artificial" = "dashed", "Natural" = "dotted"),
      #                             breaks=c("Artificial", "Natural")) +
      #     xlab(preds[j]) + 
      #     theme(legend.key.size = unit(2,"line"))
      #   # print(D200_plot)
      # } else {
      df_plot %>% 
        ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits,  group = Cluster)) + 
        theme_minimal() +
        theme(text=element_text(size=20)) +
        geom_point(aes(shape=Type), size=2, col=cols[i]) + 
        scale_shape_manual(values = c(4, 1))+ 
        geom_smooth(span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
        xlab(preds[j])
      
      # }
      
      ggsave(paste0(fig_dir, "LOESS_SKATER20/D200/", preds[j], "/D200_", preds[j], "_Cluster", skater_i, ".png"), width=10, height=6)
    }
  }
}



# D200 fixed axes
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_SKATER20/D200_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="red", method="loess")+
    geom_smooth(aes(group=Type, linetype=Type), span=.8, se=F, linewidth=1, alpha=0.2, col="red")+
    scale_linetype_manual(name="Type", 
                          values = c("Artificial" = "dashed", "Natural" = "dotted"),
                          breaks=c("Artificial", "Natural")) +
    xlab(preds[j])+ 
    theme(legend.key.size = unit(2,"line"))+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D200_origUnits))
  ggsave(paste0(fig_dir, "LOESS_SKATER20/D200_fixed/", preds[j], "/D200_", preds[j], "_ALL.png"), width=10, height=6)
  
  # Try spaghetti plot
  # df_kp <- df %>% filter(Maha20 %in% Cl_kp)
  # 
  # df %>%
  #   ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits)) + 
  #   theme_minimal() +
  #   theme(text=element_text(size=20)) +
  #   geom_point(aes(shape=Type), size=2) + 
  #   scale_shape_manual(values = c(4, 1))+ 
  #   geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="black", method="loess")+
  #   geom_smooth(data=df_kp, aes(group=Cluster, col=Cluster), span=1, se=F, linewidth=1, alpha=0.2)+
  #   xlab(preds[j])+ 
  #   theme(legend.key.size = unit(2,"line"))+
  #   coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D200_origUnits))
  # 
  
  
  for(i in 1:length(unique(df$Maha20))){
    
    skater_i <- sort(unique(df$Maha20))[i]
    
    if(skater_i %in% Cl_kp){
      
      df_plot <- df %>% filter(Maha20 %in% skater_i)
      
      df_plot %>% 
        ggplot(aes(x =  get(preds[j]) , y = Pred_D200_origUnits)) + 
        theme_minimal() +
        coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D200_origUnits))+
        theme(text=element_text(size=20)) +
        geom_smooth(data=df, span=.8, se=F, linewidth=2, alpha=0.3, col="gray85", method="loess")+
        
        geom_point(aes(shape=Type), size=2, col=cols[i]) + 
        scale_shape_manual(values = c(4, 1))+ 
        geom_smooth(aes(group = Maha20), span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
        xlab(preds[j])
      
      # }
      
      ggsave(paste0(fig_dir, "LOESS_SKATER20/D200_fixed/", preds[j], "/D200_", preds[j], "_Cluster", skater_i, ".png"), width=10, height=6)
    }
  }
}




# ######## D199 ########
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_SKATER20/D199/", preds[j]), recursive=T, showWarnings = FALSE) 
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="red", method="loess")+
    geom_smooth(aes(group=Type, linetype=Type), span=.8, se=F, linewidth=1, alpha=0.2, col="red")+
    scale_linetype_manual(name="Type", 
                          values = c("Artificial" = "dashed", "Natural" = "dotted"),
                          breaks=c("Artificial", "Natural")) +
    xlab(preds[j])+ 
    theme(legend.key.size = unit(2,"line"))
  ggsave(paste0(fig_dir, "LOESS_SKATER20/D199/", preds[j], "/D199_", preds[j], "_ALL.png"), width=10, height=6)
  
  for(i in 1:length(unique(df$Maha20))){
    
    skater_i <- sort(unique(df$Maha20))[i]
    
    if(skater_i %in% Cl_kp){
      
      df_plot <- df %>% filter(Maha20 %in% skater_i)
      
      
      # If at least 10 artificial and 10 natural, plot additional smoothers for them
      # if(sum(df_plot$Type=="Natural")>9 & sum(df_plot$Type=="Artificial")>9){
      # 
      #   df_plot %>% 
      #     ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits,  group = Maha20)) + 
      #     theme_minimal() +
      #     theme(text=element_text(size=20)) +
      #     geom_point(aes(shape=Type), size=2, col=cols[i]) + 
      #     scale_shape_manual(values = c(4, 1))+ 
      #     geom_smooth(span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
      #     geom_smooth(aes(group=Type, linetype=Type), span=1, se=F, linewidth=1, alpha=0.2, col=cols[i])+
      #     scale_linetype_manual(name="Type", 
      #                             values = c("Artificial" = "dashed", "Natural" = "dotted"),
      #                             breaks=c("Artificial", "Natural")) +
      #     xlab(preds[j]) + 
      #     theme(legend.key.size = unit(2,"line"))
      #   # print(D199_plot)
      # } else {
      df_plot %>% 
        ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits,  group = Cluster)) + 
        theme_minimal() +
        theme(text=element_text(size=20)) +
        geom_point(aes(shape=Type), size=2, col=cols[i]) + 
        scale_shape_manual(values = c(4, 1))+ 
        geom_smooth(span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
        xlab(preds[j])
      
      # }
      
      ggsave(paste0(fig_dir, "LOESS_SKATER20/D199/", preds[j], "/D199_", preds[j], "_Cluster", skater_i, ".png"), width=10, height=6)
    }
  }
}



# D199 fixed axes
for(j in 1:length(preds)){
  dir.create(paste0(fig_dir, "LOESS_SKATER20/D199_fixed/", preds[j]), recursive=T, showWarnings = FALSE) 
  pred_j <- preds[j]
  
  df %>%
    ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits)) + 
    theme_minimal() +
    theme(text=element_text(size=20)) +
    geom_point(aes(shape=Type), size=2) + 
    scale_shape_manual(values = c(4, 1))+ 
    geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="red", method="loess")+
    geom_smooth(aes(group=Type, linetype=Type), span=.8, se=F, linewidth=1, alpha=0.2, col="red")+
    scale_linetype_manual(name="Type", 
                          values = c("Artificial" = "dashed", "Natural" = "dotted"),
                          breaks=c("Artificial", "Natural")) +
    xlab(preds[j])+ 
    theme(legend.key.size = unit(2,"line"))+
    coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D199_origUnits))
  ggsave(paste0(fig_dir, "LOESS_SKATER20/D199_fixed/", preds[j], "/D199_", preds[j], "_ALL.png"), width=10, height=6)
  
  # Try spaghetti plot
  # df_kp <- df %>% filter(Maha20 %in% Cl_kp)
  # 
  # df %>%
  #   ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits)) + 
  #   theme_minimal() +
  #   theme(text=element_text(size=20)) +
  #   geom_point(aes(shape=Type), size=2) + 
  #   scale_shape_manual(values = c(4, 1))+ 
  #   geom_smooth(span=.8, se=F, linewidth=2, alpha=0.3, col="black", method="loess")+
  #   geom_smooth(data=df_kp, aes(group=Cluster, col=Cluster), span=1, se=F, linewidth=1, alpha=0.2)+
  #   xlab(preds[j])+ 
  #   theme(legend.key.size = unit(2,"line"))+
  #   coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D199_origUnits))
  # 
  
  
  for(i in 1:length(unique(df$Maha20))){
    
    skater_i <- sort(unique(df$Maha20))[i]
    
    if(skater_i %in% Cl_kp){
      
      df_plot <- df %>% filter(Maha20 %in% skater_i)
      
      df_plot %>% 
        ggplot(aes(x =  get(preds[j]) , y = Pred_D199_origUnits)) + 
        theme_minimal() +
        coord_cartesian(xlim = range(df[pred_j]), ylim = range(df$Pred_D199_origUnits))+
        theme(text=element_text(size=20)) +
        geom_smooth(data=df, span=.8, se=F, linewidth=2, alpha=0.3, col="gray85", method="loess")+
        
        geom_point(aes(shape=Type), size=2, col=cols[i]) + 
        scale_shape_manual(values = c(4, 1))+ 
        geom_smooth(aes(group = Maha20), span=1, se=F, linewidth=1.5, alpha=0.2, col=cols[i], linetype=1)+
        xlab(preds[j])
      
      # }
      
      ggsave(paste0(fig_dir, "LOESS_SKATER20/D199_fixed/", preds[j], "/D199_", preds[j], "_Cluster", skater_i, ".png"), width=10, height=6)
    }
  }
}