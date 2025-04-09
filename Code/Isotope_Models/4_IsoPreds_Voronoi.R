library(tidyverse)
# library(maps) # Other option for US map but less documentation for source
library(colorspace)
library(stringr)
# library(ggforce) # Does geom_voronoi_tile with max.radius option
library(deldir)
library(sf)
library(sfheaders)
library(tigris) # 2.2 in NAD83, default year of 2022
library(crsuggest)
library(spdep) # 1.3-5
library(sp) # used in blog example for coordinates() function
library(terra) # 1.7.78
library(ggpubr)

# Need to install tigris patch - now version 2.2
# pak::pak("walkerke/tigris")


output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

dir.create(paste0(fig_dir, "Clusters"))
cluster_dir <- paste0(fig_dir, "Clusters")

# Read in and join Ryan's cluster numbers and colors
New_ColorsNumbers <- read.csv("Tables/Maha-reassign.csv") %>% 
  dplyr::select(Maha20, New_Maha, Color_code) %>% 
  mutate(Color_code = paste0("#", Color_code)) %>% 
  mutate(Color_code = ifelse(Color_code=="#56608", "#056608", Color_code),
         Maha20 = as.factor(Maha20),
         New_Maha = as.factor(New_Maha))

head(New_ColorsNumbers)

# Read in predictions from Predict_ISO_MVRF_subset.R
df <- read.csv(paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24.csv"))
df <- df %>% rename(WetLossConv=WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s)

preds <- names(df)[2:11]


# Convert data frame to sf object
# These are NAD83 (NARS standard)
# NAD83 = North American Datum of 1983
# NAD83 (EPSG:4269)
df_sf <- df %>% st_as_sf(coords = c("LON_DD83", "LAT_DD83"), crs="EPSG:4269")
# 4269 units are degrees. Want meters
suggest_crs(df_sf, type="projected", units = "m") 
# Suggests NAD83(2011/Conus Alberts, crs_type is projected
# crs_code 6350






#### Use tigris package census maps, NAD83 ####
states_tig <- tigris::states(year=2022) %>% filter(!NAME %in% c("American Samoa", "Commonwealth of the Northern Mariana Islands", "Guam" , "United States Virgin Islands", "Puerto Rico", "Alaska", "Hawaii"))
# states_tig$NAME[order(states_tig$NAME)]

# plot(states_tig["REGION"]) # Can see portions of GL are included
ggplot() +
  geom_sf(data = states_tig, fill = NA, color = gray(.5)) + theme_void()

st_crs(states_tig) # 4269
# states_tig$geometry

# Includes portions of GL and FL looks weird, but try for now
# Both approaches for dissolving state boundaries below seem useful? 
# us_tig allows split to df and then into lists of data frames by polygon
# us_tig_union allows st_cast to split into polygons and then select one
us_tig <- summarise(states_tig)
us_tig_union <- st_union(states_tig) # just geometry
# rm(states_tig)

str(us_tig)
str(us_tig_union)

plot(us_tig) # Same plot
# plot(us_tig_union)
# us_tig_union = us_tig$geometry
# Coordinates for first polygon
# us_tig$geometry[[1]][[1]]
# # Same as
# us_tig_union[[1]][[1]]


### Remove 8 island polygons from us_tig_union
main_tig <- st_cast(us_tig_union, "POLYGON")[1]

# Convert sf object to df with polygon_ids
# Best for plotting with deldir plot functions
# us_df <- sf_to_df(us_tig)
# List of all individual polygons in US census map - with x-y coords
# ***Not sure if I need this still***
# us.tig.list <- split(us_df, f=us_df$polygon_id )
# str(us.tig.list)
# The mainland is the first polygon 




# Calculate Voronoi Tesselation and tiles
### **** To do: look into spherical voronoi calculations ************



# Can use terra to do voronoi
# https://stackoverflow.com/questions/47203587/r-delimit-a-voronoi-diagram-according-to-a-map
# Suggested projecting to planar reference system - 
# Need to be done for NAD83 because it is geodetic?


#### Using terra (see above) so that can give it a spatial object and project *********
# Project to 6350
# Note that 4269 is geodetic not projected
# Project to 6350
df_sf_proj <- st_transform(df_sf, crs="EPSG:6350")
df_sf_proj$geometry

main_proj <- st_transform(main_tig, crs="EPSG:6350")

# lakes
df_vect <- vect(df_sf_proj)
plot(df_vect)

# US poly
main_vect <- vect(main_proj)
plot(main_vect)

# tesselation
vor_terra <- terra::voronoi(df_vect, bnd=main_vect)
plot(vor_terra)

### Need to do clustering on clipped polygons because polygons can touch outside the boundaries of the US, which we don't want 
vor_terra_crp <- crop(vor_terra, main_vect)
plot(vor_terra_crp)
plot(df_vect, add=T)

vor_ter_sf_proj <-  sf::st_as_sf(vor_terra_crp)

# Project back to 4269
vor_ter_sf <- st_transform(vor_ter_sf_proj, crs="EPSG:4269")


# vor_ter_sf is data with voronoi polys
# df_sf is data with points
vor_ter_sf <- vor_ter_sf %>% arrange(NLA12_ID)
df_sf <- df_sf %>% arrange(NLA12_ID)


# Find largest polygon for each multipolygon and replace its geometry with just that single polygon
unique(st_geometry_type(vor_ter_sf))

# Four are multipolygons
which(st_geometry_type(vor_ter_sf)=="MULTIPOLYGON")
# 27 444 796 885

# INELEGANT BUT DOES THE JOB: deal with each multipolygon manually

# 27
plot(st_geometry(vor_ter_sf)[27], border=gray(.5))
plot(st_geometry(df_sf)[27], add=T)
# Plot first polygon - confirm largest
plot(st_cast(st_geometry(vor_ter_sf)[27], "POLYGON")[1])
plot(st_geometry(df_sf)[27], add=T)
# Replace geometry
st_geometry(vor_ter_sf)[27] <- st_cast(st_geometry(vor_ter_sf)[27], "POLYGON")[1]
plot(st_geometry(vor_ter_sf)[27], border=gray(.5))

# 444
plot(st_geometry(vor_ter_sf)[444], border=gray(.5))
plot(st_geometry(df_sf)[444], add=T)
# Plot first polygon - confirm largest
plot(st_cast(st_geometry(vor_ter_sf)[444], "POLYGON")[1])
plot(st_geometry(df_sf)[444], add=T)
# Replace geometry
st_geometry(vor_ter_sf)[444] <- st_cast(st_geometry(vor_ter_sf)[444], "POLYGON")[1]
plot(st_geometry(vor_ter_sf)[444], border=gray(.5))

# 796
plot(st_geometry(vor_ter_sf)[796], border=gray(.5))
plot(st_geometry(df_sf)[796], add=T)
# Plot first polygon - confirm largest
plot(st_cast(st_geometry(vor_ter_sf)[796], "POLYGON")[1])
plot(st_geometry(df_sf)[796], add=T)
# Replace geometry
st_geometry(vor_ter_sf)[796] <- st_cast(st_geometry(vor_ter_sf)[796], "POLYGON")[1]
plot(st_geometry(vor_ter_sf)[796], border=gray(.5))

# 885
plot(st_geometry(vor_ter_sf)[885], border=gray(.5))
plot(st_geometry(df_sf)[885], add=T)
# Plot THIRD polygon - confirm largest
plot(st_cast(st_geometry(vor_ter_sf)[885], "POLYGON")[3])
plot(st_geometry(df_sf)[885], add=T)
# Replace geometry
st_geometry(vor_ter_sf)[885] <- st_cast(st_geometry(vor_ter_sf)[885], "POLYGON")[3]
plot(st_geometry(vor_ter_sf)[885], border=gray(.5))





# Plot voronoi with points and polys colored by D199 prediction
ggplot(data=vor_ter_sf, aes(fill=Pred_D199_origUnits)) + 
  geom_sf(col="white") +
  theme_void() +
  theme(legend.position="bottom") +
  geom_sf(data=df_sf, col="white", size=1)

# Can also plot this way
plot(vor_ter_sf["Pred_D199_origUnits"], axes=T, max.plot=16)



# Plot 10 predictors, Omernik_II, HUC2, 3 isotope predictions spatially
plot_vars <- c(preds, "Omernik_II", "HUC2", "Pred_D199_origUnits", "Pred_D200_origUnits", "Pred_D202_origUnits")
plot(vor_ter_sf[plot_vars], max.plot=16)


str(vor_ter_sf)
vor_ter_sf$geometry




#  **** To do?? ****
# *** This would be place to crop each polygon to a radius around the lake ***

# How to do max radius like in ggplot2? Comes from ggforce package
# # Plot with max radius around each point - now those point polygons are estimated?
# ggplot(df, aes(x=LON_DD83, y=LAT_DD83, group = -1L)) +
#   geom_voronoi_tile(aes(fill=Omernik_II),
#                     max.radius = 1,
#                     colour = "black",
#                     bound=USA_bound) +
#   geom_point(aes(x=LON_DD83,y=LAT_DD83)) +
#   theme_void()+ 
#   geom_polygon( data=USA, aes(x=long, y=lat, group=group), color="gray80", fill=NA )



## Have cropped polygons created in planar geometry
# It is slightly different from voronoi_poly_clip from previous code (created with lat/longs)

# And note these are polygons, not multipolygons!
# Previously needed to figure out how clustering would work with multipolygons (need to drop pieces without a point inside them# Needs to not treat each polygon as if it's a separate observation and use the lake information twice)
# But now I think it's not an issue













# Try SKATER algorithm in spdep package
# https://www.dshkol.com/post/spatially-constrained-clustering-and-regionalization/
nla_scaled <- vor_ter_sf %>% dplyr::select(all_of(preds)) %>% 
  st_drop_geometry() %>%
  mutate(across(.cols = Hg0DryDep:LOI_PERCENT,
                .fns = ~scale(.))) 
# hist(x=nla_scaled$Hg0DryDep)


# ct_dat = vor_ter_sf
# ct_scaled = nla_scaled

# Construct neighbors list
nla_nb <- poly2nb(as_Spatial(vor_ter_sf))
# nla_nb_noqueen <- poly2nb(as_Spatial(vor_ter_sf), queen = FALSE)

# plot(as_Spatial(vor_ter_sf), main = "Without queen")
# plot(nla_nb_noqueen, coords = coordinates(as_Spatial(vor_ter_sf)), col="blue", add = TRUE)

# Not really any difference between the two (no polys meet only at corner)
# Keep queen - possible diagonal regions could be connnected at corners
plot(as_Spatial(vor_ter_sf), main = "Polygon centroids")
# plot(nla_nb_noqueen, coords = coordinates(as_Spatial(vor_ter_sf)), col="blue", add = TRUE)
plot(nla_nb, coords = coordinates(as_Spatial(vor_ter_sf)), col="blue", add = TRUE)
# Plotting the centroid, not the lake itself

plot(as_Spatial(vor_ter_sf), main = "Lake locations")
plot(nla_nb, coords = coordinates(as_Spatial(df_sf)), col="red", add = TRUE)



# 20 colors for plotting
# cols2 <- sequential_hcl(5, palette = "Light Grays")
# cols3 <- qualitative_hcl(17, palette = "Dark 3")
# set.seed(5)
# cols <- sample(c(cols3, cols2[-c(4,5)]))




# Run SKATER #####

# 'nbcosts' provides distances for euclidian, manhattan, canberra, binary, minkowski, and mahalanobis, and defaults to euclidean if not specified
# method = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "mahalanobis")

# Edge costs/distances - Try first with euclidean distance
# costs_eucl <- nbcosts(nla_nb, data = nla_scaled, method = c("euclidean"))
# Shouldn't need to be scaled for mahalanobis

# 'nb2listw' transforms the edge costs into spatial weights to supplement the neighbour list, and then is fed into 'mstree'.
# nla_w_eucl <- nb2listw(nla_nb, costs_eucl, style="B") # style B is binary coding

# 'mstree'  creates the minimal spanning tree that turns the adjacency graph into a subgraph with n nodes and n-1 edges
# nla_mst_eucl <- mstree(nla_w_eucl)

# Edges with higher dissimilarity are removed sequentially until left with a spanning tree that takes the minimum sum of dissimilarities across all edges of the tree, hence minimum spanning tree.
# plot(st_geometry(vor_ter_sf), border=gray(.5), main="Euclidean spanning tree")
# plot(nla_mst_eucl, coordinates(as_Spatial(df_sf)),col="blue", cex.lab=0.6, cex.circles=0.035, add=TRUE)



# Once the minimum spanning tree is in place, the SKATER algorithm comes in to partition the MST. The paper details the algorithm in full for those interested, but in short it works by iteratively partitioning the graph by identifying which edge to remove to maximize the quality of resulting clusters as measured by the sum of the intercluster square deviations SSD. Regions that are similar to one another will have lower values.
# This is implemented via spdep::skater and the ncuts arg indicates the number of partitions to make, resulting in ncuts+1 groups.
# numclust <- 10
# clus10_eucl <- skater(edges = nla_mst_eucl[,1:2], data = nla_scaled, ncuts = numclust-1, method = c("euclidean"))
# 
# numclust <- 20
# clus20_eucl <- skater(edges = nla_mst_eucl[,1:2], data = nla_scaled, ncuts = numclust-1, method = c("euclidean"))
# 
# 
# ### groups size
# table(clus10_eucl$groups)
# table(clus20_eucl$groups)
# 
# clusters <- vor_ter_sf %>% mutate(Euc10 = as.factor(clus10_eucl$groups),
#                                   Euc20 = as.factor(clus20_eucl$groups))
# 
# plot(clusters['Euc10'], main = "Euclidean - 10")
# plot(clusters['Euc20'], main = "Euclidean - 20")
# 
# 
# 
# 
# # Plot voronoi with points and polys colored by cluster
# e10polys <- ggplot() +
#   geom_sf(data=clusters, aes(fill=Euc10), col="white") +
#   theme_void() +
#   theme(legend.position="bottom") +
#   geom_sf(data=df_sf, col="white", size=.75) +
#   scale_fill_manual(values = cols)
# 
# e10states <- ggplot() + 
#   geom_sf(data=clusters, aes(fill=Euc10), col=NA) +
#   theme_void() +
#   theme(legend.position="bottom") +
#   geom_sf(data=df_sf, col="white", size=.75) +
#   geom_sf(data=states_tig, col="white", fill=NA) +
#   scale_fill_manual(values = cols)
# 
# # 20
# e20polys <- ggplot() +
#   geom_sf(data=clusters, aes(fill=Euc20), col="white") +
#   theme_void() +
#   theme(legend.position="bottom") +
#   geom_sf(data=df_sf, col="white", size=.75)+
#   scale_fill_manual(values = cols)
# 
# e20states <- ggplot() + 
#   geom_sf(data=clusters, aes(fill=Euc20), col=NA) +
#   theme_void() +
#   theme(legend.position="bottom") +
#   geom_sf(data=df_sf, col="white", size=.75) +
#   geom_sf(data=states_tig, col="white", fill=NA)+
#   scale_fill_manual(values = cols)
# 
# e20states





#### Try mahalanobis distance for mst and skater ####
# Uses mahalanobis() function. Needs cov
cov_pred <- cov(nla_scaled)
# Edge costs/distances
costs_maha <- nbcosts(nb=nla_nb, data = nla_scaled, method = c("mahalanobis"), cov=cov_pred)
# Columns don't actually need to be scaled to do mahalanobis, but won't matter

# 'nb2listw' transforms the edge costs into spatial weights to supplement the neighbour list, and then is fed into 'mstree'. 
nla_w_maha <- nb2listw(nla_nb, costs_maha, style="B") # style B is binary coding

# 'mstree'  creates the minimal spanning tree that turns the adjacency graph into a subgraph with n nodes and n-1 edges
set.seed(38)
nla_mst_maha <- mstree(nla_w_maha)


# Edges with higher dissimilarity are removed sequentially until left with a spanning tree that takes the minimum sum of dissimilarities across all edges of the tree, hence minimum spanning tree. 
plot(st_geometry(vor_ter_sf), border=gray(.5), main="Mahalanobis spanning tree")
plot(nla_mst_maha, coordinates(as_Spatial(df_sf)),col="blue", cex.lab=0.6, cex.circles=0.035, add=TRUE)

# Once the minimum spanning tree is in place, the SKATER algorithm comes in to partition the MST. The paper details the algorithm in full for those interested, but in short it works by iteratively partitioning the graph by identifying which edge to remove to maximize the quality of resulting clusters as measured by the sum of the intercluster square deviations SSD. Regions that are similar to one another will have lower values. 
# This is implemented via spdep::skater and the ncuts arg indicates the number of partitions to make, resulting in ncuts+1 groups.
numclust <- 10
clus10_maha <- skater(edges = nla_mst_maha[,1:2], data = nla_scaled, ncuts = numclust-1, method = c("mahalanobis"), cov=cov_pred)
head(clus10_maha$groups)

numclust <- 20
clus20_maha <- skater(edges = nla_mst_maha[,1:2], data = nla_scaled, ncuts = numclust-1, method = c("mahalanobis"), cov=cov_pred)
# sum(clus20_maha$groups != df_clust_write$Maha20)

numclust <- 30
clus30_maha <- skater(edges = nla_mst_maha[,1:2], data = nla_scaled, ncuts = numclust-1, method = c("mahalanobis"), cov=cov_pred)

# numclust <- 40
# clus40_maha <- skater(edges = nla_mst_maha[,1:2], data = nla_scaled, ncuts = numclust-1, method = c("mahalanobis"), cov=cov_pred)

numclust <- 100
clus100_maha <- skater(edges = nla_mst_maha[,1:2], data = nla_scaled, ncuts = numclust-1, method = c("mahalanobis"), cov=cov_pred)

table(clus10_maha$groups)
table(clus20_maha$groups) # Too few in each cluster
table(clus30_maha$groups) # Too few
# table(clus40_maha$groups) # Too few
table(clus100_maha$groups) # Too few


clusters <- vor_ter_sf %>% mutate(Maha10 = as.factor(clus10_maha$groups),
                                Maha20 = as.factor(clus20_maha$groups),
                                Maha30 = as.factor(clus30_maha$groups),
                                # Maha40 = as.factor(clus40_maha$groups),
                                Maha100 = as.factor(clus100_maha$groups))
                              
clusters
  
point_clusters <- df_sf %>% mutate(Maha10 = as.factor(clus10_maha$groups),
                                   Maha20 = as.factor(clus20_maha$groups),
                                   Maha30 = as.factor(clus30_maha$groups),
                                   # Maha40 = as.factor(clus40_maha$groups),
                                   Maha100 = as.factor(clus100_maha$groups))

# clusters <- clusters %>% mutate(Maha10 = as.factor(clus10_maha$groups),
                                  # Maha20 = as.factor(clus20_maha$groups),
                                  # Maha30 = as.factor(clus30_maha$groups))

plot(clusters['Maha10'], main = "Mahalanobis - 10", key.pos=NULL, pal=rainbow(10))
plot(clusters['Maha20'], main = "Mahalanobis - 20", key.pos=NULL, pal=rainbow(20))
plot(clusters['Maha30'], main = "Mahalanobis - 30", key.pos=NULL, pal=rainbow(30))
# plot(clusters['Maha40'], main = "Mahalanobis - 40", key.pos=NULL, pal=rainbow(40))
plot(clusters['Maha100'], main = "Mahalanobis - 100", key.pos=NULL, pal=rainbow(100))



# ssw() returns sum of mahalanobis distances within each cluster, using pooled cov matrix. So could use elbow method
plot(clus10_maha$ssw)
plot(diff(clus10_maha$ssw))

png(paste0(cluster_dir, "/Dissimilarity_vs_Clusters.png"), width = 600, height = 600)
par(mfrow=c(2,1), las=1)
plot(clus100_maha$ssw, xlab="Number clusters", ylab="Sum dissimilarity")
abline(v=20, lty=2)
plot(2:100, (-1)*diff(clus100_maha$ssw), xlab="Number clusters", ylab="Reduction in dissimilarity")
abline(v=20, lty=2)
dev.off()
# Use 20 clusters



# OLD COLORS - 20 colors for plotting
# cols2 <- sequential_hcl(5, palette = "Light Grays")
# cols3 <- qualitative_hcl(17, palette = "Dark 3")
# set.seed(13) # 5
# cols <- sample(c(cols3, cols2[-c(4,5)]))



# Plot voronoi with points and polys colored by cluster
# m10polys <- ggplot() +
#   geom_sf(data=clusters, aes(fill=Maha10), col="white") +
#   theme_void() +
#   theme(legend.position="bottom") +
#   geom_sf(data=df_sf, col="white", size=.75) +
#   scale_fill_manual(values = cols)
# 
# m10states <- ggplot() + 
#   geom_sf(data=clusters, aes(fill=Maha10), col=NA) +
#   theme_void() +
#   theme(legend.position="bottom") +
#   geom_sf(data=df_sf, col="white", size=.75) +
#   geom_sf(data=states_tig, col="white", fill=NA) +
#   scale_fill_manual(values = cols)


# Join new cluster numbers and colors to polys and points

clusters_orig <- clusters
point_clusters_orig <- point_clusters

# clusters <- clusters_orig
# point_clusters <- point_clusters_orig

clusters <- left_join(clusters, New_ColorsNumbers) %>% 
  dplyr::select(-c("Maha10", "Maha20", "Maha30", "Maha100")) %>% 
  rename(Maha20=New_Maha)

point_clusters <- left_join(point_clusters, New_ColorsNumbers) %>% 
  dplyr::select(-c("Maha10", "Maha20", "Maha30", "Maha100")) %>% 
  rename(Maha20=New_Maha)

# Centroid for labeling
centroid <- point_clusters %>% group_by(Maha20) %>% summarise(centroid=st_union(geometry)) %>% st_centroid() 

head(clusters)

New_ColorsNumbers

scale_color_KV <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(New_ColorsNumbers$Color_code, New_ColorsNumbers$New_Maha)
  )
}
scale_fill_KV <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(New_ColorsNumbers$Color_code, New_ColorsNumbers$New_Maha)
  )
}

scale_color_TEXT <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(c(rep("white", 3), "black", "black", "white", "black", "black","white", rep("black", 11)), 1:20)
  )
}


# 20
m20polys <- ggplot() +
  geom_sf(data=clusters, aes(fill=Maha20), col="white") +
  theme_void() +
  theme(legend.position="bottom") +
  geom_sf(data=df_sf, col="white", size=1)+
  # scale_fill_manual(values = cols) +
  scale_fill_KV() +
  geom_sf_text(data = centroid, aes(label = Maha20, color=Maha20), size=12, alpha=0.85,  show.legend = F) + # colour="black"
  scale_color_TEXT()

m20states <- ggplot() + 
  geom_sf(data=clusters, aes(fill=Maha20), col=NA) +
  theme_void() +
  theme(legend.position="bottom") +
  geom_sf(data=df_sf, col="white", size=1) +
  geom_sf(data=states_tig, col="white", fill=NA)+
  # scale_fill_manual(values = cols)+
  scale_fill_KV() +
  geom_sf_text(data = centroid, aes(label = Maha20, color=Maha20), size=12, alpha=0.85,  show.legend = F) + # colour="black"
  scale_color_TEXT()

# Plot Mahalanobis 20 cluster with lake and state polys
ggarrange(m20polys + 
            # ggtitle("Skater-Mahalanobis-20clust") + 
            theme(legend.position="none",
                  plot.title = element_text(hjust = 0.5, size=40),
                  plot.margin = unit(c(0,0,0,0), 'lines')),
          m20states + 
            # ggtitle("Skater-Mahalanobis-20clust") +
            theme(legend.position="none", 
                  plot.title = element_text(hjust = 0.5, size=40),
                  plot.margin = unit(c(0,0,0,0), 'lines')),
          ncol=1, 
          nrow=2)
ggsave(paste0(cluster_dir, "/SKATER_Mahalanobis_20clust_NEWCOLORS.png"),
       width=20, height=20)

# Save just figure with state outline
m20states+ 
  # ggtitle("Skater-Mahalanobis-20clust") + 
  theme(legend.position="none")
ggsave(paste0(cluster_dir, "/SKATER_Mahalanobis_20clust_StateOutlines_NEWCOLORS.png"),
       width=20, height=10)  

       



# Multiple plots
plot_vars <- c(preds, "Pred_D199_origUnits", "Pred_D200_origUnits", "Pred_D202_origUnits") #,  "Pred_D200_origUnits", "Pred_D202_origUnits") "Maha20", "Omernik_II",  
png(paste0(cluster_dir, "/Predictors_and_Iso.png"),
    width=3000, height=2000, pointsize=40)
plot(clusters[plot_vars], max.plot=15)
dev.off()



# Write clusters to original file - keeping original numbers
clusters_sub <- st_drop_geometry(clusters_orig) %>% dplyr::select(NLA12_ID, Maha10, Maha20, Maha30)
df_clust_write <- left_join(df, clusters_sub)
write.csv(df_clust_write, paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv"), row.names = F)
# df_clust_write2 <- read.csv(paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS.csv"))
# head(df_clust_write)


# Write clusters to original file - with new cluster numbers
clusters_sub2 <- st_drop_geometry(clusters) %>% dplyr::select(NLA12_ID, Maha20)
df_clust_write2 <- left_join(df, clusters_sub2)
write.csv(df_clust_write2, paste0(output_dir, "Isotope_Predictions_All_Lakes_FINALFINALMOD_2024-01-24_WITH_SKATER_CLUSTERS_NEWnumbers.csv"), row.names = F)


# Write clusters and as shapefile for other plotting - sent to Mike at USGS for plotting cluster outlines (numbers don't matter here)
# output_dir_shapefile <- "Model_Output/Iso/Shapefiles"
# dir.create(output_dir_shapefile)
# 
# # Write lake polygons with Maha20 ID - NEW CLUSTERS
# clusters_shp <- clusters # %>% dplyr::select(-Maha10, -Maha30, -Maha100)
# st_write(clusters_shp, paste0(output_dir_shapefile, "/Lake_VoronoiPolygons_with_Maha20ClusterID.shp"))
# 
# point_clusetrs_shp <- point_clusters # %>% dplyr::select(-Maha10, -Maha30, -Maha100)
# st_write(point_clusetrs_shp, paste0(output_dir_shapefile, "/Lake_Points_with_Maha20ClusterID.shp"))







# ggarrange(e10polys + ggtitle("Skater-Euclidean-10clust") + theme(legend.position="none", 
#                                                      plot.title = element_text(hjust = 0.5, size=40),
#                                                      plot.margin = unit(c(0,0,0,0), 'lines')), 
#           e20polys + ggtitle("Skater-Euclidean-20clust") + theme(legend.position="none", 
#                                                      plot.title = element_text(hjust = 0.5, size=40),
#                                                      plot.margin = unit(c(0,0,0,0), 'lines')), 
#           m10polys + ggtitle("Skater-Mahalanobis-10clust") + theme(legend.position="none", 
#                                                        plot.title = element_text(hjust = 0.5, size=40),
#                                                        plot.margin = unit(c(0,0,0,0), 'lines')), 
#           m20polys + ggtitle("Skater-Mahalanobis-20clust") + theme(legend.position="none", 
#                                                        plot.title = element_text(hjust = 0.5, size=40),
#                                                        plot.margin = unit(c(0,0,0,0), 'lines')), 
#           ncol=2, nrow=2)
# ggsave(paste0(cluster_dir, "/SKATER_LakePolys.png"),
#        width=40, height=20)
# 
# 
# ggarrange(e10polys + ggtitle("Skater-Euclidean-10clust") + theme(legend.position="none", 
#                                                                  plot.title = element_text(hjust = 0.5, size=40),
#                                                                  plot.margin = unit(c(0,0,0,0), 'lines')), 
#           e20polys + ggtitle("Skater-Euclidean-20clust") + theme(legend.position="none", 
#                                                                  plot.title = element_text(hjust = 0.5, size=40),
#                                                                  plot.margin = unit(c(0,0,0,0), 'lines')), 
#           m10polys + ggtitle("Skater-Mahalanobis-10clust") + theme(legend.position="none", 
#                                                                    plot.title = element_text(hjust = 0.5, size=40),
#                                                                    plot.margin = unit(c(0,0,0,0), 'lines')), 
#           m20polys + ggtitle("Skater-Mahalanobis-20clust") + theme(legend.position="none", 
#                                                                    plot.title = element_text(hjust = 0.5, size=40),
#                                                                    plot.margin = unit(c(0,0,0,0), 'lines')), 
#           ncol=2, nrow=2)
# ggsave(paste0(cluster_dir, "/SKATER_LakePolys.png"),
#        width=40, height=20)



# ggarrange(e10states + ggtitle("Skater-Euclidean-10clust") + theme(legend.position="none", 
#                                                      plot.title = element_text(hjust = 0.5, size=40),
#                                                      plot.margin = unit(c(0,0,0,0), 'lines')), 
#           e20states + ggtitle("Skater-Euclidean-20clust") + theme(legend.position="none", 
#                                                      plot.title = element_text(hjust = 0.5, size=40),
#                                                      plot.margin = unit(c(0,0,0,0), 'lines')), 
#           m10states + ggtitle("Skater-Mahalanobis-10clust") + theme(legend.position="none", 
#                                                        plot.title = element_text(hjust = 0.5, size=40),
#                                                        plot.margin = unit(c(0,0,0,0), 'lines')), 
#           m20states + ggtitle("Skater-Mahalanobis-20clust") + theme(legend.position="none", 
#                                                        plot.title = element_text(hjust = 0.5, size=40),
#                                                        plot.margin = unit(c(0,0,0,0), 'lines')), 
#           ncol=2, nrow=2)
# ggsave(paste0(cluster_dir, "/SKATER_StatePolys.png"),
#        width=40, height=20)
