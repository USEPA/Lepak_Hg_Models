library(tidyverse)
# library(maps) # Other option for US map but less documentation for source
library(colorspace)
library(stringr)
# library(ggforce) # Does geom_voronoi_tile with max.radius option
library(deldir)
library(sf)
library(sfheaders)
library(tigris) # in NAD83, default year of 2022
library(crsuggest)
library(spdep)
library(sp) # used in blog example for coordinates() function
library(terra)


output_dir <- "Model_Output/Iso/"
model_dir <- "Saved_Models/Iso/"
fig_dir <- "Figures/Iso/"

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
rm(states_tig)

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
us_df <- sf_to_df(us_tig)
# List of all individual polygons in US census map - with x-y coords
us.tig.list <- split(us_df, f=us_df$polygon_id )
str(us.tig.list)
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

df_vect <- vect(df_sf_proj)
plot(df_vect)

main_vect <- vect(main_proj)
plot(main_vect)

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
hist(x=nla_scaled$Hg0DryDep)



nla_nb <- poly2nb(as_Spatial(vor_ter_sf))
nla_nb_noqueen <- poly2nb(as_Spatial(vor_ter_sf), queen = FALSE)

# plot(as_Spatial(vor_ter_sf), main = "With queen")
# plot(nla_nb, coords = coordinates(as_Spatial(vor_ter_sf)), col="blue", add = TRUE)

# Not really any difference between the two (no polys meet only at corner)
# Prefer no queen so don't have clusters formed of polys meeting at diagonal
plot(as_Spatial(vor_ter_sf), main = "Without queen")
# plot(nla_nb, coords = coordinates(as_Spatial(vor_ter_sf)), col="blue", add = TRUE)
plot(nla_nb_noqueen, coords = coordinates(as_Spatial(vor_ter_sf)), col="red", add = TRUE)






# ggplot(df, aes(x=LON_DD83, y=LAT_DD83, group = -1L)) +
#   geom_voronoi_tile(aes(fill=Omernik_II),
#                     max.radius = 1,
#                     colour = "black",
#                     bound=USA_bound) +
#   geom_point(aes(x=LON_DD83,y=LAT_DD83)) +
#   theme_void()+
#   geom_polygon( data=USA, aes(x=long, y=lat, group=group), color="gray80", fill=NA )
