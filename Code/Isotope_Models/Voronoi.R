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




# deldir package

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


# Calculate Voronoi Tesselation and tiles
### **** To do: look into spherical voronoi calculations ************









# Can use terra to do voronoi
# https://stackoverflow.com/questions/47203587/r-delimit-a-voronoi-diagram-according-to-a-map
# Suggested projecting to planar reference system - 
# I think this may need to be done for NAD83 because it is geodetic?


#### ***** Need to redo this using terra (see above) so that can give it a spatial object *********
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

vor_terra_crp <- crop(vor_terra, main_vect)
plot(vor_terra_crp)

vor_ter_sf_proj <-  sf::st_as_sf(vor_terra_crp)

# Project back to 4269
vor_ter_sf <- st_transform(vor_ter_sf_proj, crs="EPSG:4269")


plot(vor_ter_sf["Pred_D199_origUnits"], axes=T, max.plot=16)

str(vor_ter_sf)
vor_ter_sf$geometry

## Stopped here - Have cropped polygons created in planar geometry
# It is slightly different from voronoi_poly_clip below
# And note these are polygons, not multipolygons!


### SUCCESS!!








# tesselation <- deldir(df$LON_DD83, df$LAT_DD83)
# tiles <- tile.list(tesselation)
# 
# plot(tiles, pch = 19,
#      fillcol = hcl.colors(50, "Purple-Yellow"))
# 
# # Clip to US with deldir plot functions
# plot(tiles, pch = 19,
#      col.pts = "black",
#      border = "white",
#      fillcol = hcl.colors(50, "viridis"),
#      clipp = us.tig.list)
# 
# # Clip to just largest polygon - drops islands
# plot(tiles, pch = 19,
#      col.pts = "black",
#      border = "white",
#      fillcol = hcl.colors(50, "viridis"),
#      clipp = us.tig.list[[1]])
# 
# # str(tiles[[1]])
# # The boundary of the cell is in the polygon defined by the x and y components.
# 
# 
# 
# # Convert tiles to sf polygon object 
# voronoi_poly <- tiles %>%
#   purrr::map(~{cbind(x = .$x, y = .$y)} %>%
#                rbind(.[1,]) %>%
#                list() %>%
#                sf::st_polygon()) %>%
#   sf::st_sfc() %>%
#   sf::st_sf() %>%
#   dplyr::mutate(id = dplyr::row_number())
# 
# st_crs(voronoi_poly) <- "EPSG:4269"
# # NAD83 (EPSG:4269)


str(voronoi_poly)
plot(voronoi_poly)



# Join voronoi polys to original data
df_voronoi <- df %>% dplyr::mutate(id = dplyr::row_number())
df_voronoi <- left_join(df_voronoi, voronoi_poly, by=join_by(id))
str(df_voronoi)

sf_voronoi <- st_as_sf(df_voronoi)
str(sf_voronoi)

# Note also have df_sf with lake point geometry



# Clip the voronoi polys by US boundary using just largest polygon - but note contains GL and islands
voronoi_poly_clip <- st_intersection(sf_voronoi, main_tig)
# voronoi_poly_clip <- st_intersection(voronoi_poly, us_tig) # Can do with islands as well

head(voronoi_poly_clip) # Still a multipolygon for each lake, even after clipping using single polygon
names(voronoi_poly_clip)
str(voronoi_poly_clip)


# Plot 10 predictors, Omernik_II, HUC2, 3 isotope predictions spatially
plot_vars <- c(preds, "Omernik_II", "HUC2", "Pred_D199_origUnits", "Pred_D200_origUnits", "Pred_D202_origUnits")
plot(voronoi_poly_clip[plot_vars], max.plot=16)

### Need to do clustering on clipped polygons because polygons can touch outside the boundaries of the US, which we don't want (compare plots below)
plot(voronoi_poly_clip["Pred_D199_origUnits"], max.plot=16)
plot(sf_voronoi["Pred_D199_origUnits"], axes=T, max.plot=16)
plot(df_sf, add=T)

(df_sf$geometry)

voronoi_poly_clip$geometry

### Need to figure out how this works with multipolygons
# Drop polygons without a point inside them?
# Maybe it's okay? But needs to not treat each polygon as if it's a separate observation and use the lake information twice


# Try SKATER algorithm in spdep package
# https://www.dshkol.com/post/spatially-constrained-clustering-and-regionalization/
nla_scaled <- voronoi_poly_clip %>% dplyr::select(all_of(preds)) %>% 
  st_drop_geometry() %>%
  mutate(across(.cols = Hg0DryDep:LOI_PERCENT,
                .fns = ~scale(.))) 
hist(nla_scaled$Hg0ryDep)



nla_nb <- poly2nb(as_Spatial(voronoi_poly_clip))
nla_nb_noqueen <- poly2nb(as_Spatial(voronoi_poly_clip), queen = FALSE)
# Not really any difference between the two (no polys meet only at corner)

# plot(as_Spatial(voronoi_poly_clip), main = "With queen")
# plot(nla_nb, coords = coordinates(as_Spatial(voronoi_poly_clip)), col="blue", add = TRUE)

#
plot(as_Spatial(voronoi_poly_clip), main = "Without queen")
# plot(nla_nb, coords = coordinates(as_Spatial(voronoi_poly_clip)), col="blue", add = TRUE)
plot(nla_nb_noqueen, coords = coordinates(as_Spatial(voronoi_poly_clip)), col="red", add = TRUE)



# ggplot(df, aes(x=LON_DD83, y=LAT_DD83, group = -1L)) +
#   geom_voronoi_tile(aes(fill=Omernik_II),
#                     max.radius = 1,
#                     colour = "black",
#                     bound=USA_bound) +
#   geom_point(aes(x=LON_DD83,y=LAT_DD83)) +
#   theme_void()+
#   geom_polygon( data=USA, aes(x=long, y=lat, group=group), color="gray80", fill=NA )
