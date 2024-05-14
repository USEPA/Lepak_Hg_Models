library(tidyverse)
library(maps)
library(colorspace)
library(stringr)
library(ggforce)
library(deldir)
library(sf)
library(sfheaders)
library(deldir)
library(tigris) # in NAD83, default year of 2022

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


#### Use tigris package census maps, NAD83 ####
states_tig <- tigris::states(year=2022) %>% filter(!NAME %in% c("American Samoa", "Commonwealth of the Northern Mariana Islands", "Guam" , "United States Virgin Islands", "Puerto Rico", "Alaska", "Hawaii"))
# states_tig$NAME[order(states_tig$NAME)]

plot(states_tig["REGION"])
ggplot() +
  geom_sf(data = states_tig, fill = NA, color = gray(.5)) + theme_void()


str(states_tig)

# Includes portions of GL and FL looks weird, but try for now
# Both approaches below seem useful? 
# us_tig allows split to df and then into lists of data frames by polygon
# us_tig_union allows st_cast to split into polygons and then select one
us_tig <- summarise(states_tig)
us_tig_union <- st_union(states_tig) # just geometry

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

# How to do max radius like in ggplot2?
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
tesselation <- deldir(df$LON_DD83, df$LAT_DD83)
tiles <- tile.list(tesselation)

plot(tiles, pch = 19)
plot(tiles, pch = 19,
     fillcol = hcl.colors(50, "Purple-Yellow"))

# Clip to US with deldir plot functions
plot(tiles, pch = 19,
     col.pts = "black",
     border = "white",
     fillcol = hcl.colors(50, "viridis"),
     clipp = us.tig.list)

# Clip to just largest polygon - drops islands
plot(tiles, pch = 19,
     col.pts = "black",
     border = "white",
     fillcol = hcl.colors(50, "viridis"),
     clipp = us.tig.list[[1]])

# str(tiles[[1]])
# The boundary of the cell is in the polygon defined by the x and y components.







# Convert tiles to sf polygon object & clip
voronoi_poly <- tiles %>%
  purrr::map(~{cbind(x = .$x, y = .$y)} %>%
               rbind(.[1,]) %>%
               list() %>%
               sf::st_polygon()) %>%
  sf::st_sfc() %>%
  sf::st_sf() %>%
  dplyr::mutate(id = dplyr::row_number())

st_crs(voronoi_poly) <- "EPSG:4269"
# NAD83 (EPSG:4269)


str(voronoi_poly)
plot(voronoi_poly)



# Join voronoi polys to original data
df_voronoi <- df %>% dplyr::mutate(id = dplyr::row_number())

df_voronoi <- left_join(df_voronoi, voronoi_poly)
str(df_voronoi)

df_voronoi$geometry


# Note also have df_sf with lake point geometry


# Write ggplot2 code that plots the voronoi polygons, clipped to US boundary with lake points added (but whole voronoi polygon is kept)





##########################################################
### ** Do this after the clustering - i.e., cluster the larger polygons and then clip ####
### ** Otherwise, get multipolygon associated with each lake
# Clip the voronoi polys by US boundary using just largest polygon - but note contains GL and islands
# voronoi_poly_clip <- st_intersection(voronoi_poly, us_tig) # Can do with islands as well
voronoi_poly_clip <- st_intersection(voronoi_poly, main_tig)

plot(voronoi_poly)
plot(voronoi_poly_clip)

head(voronoi_poly_clip) # Still a multipolygon for each lake, even after clipping using single polygon





