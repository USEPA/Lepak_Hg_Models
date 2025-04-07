# MainStates <- map_data("state")
USA <- map_data("usa") # calls maps::map and formats data
# usmap <- map("usa")
# This database produces a map of the United States mainland generated from US Department of the Census data (see the reference).
# US Department of Commerce, Census Bureau, County Boundary File, computer tape, available from Customer Services, Bureau of the Census, Washingdon DC 20233.
# UNCLEAR WHAT THE YEAR IS THOUGH AND THE PROJECTION


USAtemp <- USA
names(USAtemp)[1:2] <- c("x", "y") # long, lat

# USA.list splits the maps package data for US into a list by group and renames coords to x and y
USA.list <- split(USAtemp, f=USAtemp$group)

# USA_bound is just the single main polygon and renamed coords to x and y
USA_bound <- USAtemp %>% filter(region=="main") %>% arrange(order)
names(USA_bound) <- c("x", "y", "order")




## INITIAL VISUALIZATIONS WITH GGPLOT

# Plot each lake location on map
ggplot(df, aes(x=LON_DD83, y=LAT_DD83)) +
  geom_polygon( data=USA, aes(x=long, y=lat, group=group), color="gray80", fill=NA ) +
  geom_point(size=2, col="gray70", shape=21) +
  theme_void() 

# Plot
# geom_voronoi_tile uses deldir

# # Voronoi without US boundary
# ggplot(df, aes(x=LON_DD83, y=LAT_DD83, group = -1L)) +
#   geom_voronoi_tile(aes(fill=Omernik_II),
#                     # max.radius = 10,
#                     colour = "black") +
#   geom_point(aes(x=LON_DD83,y=LAT_DD83))+
#   theme_void() +
#   geom_polygon( data=USA_bound, aes(x=x, y=y, group=-1L), color="gray80", fill=NA )
# 
# 
# # Voronoi clipped to contiguous US without islands - area in NY-ish not filling in 
# ggplot(df, aes(x=LON_DD83, y=LAT_DD83, group = -1L)) +
#   geom_voronoi_tile(aes(fill=Omernik_II),
#                     # max.radius = 10,
#                     colour = "black",
#                     bound=USA_bound) +
#   geom_point(aes(x=LON_DD83,y=LAT_DD83))+
#   theme_void() +
#   geom_polygon( data=USA, aes(x=long, y=lat, group=group), color="gray80", fill=NA )
# 
# 
# # Plot with max radius around each point - now those point polygons are estimated?
# ggplot(df, aes(x=LON_DD83, y=LAT_DD83, group = -1L)) +
#   geom_voronoi_tile(aes(fill=Omernik_II),
#                     max.radius = 1,
#                     colour = "black",
#                     bound=USA_bound) +
#   geom_point(aes(x=LON_DD83,y=LAT_DD83)) +
#   theme_void()+ 
#   geom_polygon( data=USA, aes(x=long, y=lat, group=group), color="gray80", fill=NA )


######################

tesselation <- deldir(df$LON_DD83, df$LAT_DD83)
tiles <- tile.list(tesselation)

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

# Clip to US with islands
plot(tiles, pch = 19,
     col.pts = "black",
     border = "white",
     fillcol = hcl.colors(50, "viridis"),
     clipp = USA.list)

# Clip to US without islands
plot(tiles, pch = 19,
     col.pts = "black",
     border = "white",
     fillcol = hcl.colors(50, "viridis"),
     clipp = USA_bound)

# str(tiles[[1]])
# The boundary of the cell is in the polygon defined by the x and y components.



# Convert tiles to sf polygon object
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



### Need to do clustering on clipped polygons because polygons can touch outside the boundaries of the US, which we don't want (compare plots below)
plot(voronoi_poly_clip["Pred_D199_origUnits"], max.plot=16)
plot(sf_voronoi["Pred_D199_origUnits"], axes=T, max.plot=16)

(df_sf$geometry)

voronoi_poly_clip$geometry






# This stuff not working
#### Use maps package census map, unsure if NAD83 but probably ####
### FOR NOW, ASSUMING USA MAP FROM maps IS NAD83 BECAUSE IT IS A CENSUS MAP
# USA_sf <- sfheaders::sf_multipolygon(obj=USA, polygon_id = "group", x="long", y="lat")
# st_crs(USA_sf) <- "EPSG:4269"

# Try for bound - this made a points object
# USA_bound_sf <- USA_bound %>% st_as_sf(coords = c("x", "y"), crs="EPSG:4269")
# USA_bound_sf <- sfheaders::sf_polygon(obj=USA_bound, x= "x", y="y")
# st_crs(USA_bound_sf) <- "EPSG:4269"
# USA_bound_sf2 <- summarise(USA_bound_sf)
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 6437 crosses edge 6439 

# Didn't work witith multipolygon or polygon approach without islands approach
# voronoi_poly_clip2 <- st_intersection(voronoi_poly, USA_sf)
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
# Loop 0 is not valid: Edge 6437 crosses edge 6439

# voronoi_poly_clip2 <- st_intersection(voronoi_poly, USA_bound_sf)

# plot(USA_bound_sf)
# str(USA_bound_sf)

# plot(voronoi_poly_clip2)












# st_as_sf.deldir <- function(dd, extract = c("tiles", "triangles")) {
#   extract <- match.arg(extract)
#   
#   if (extract == "tiles") {
#     ddlist <- deldir::tile.list(dd)
#   }
#   else if (extract == "triangles") {
#     ddlist <- deldir::triang.list(dd)
#   }
#   
#   ddlist %>%
#     purrr::map(~{cbind(x = .$x, y = .$y)} %>%
#                  rbind(.[1,]) %>%
#                  list() %>%
#                  sf::st_polygon()) %>%
#     sf::st_sfc() %>%
#     sf::st_sf() %>%
#     dplyr::mutate(id = dplyr::row_number()) %>%
#     return()
# }


# df$x <- df$LON_DD83
# df$y <- df$LAT_DD83


# g <- ggplot(df, aes(x = x, y = y)) + geom_point(col = "black")
# for (i in 1:50){
#   g <- g + geom_polygon(data = data.frame(x = tiles[[i]]$x,
#                                           y = tiles[[i]]$y,
#                                           density = 1/tiles[[i]]$area), 
#                         aes(fill = density))
# }
# g