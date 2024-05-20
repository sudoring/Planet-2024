library(sf) # read_sf
library(terra) # crs
library(dplyr) 
library(sp) # coordinates # proj4string
library(ggplot2)
library(rnaturalearth) # ne_states
getwd()

######## Input data 

#### 1) point data: weather station's location

## filter weather location
# ...

## convert data frame into SpatialPointsDataFrame 
inforefcsv <- read.csv("C:/Users/user/Desktop/2024_NIMS/FM/inforef.csv")
inforefcsv %>% dplyr::select(지점, type) %>% unique %>% 
  dplyr::group_by(type) %>% summarise(ntype=n()) 
inforef <- inforefcsv %>% dplyr::rename(latitude=위도, longitude=경도)
sp::coordinates(inforef) <- c("longitude", "latitude")
class(inforef)

## assign CRS WGS84 longitude/latitude
sp::proj4string(inforef) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
class(inforef)
# [1] "SpatialPointsDataFrame"
# attr(,"package")
# [1] "sp"
# crs(inforef) # error

## convert SpatialPointsDataFrame to sf
inforef <- st_as_sf(inforef)
class(inforef)
# [1] "sf"         "data.frame"


#### 2) polygon data: species distribution map

Robinia <- read_sf("C:/Users/user/Desktop/2024_NIMS/FM/Robinia_map_2023.shp")
Liriodendron <- read_sf("C:/Users/user/Desktop/2024_NIMS/FM/Liriodendron_map_2023.shp")
Castanea <- read_sf("C:/Users/user/Desktop/2024_NIMS/FM/Castanea_map_2023.shp")
Prunus <- read_sf("C:/Users/user/Desktop/2024_NIMS/FM/Prunus_map_2023.shp")
st_crs(Robinia) # unit: m
st_crs(Liriodendron) # unit: m
st_crs(Castanea) # unit: m
st_crs(Prunus) # unit: m


## re-project weather location point data from longitude/latitude 
inforefrp <- st_transform(inforef, st_crs(Robinia))
st_crs(inforefrp) # unit: m
ggplot(inforefrp) +
  geom_sf(aes(color = type)) +
  scale_color_manual(values = c("AWS" = "blue", "ASOS" = "red")) +
  labs(color = "Type") +
  theme_minimal()


# ## find polygons having an area greater than a specific threshold
# sp_polygons <- Prunus  # Robinia, Liriodendron, Castanea, Prunus
# 
# thr_area <- 9000
# all_areas <- as.numeric(st_area(sp_polygons)) # area unit: m^2
# # nrow(sp_polygons)
# # length(all_areas)
# summary(all_areas)
# hist(all_areas)
# 
# Large_polygons <- sp_polygons[all_areas > thr_area, ]
# Polygons_area <- as.numeric(st_area(Large_polygons)) # area unit: m^2
# summary(Polygons_area)
# hist(Polygons_area,labels=T)


## find polygons within the distance threshold & greater than a specific area
sp_polygons <- Robinia  # Robinia, Liriodendron, Castanea, Prunus

thr_dist <- 1000 # unit: m
thr_area <- 9000 # unit: m^2

distances <- st_distance(sp_polygons, inforefrp)
# str(distances) # Units: [m] num [1:31418, 1:657] 113214 113571 113144 113375 113046 ...
# nrow(sp_polygons) # 31418
# nrow(inforefrp_sf) # 657
min_dist <- apply(distances, 1, min) # for a matrix, 1 indicates rows
Filter_polygons <- sp_polygons[min_dist < thr_dist, ]
# Filter_polygons <- sp_polygons[min_dist < 3000, ]
# Filter_polygons <- sp_polygons[min_dist < 5000, ]

all_areas <- as.numeric(st_area(Filter_polygons)) # area unit: m^2
Filter2_polygons <- Filter_polygons[all_areas > thr_area, ]
Polygons_area <- as.numeric(st_area(Filter2_polygons)) # area unit: m^2
hist(Polygons_area,labels=T)
hist(as.numeric(Filter_polygons$갱신년),labels=T) # 2015-2023


## test small plot # Robinia

all_areas <- as.numeric(st_area(Filter_polygons)) # area unit: m^2
small_polygons <- Filter_polygons[all_areas > thr_area & all_areas < 10000, ]
smallpl_area <- as.numeric(st_area(small_polygons)) # area unit: m^2
# hist(smallpl_area,labels=T)
small3pl_area <- sort(smallpl_area)[1:3]
small3pl <- Filter_polygons[all_areas %in% small3pl_area, ]
# getwd()

# save the multipolygon to a shapefile
colnames(small3pl)
small3pl <- small3pl %>%
  rename(Update_Yr = 갱신년)
colnames(small3pl)
# getwd()
st_write(small3pl, "Robinia_small_3_plots.shp")


## species distribution

# Robinia map
sp_polygons <- Robinia  # Robinia, Liriodendron, Castanea, Prunus
thr_dist <- 1000 # unit: m
thr_area <- 9000 # unit: m^2
distances <- st_distance(sp_polygons, inforefrp_sf)
min_dist <- apply(distances, 1, min) # for a matrix, 1 indicates rows
Filter_polygons <- sp_polygons[min_dist < thr_dist, ]
all_areas <- as.numeric(st_area(Filter_polygons)) # area unit: m^2
Filter2_polygons <- Filter_polygons[all_areas > thr_area, ]
filtRobinia <- Filter2_polygons

# Liriodendron map
sp_polygons <- Liriodendron  # Robinia, Liriodendron, Castanea, Prunus
thr_dist <- 1000 # unit: m
thr_area <- 9000 # unit: m^2
distances <- st_distance(sp_polygons, inforefrp_sf)
min_dist <- apply(distances, 1, min) # for a matrix, 1 indicates rows
Filter_polygons <- sp_polygons[min_dist < thr_dist, ]
all_areas <- as.numeric(st_area(Filter_polygons)) # area unit: m^2
Filter2_polygons <- Filter_polygons[all_areas > thr_area, ]
filtLiriodendron <- Filter2_polygons

# Castanea map
sp_polygons <- Castanea  # Robinia, Liriodendron, Castanea, Prunus
thr_dist <- 1000 # unit: m
thr_area <- 9000 # unit: m^2
distances <- st_distance(sp_polygons, inforefrp_sf)
min_dist <- apply(distances, 1, min) # for a matrix, 1 indicates rows
Filter_polygons <- sp_polygons[min_dist < thr_dist, ]
all_areas <- as.numeric(st_area(Filter_polygons)) # area unit: m^2
Filter2_polygons <- Filter_polygons[all_areas > thr_area, ]
filtCastanea <- Filter2_polygons

# Prunus map
sp_polygons <- Prunus  # Robinia, Liriodendron, Castanea, Prunus
thr_dist <- 3000 # unit: m
thr_area <- 9000 # unit: m^2
distances <- st_distance(sp_polygons, inforefrp_sf)
min_dist <- apply(distances, 1, min) # for a matrix, 1 indicates rows
Filter_polygons <- sp_polygons[min_dist < thr_dist, ]
all_areas <- as.numeric(st_area(Filter_polygons)) # area unit: m^2
Filter2_polygons <- Filter_polygons[all_areas > thr_area, ]
filtPrunus <- Filter2_polygons


