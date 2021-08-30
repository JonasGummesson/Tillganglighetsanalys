# Analys av närhet till utbudspunkter
# OBS! Koder är under framtagande och är inte klar ännu

# ladda bibliotek

library(sf)
library(tidyverse)
library(tidygraph)
library(igraph)
library(sfnetworks)
library(data.table)
library(httr)
library(ggmap)
library(remotes)
library(nngeo) # st_segment
library(gridExtra)
library(cleangeo)
library(ggrepel)
library(viridis)

# konfigurera proxy
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

keys <- read.delim("~/GitHub/Tillganglighetsanalys/keys.txt", header = TRUE, sep = ";")

googleApiKey <- as.data.table(keys) %>% filter(Name == "GoogleApiKey") %>% pull(Value)
############### ladda google maps ################################
ggmap::register_google(key = googleApiKey) # Jonas nyckel -> kostnader går till Jonas så används med måtta!!

map_Dalarna <- get_map(location = c(lon = 15.55, lat = 61.55), zoom = 7, maptype = "hybrid")
map_Falun <- get_map(location = c(lon = 15.65, lat = 60.69), zoom = 10, maptype = "hybrid")
map_FalunZoom <- get_map(location = c(lon = 15.64, lat = 60.61), zoom = 15, maptype = "hybrid")

# konvertera ggmap kartor till Sweref99
ggmap_4326_to_3006_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), c("ymin", "xmin", "ymax", "xmax"))
  
  # Convert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3006 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3006))    
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3006["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3006["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3006["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3006["xmax"]
  map
}

map_FalunZoom_3006 <- ggmap_4326_to_3006_bbox(map_FalunZoom)


# ladda kommunkartor
sf_kommuner_dalarna <- st_read(dsn = "E:\\Filer\\admgumjon\\Kommungränser_Dalarna")
st_crs(sf_kommuner_dalarna) <- st_crs(3006)
sf_kommuner_dalarna$KOMMUNNAMN <- iconv(sf_kommuner_dalarna$KOMMUNNAMN, "1252", "UTF-8")

# ladda vägnät, lantmäteriet (saknar vägar så denna bör inte användas)
sf_vägnät_dalarna_lm <- st_read(dsn = "E:/Filer/admgumjon/Vägkartor")
st_crs(sf_vägnät_dalarna_lm) <- st_crs(3006)
sf_vägnät_dalarna_lm$KATEGORI <- iconv(sf_vägnät_dalarna_lm$KATEGORI, "1252", "UTF-8")
str(sf_vägnät_dalarna_lm)

################## ladda vägnät NVDB #################
sf_vägnät_dalarna_nvdb <- st_read(dsn = "E:/Filer/admgumjon/Vägkartor/NVDB_Hastighet")
st_crs(sf_vägnät_dalarna_nvdb) <- st_crs(3006)
str(sf_vägnät_dalarna_nvdb)


