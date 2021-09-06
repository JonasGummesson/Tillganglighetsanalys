# Skapar testdata för enbart Falun, enbart för att kunna felsöka problem i nätverken

# begränsa vägnät till falun
Y <- 6719371.765	# koordinater till lasarettet
X <- 535272.3
sfc = st_sfc(st_point(x = c(X,Y), "XY"))
st_crs(sfc) =  st_crs(3006)

# större buffer för lm data <- färre och större vägar
sf_buffer <- st_as_sf(st_buffer(sfc, 5000))
sf_vägnät_lm_falun <- sf_vägnät_lm %>% 
  st_zm(drop = TRUE, what = "ZM") %>%  # ta bort Z koordinat
  st_join(sf_buffer, left=FALSE) %>%
  mutate(sf_edge_id = row_number()) %>%
  select(sf_edge_id, xEdge, yEdge, geometry)

# mindre buffer för NVDB <- fler mindre vägar
sf_buffer <- st_as_sf(st_buffer(sfc, 500))
sf_vägnät_nvdb_falun <- sf_vägnät_nvdb %>% 
  st_join(sf_buffer, left=FALSE) %>%
  filter(HTHAST >= 30) %>%
  select(sf_edge_id, HTHAST, geometry)


p <- ggplot(sf_vägnät_lm_falun)+geom_sf(inherit.aes=FALSE)




p <- ggmap(map_Falun)+
  geom_sf(data= sf_vägnät_lm_falun, inherit.aes =FALSE, color = "red", size= 2)

p <- ggmap(map_FalunZoom)+
  geom_sf(data= sf_vägnät_nvdb_falun, inherit.aes =FALSE, color = "red", size= 2)

