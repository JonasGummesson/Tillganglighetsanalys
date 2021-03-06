# Skapar testdata f�r enbart Falun, enbart f�r att kunna fels�ka problem i n�tverken

# begr�nsa v�gn�t till falun
Y <- 6719371.765	# koordinater till lasarettet
X <- 535272.3
sfc = st_sfc(st_point(x = c(X,Y), "XY"))
st_crs(sfc) =  st_crs(3006)

# st�rre buffer f�r lm data <- f�rre och st�rre v�gar
sf_buffer <- st_as_sf(st_buffer(sfc, 5000))
sf_v�gn�t_lm_falun <- sf_v�gn�t_lm %>% 
  st_zm(drop = TRUE, what = "ZM") %>%  # ta bort Z koordinat
  st_join(sf_buffer, left=FALSE) %>%
  mutate(sf_edge_id = row_number()) %>%
  select(sf_edge_id, xEdge, yEdge, geometry)

# mindre buffer f�r NVDB <- fler mindre v�gar
sf_buffer <- st_as_sf(st_buffer(sfc, 1000))
sf_v�gn�t_nvdb_falun <- sf_v�gn�t_nvdb %>% 
  st_join(sf_buffer, left=FALSE) %>%
  filter(HTHAST >= 30) %>%
  select(sf_edge_id, HTHAST, geometry)


p <- ggplot(sf_v�gn�t_lm_falun)+geom_sf(inherit.aes=FALSE)




p <- ggmap(map_Falun)+
  geom_sf(data= sf_v�gn�t_lm_falun, inherit.aes =FALSE, color = "red", size= 2)

p <- ggmap(map_FalunZoom)+
  geom_sf(data= sf_v�gn�t_nvdb_falun, inherit.aes =FALSE, color = "red", size= 2)

