
# begränsa vägnät till falun
Y <- 6719371.765	# koordinater till lasarettet
X <- 535272.3
sfc = st_sfc(st_point(x = c(X,Y), "XY"))
st_crs(sfc) =  st_crs(3006)
sf_buffer <- st_as_sf(st_buffer(sfc, 200))


sf_falun_lm <- sf_vägnät_dalarna_lm %>% 
  st_zm(drop = TRUE, what = "ZM") %>%  # ta bort Z koordinat
  st_join(sf_buffer, left=FALSE) %>%
  mutate(sf_edge_id = row_number()) %>%
  select(sf_edge_id, geometry)

sf_falun_nvdb <- sf_vägnät_dalarna_nvdb %>% 
  st_zm(drop = TRUE, what = "ZM") %>%  # ta bort Z koordinat
  st_join(sf_buffer, left=FALSE) %>%
  mutate(sf_edge_id = row_number()) %>%
  filter(HTHAST >= 30) %>%
  select(sf_edge_id, HTHAST, geometry)


ggplot(sf_falun_lm)+geom_sf(inherit.aes=FALSE)


# lägg till centroid-koordinater för bågar   
sf_falun_lm <- sf_falun_lm %>% 
  as.data.table() %>% 
  inner_join(st_centroid(sf_falun_lm) %>% 
               mutate(XcoordEdge = st_coordinates(geometry)[,1],
                      YcoordEdge = st_coordinates(geometry)[,2]), 
             suffix=c("", ".centroid"), by=("sf_edge_id")) %>%
  select(XcoordEdge,YcoordEdge, geometry) %>% st_as_sf() 

sf_falun_nvdb <- sf_falun_nvdb %>% 
  as.data.table() %>% 
  inner_join(st_centroid(sf_falun_nvdb) %>% 
               mutate(XcoordEdge = st_coordinates(geometry)[,1],
                      YcoordEdge = st_coordinates(geometry)[,2]), 
             suffix=c("", ".centroid"), by=("sf_edge_id")) %>%
  select(HTHAST, XcoordEdge,YcoordEdge, geometry) %>% st_as_sf() 



# avrunda koordinater till närmaste meter (för att förenkla koppling mellan bågar)
st_geometry(sf_falun_nvdb) <- st_geometry(sf_falun_nvdb) %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = st_crs(sf_falun_nvdb))


# segmentera


sf_falun_nvdb_points <- rbind(
  st_cast(st_line_sample(sf_falun_nvdb, sample = 1), "POINT") %>% as.data.frame() ,
  st_cast(st_line_sample(sf_falun_nvdb, sample = 0), "POINT") %>% as.data.frame()
) %>% st_as_sf()

#?st_join
#sf_falun_points = st_cast(sf_falun, "POINT")

#hitta ensamma ändpunkter
sf_falun_nvdb_point_single <- sf_falun_nvdb_points %>% 
  group_by(st_coordinates(geometry)) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>%
  filter(n == 1) %>%
  select(geometry)


# klipp av bågar i närheten ensamma ändpunkter (TEST!!)
buffer_points <- st_combine(st_buffer(sf_falun_nvdb_point_single, 5))
sf_falun_nvdb_parts = st_collection_extract(lwgeom::st_split(sf_falun_nvdb, buffer_points),"LINESTRING")


