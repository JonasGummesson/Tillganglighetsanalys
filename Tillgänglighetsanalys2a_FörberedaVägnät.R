################### Fixa till vägnät #####################


sf_vägnät_lm <- sf_vägnät_lm %>%
  as.data.table() %>%
  inner_join(st_centroid(sf_vägnät_lm) %>% 
               mutate(xEdge = st_coordinates(geometry)[,1],
                      yEdge = st_coordinates(geometry)[,2]), 
             suffix=c("", ".centroid"), by=("sf_edge_id")) %>%
  select(sf_edge_id, geometry, xEdge, yEdge) %>%
  st_as_sf()

sf_vägnät_nvdb <- sf_vägnät_nvdb %>%
  as.data.table() %>%
  inner_join(st_centroid(sf_vägnät_nvdb) %>% 
               mutate(xEdge = st_coordinates(geometry)[,1],
                      yEdge = st_coordinates(geometry)[,2]), 
             suffix=c("", ".centroid"), by=("sf_edge_id")) %>%
  select(sf_edge_id ,HTHAST, geometry, xEdge, yEdge) %>%
  st_as_sf()






