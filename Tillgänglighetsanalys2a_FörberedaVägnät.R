################### Fixa till v�gn�t #####################


sf_v�gn�t_lm <- sf_v�gn�t_lm %>%
  as.data.table() %>%
  inner_join(st_centroid(sf_v�gn�t_lm) %>% 
               mutate(xEdge = st_coordinates(geometry)[,1],
                      yEdge = st_coordinates(geometry)[,2]), 
             suffix=c("", ".centroid"), by=("sf_edge_id")) %>%
  select(sf_edge_id, geometry, xEdge, yEdge) %>%
  st_as_sf()

sf_v�gn�t_nvdb <- sf_v�gn�t_nvdb %>%
  as.data.table() %>%
  inner_join(st_centroid(sf_v�gn�t_nvdb) %>% 
               mutate(xEdge = st_coordinates(geometry)[,1],
                      yEdge = st_coordinates(geometry)[,2]), 
             suffix=c("", ".centroid"), by=("sf_edge_id")) %>%
  select(sf_edge_id ,HTHAST, geometry, xEdge, yEdge) %>%
  st_as_sf()






