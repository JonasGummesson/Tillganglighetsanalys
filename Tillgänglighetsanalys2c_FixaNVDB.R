# OBS! utgår, har fixat det med QGIS istället!

# lm enbart falun
net_vägnät_nvdb_test  <- as_sfnetwork(sf_vägnät_nvdb, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(edgeId = paste0(from, "-", to)) 


# NVDB <- funkar inte!!
# PROBLEM: en massa noder går inte att nå!!
#dist_nvdb = distances(graph = net_vägnät_nvdb_test,v=1, mode="out")
#dist_nvdb

# beräkna avstånd till Lasarettet i Falun
dist_nvdb <- net_vägnät_nvdb_test %>%
      activate("nodes") %>%
      left_join(
        t(distances(graph = net_vägnät_nvdb_test, v = 1, mode = "all")) %>%
          as_tibble() %>%
          rename("distance" = "V1") %>%
          mutate("distanceKm" = distance / 1000) %>%
          rownames_to_column("nodeId") %>%
          mutate(nodeId = as.integer(nodeId)),
        by=c("nodeId" = "nodeId")
      )

dist_nvdb <- dist_nvdb %>%
      activate("edges") %>%
      left_join(
        st_as_sf(dist_nvdb, "edges") %>% 
          as.data.table() %>%
          select(from, to, sf_edge_id)  %>%
          inner_join(st_as_sf(dist_nvdb, "nodes") %>% as.data.table() %>% select(nodeId, distance) %>% rename(distance.Node1 = distance), by=c("from"="nodeId")) %>%
          inner_join(st_as_sf(dist_nvdb, "nodes") %>% as.data.table() %>% select(nodeId, distance) %>% rename(distance.Node2 = distance), by=c("to"="nodeId")) %>%
          rowwise() %>%
          mutate(distance.NodeMax = max(distance.Node1, distance.Node2))
        ,by=c("sf_edge_id"="sf_edge_id"))


p_nvdb <- ggplot()+
  geom_sf(data = st_as_sf(dist_nvdb, "edges"), aes(color=distance.NodeMax))+
  #geom_sf(data = st_as_sf(dist_nvdb, "nodes") %>% filter(!is.infinite(distanceKm)), aes(color=distance), size=2)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (m)",
                        na.value = "grey100")   
#dist_falun_lm
p_lm <- ggplot()+
  geom_sf(data = st_as_sf(sf_vägnät_lm, "edges"), aes(color=distance.NodeMax))+
  #geom_sf(data = st_as_sf(dist_nvdb, "nodes") %>% filter(!is.infinite(distanceKm)), aes(color=distance), size=2)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (m)",
                        na.value = "grey100")   

grid.arrange(p_lm, p_nvdb)
