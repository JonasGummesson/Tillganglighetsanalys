library(microbenchmark)
nodes_utbud <- unique(sf_utbud_nvdb %>% 
  filter(tolower(VårdtypGrupp) %like% "Somatik akut") %>%
  pull(nodeId))




############################### Beräkna för hela Dalarna ###################################################
mbm = microbenchmark(
  dist_dalarna_nvdb <- sf_utbud_nvdb %>%
    filter(VårdtypGrupp == "Somatik akut" | VårdtypGrupp == "Primärvård")  %>%
    rename("utbudNodeId" = "nodeId", "utbudX" = "x", utbudY = "y") %>%
    as.data.table() %>%
    select(utbudNodeId, utbudX, utbudY) %>%
    group_by(utbudNodeId, utbudX, utbudY) %>%
    summarise() %>%
    mutate(net = map(.x = utbudNodeId, .f = function(utbudNodeId){
      distanceNodes <- net_vägnät_nvdb %>%
        activate("nodes") %>%
        left_join(
          t(distances(graph = net_vägnät_nvdb, v = utbudNodeId, mode = "all")) %>%
            as_tibble() %>%
            rename("distance" = "V1") %>%
            mutate("distanceKm" = distance / 1000) %>%
            rownames_to_column("nodeId") %>%
            mutate(nodeId = as.integer(nodeId)) %>%
            select(nodeId, distance, distanceKm),
          by=c("nodeId" = "nodeId")
        )
      distanceNodes %>%
        activate("edges") %>%
        left_join(
          st_as_sf(distanceNodes, "edges") %>% 
            as.data.table() %>%
            select(from, to, sf_edge_id)  %>%
            inner_join(st_as_sf(distanceNodes, "nodes") %>% as.data.table() %>% select(nodeId, distance) %>% rename(distance.Node1 = distance), by=c("from"="nodeId")) %>%
            inner_join(st_as_sf(distanceNodes, "nodes") %>% as.data.table() %>% select(nodeId, distance) %>% rename(distance.Node2 = distance), by=c("to"="nodeId")) %>%
            rowwise() %>%
            mutate(distance.NodeMax = max(distance.Node1, distance.Node2)) %>%
            mutate(distance.NodeMaxKm = distance.NodeMax / 1000) %>%
            select(sf_edge_id, distance.NodeMax, distance.NodeMaxKm)
          ,by=c("sf_edge_id"="sf_edge_id"))
      })) %>%
    left_join(sf_utbud_nvdb %>% select(Populärnamn, VårdtypGrupp, utbudNamn, nodeId) %>% filter(VårdtypGrupp == "Somatik akut" | VårdtypGrupp == "Primärvård")
                , by=c("utbudNodeId" = "nodeId")),
  times = 1)
#mbm
  
  



# plotta utbudspunkt i Dalarna

net <- (dist_dalarna_nvdb %>% pull(net))[[1]]

p <- ggplot()+
  geom_sf(data = sf_kommuner_dalarna, color = "grey", linetype="dashed")+
  geom_sf(data = st_as_sf(net, "edges"), aes(color=distance.NodeMaxKm))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),name="Distans (km)",na.value = "green") 



############################################################################################################
############################################################################################################


sf_vägnät_nvdb_points <- rbind(
  st_cast(st_line_sample(sf_vägnät_nvdb, sample = 1), "POINT") %>% as.data.frame() ,
  st_cast(st_line_sample(sf_vägnät_nvdb, sample = 0), "POINT") %>% as.data.frame()
) %>% st_as_sf()

#?st_join
#sf_falun_points = st_cast(sf_falun, "POINT")

#hitta ensamma ändpunkter
sf_vägnät_nvdb_points_single <- sf_vägnät_nvdb_points %>% 
  group_by(st_coordinates(geometry)) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>%
  filter(n == 1) %>%
  select(geometry)


# klipp av bågar i närheten ensamma ändpunkter (TEST!!)
buffer_points <- st_combine(st_buffer(sf_vägnät_nvdb_points_single, 5))
sf_vägnät_nvdb_parts = st_collection_extract(lwgeom::st_split(sf_vägnät_nvdb, buffer_points),"LINESTRING")




############################### Beräkna för hela Dalarna ###################################################
net_vägnät_nvdb_parts = as_sf(sf_vägnät_nvdb_parts)

mbm_dist_dalarna_nvdb_parts = microbenchmark(
  dist_dalarna_nvdb_parts <- sf_utbud_nvdb %>%
    filter(VårdtypGrupp == "Somatik akut" | VårdtypGrupp == "Primärvård")  %>%
    rename("utbudNodeId" = "nodeId", "utbudX" = "x", utbudY = "y") %>%
    as.data.table() %>%
    select(utbudNodeId, utbudX, utbudY) %>%
    group_by(utbudNodeId, utbudX, utbudY) %>%
    summarise() %>%
    mutate(net = map(.x = utbudNodeId, .f = function(utbudNodeId){
      distanceNodes <- net_vägnät_nvdb %>%
        activate("nodes") %>%
        left_join(
          t(distances(graph = sf_vägnät_nvdb_parts, v = utbudNodeId, mode = "all")) %>%
            as_tibble() %>%
            rename("distance" = "V1") %>%
            mutate("distanceKm" = distance / 1000) %>%
            rownames_to_column("nodeId") %>%
            mutate(nodeId = as.integer(nodeId)) %>%
            select(nodeId, distance, distanceKm),
          by=c("nodeId" = "nodeId")
        )
      distanceNodes %>%
        activate("edges") %>%
        left_join(
          st_as_sf(distanceNodes, "edges") %>% 
            as.data.table() %>%
            select(from, to, sf_edge_id)  %>%
            inner_join(st_as_sf(distanceNodes, "nodes") %>% as.data.table() %>% select(nodeId, distance) %>% rename(distance.Node1 = distance), by=c("from"="nodeId")) %>%
            inner_join(st_as_sf(distanceNodes, "nodes") %>% as.data.table() %>% select(nodeId, distance) %>% rename(distance.Node2 = distance), by=c("to"="nodeId")) %>%
            rowwise() %>%
            mutate(distance.NodeMax = max(distance.Node1, distance.Node2)) %>%
            mutate(distance.NodeMaxKm = distance.NodeMax / 1000) %>%
            select(sf_edge_id, distance.NodeMax, distance.NodeMaxKm)
          ,by=c("sf_edge_id"="sf_edge_id"))
    })) %>%
    left_join(sf_utbud_nvdb %>% select(Populärnamn, VårdtypGrupp, utbudNamn, nodeId) %>% filter(VårdtypGrupp == "Somatik akut" | VårdtypGrupp == "Primärvård")
              , by=c("utbudNodeId" = "nodeId")),
  times = 1)
#mbm_dist_dalarna_nvdb_parts

#dist_dalarna_nvdb_parts

# plotta utbudspunkt i Dalarna

net_parts <- (dist_dalarna_nvdb_parts %>% pull(net))[[1]]

p <- ggplot()+
  geom_sf(data = sf_kommuner_dalarna, color = "grey", linetype="dashed")+
  geom_sf(data = st_as_sf(net_parts, "edges"), aes(color=distance.NodeMaxKm))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),name="Distans (km)",na.value = "green") 

net %>% 
  activate("nodes") %>%
  mutate(nåbar=ifelse(is.infinite(distance), 0, 1)) %>%
  summarise(nåbar = sum(nåbar), n=n())


net_parts %>% 
  activate("nodes") %>%
  mutate(nåbar=ifelse(is.infinite(distance), 0, 1)) %>%
  summarise(nåbar = sum(nåbar), n=n())
