#### Ber�kna avst�nd fr�n utbudspunkter

# NVDB <- funkar inte!!
# PROBLEM: en massa noder g�r inte att n�!!
dist_nvdb = distances(graph = net_v�gn�t_nvdb_falun,v=1, mode="out")

############# Ber�kna avst�nd f�r lm ###################
noder_utbud_falun <- unique(sf_utbud_lm_falun %>% 
                        filter(tolower(V�rdtypGrupp) %like% "akut") %>%
                        pull(nodeId))

nodes_utbud <- unique(sf_utbud_lm %>% 
  filter(tolower(V�rdtypGrupp) %like% "akut") %>%
  pull(nodeId))



net_v�gn�t_lm_falun %>%
  activate("nodes") %>%
  left_join(
    t(distances(graph = net_v�gn�t_lm_falun, v = 1, mode = "all")) %>%
      as_tibble() %>%
      rename("distance" = "V1") %>%
      mutate("distanceKm" = distance / 1000) %>%
      rownames_to_column("nodeId") %>%
      mutate(nodeId = as.integer(nodeId)),
    by=c("nodeId" = "nodeId")
  )


# ber�kna avst�nd till Lasarettet i Falun
dist_falun_lm <- sf_utbud_lm_falun %>%
  filter(V�rdtypGrupp == "Somatik akut" | V�rdtypGrupp == "Prim�rv�rd") %>%
  rename("utbudNodeId" = "nodeId", "utbudX" = "x", utbudY = "y") %>%
  as.data.table() %>%
  select(utbudNodeId, utbudX, utbudY) %>%
  group_by(utbudNodeId, utbudX, utbudY) %>%
  summarise() %>%
  mutate(net = map(.x = utbudNodeId, .f = function(utbudNodeId){
    temp <- net_v�gn�t_lm_falun %>%
      activate("nodes") %>%
      left_join(
        t(distances(graph = net_v�gn�t_lm_falun, v = utbudNodeId, mode = "all")) %>%
          as_tibble() %>%
          rename("distance" = "V1") %>%
          mutate("distanceKm" = distance / 1000) %>%
          rownames_to_column("nodeId") %>%
          mutate(nodeId = as.integer(nodeId)),
        by=c("nodeId" = "nodeId")
      )
      temp %>%
        activate("edges") %>%
      left_join(
        st_as_sf(temp, "edges") %>% 
          as.data.table() %>%
          select(from, to, sf_edge_id)  %>%
          inner_join(st_as_sf(temp, "nodes") %>% as.data.table() %>% select(nodeId, distance) %>% rename(distance.Node1 = distance), by=c("from"="nodeId")) %>%
          inner_join(st_as_sf(temp, "nodes") %>% as.data.table() %>% select(nodeId, distance) %>% rename(distance.Node2 = distance), by=c("to"="nodeId")) %>%
          rowwise() %>%
          mutate(distance.NodeMax = max(distance.Node1, distance.Node2))
        ,by=c("sf_edge_id"="sf_edge_id"))
  })) %>%
  left_join(sf_utbud_lm %>% select(Popul�rnamn, V�rdtypGrupp, utbudNamn, nodeId), by=c("utbudNodeId" = "nodeId"))


net <- (dist_falun_lm %>% pull(net))[[1]]

p <- ggplot()+
  geom_sf(data = st_as_sf(net, "edges"), aes(color=distance.NodeMax))+
  geom_sf(data = st_as_sf(net, "nodes"), aes(color=distance), size=2)+
  #geom_text_repel(data = st_as_sf(net, "edges"), aes(label = sprintf("%.0f m", distance.NodeMax), x = xEdge, y = yEdge), color = "blue", size=4)  +
  # geom_sf(data =   dist_dalarna_lm %>% head(1), aes(x=utbudX, y=utbudY), color="purple", size=5)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (m)",
                        na.value = "grey100") 

p <- ggmap(map_Falun)+
  geom_sf(data = st_as_sf(net, "edges"), aes(color=distance.NodeMax), inherit.aes = FALSE, size=1, alpha=0.5)+
  geom_sf(data = st_as_sf(net, "nodes"), aes(color=distance), size=2,  inherit.aes = FALSE)+
  #geom_text_repel(data = st_as_sf(net, "edges"), aes(label = sprintf("%.0f m", distance.NodeMax), x = xEdge, y = yEdge), color = "blue", size=4)  +
  # geom_sf(data =   dist_dalarna_lm %>% head(1), aes(x=utbudX, y=utbudY), color="purple", size=5)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (m)",
                        na.value = "grey100") 




############################### Ber�kna f�r hela Dalarna ###################################################
dist_dalarna_lm <- sf_utbud_lm %>%
  filter(V�rdtypGrupp == "Somatik akut" | V�rdtypGrupp == "Prim�rv�rd")  %>%
  rename("utbudNodeId" = "nodeId", "utbudX" = "x", utbudY = "y") %>%
  as.data.table() %>%
  select(utbudNodeId, utbudX, utbudY) %>%
  group_by(utbudNodeId, utbudX, utbudY) %>%
  summarise() %>%
 # head(1) %>%
  #select(utbudNamn, utbudNodeId, utbudX, utbudY, V�rdtypGrupp) %>%
  mutate(net = map(.x = utbudNodeId, .f = function(utbudNodeId){
    distanceNodes <- net_v�gn�t_lm %>%
      activate("nodes") %>%
      left_join(
        t(distances(graph = net_v�gn�t_lm, v = utbudNodeId, mode = "all")) %>%
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
  left_join(sf_utbud_lm %>% select(Popul�rnamn, V�rdtypGrupp, utbudNamn, nodeId) %>% filter(V�rdtypGrupp == "Somatik akut" | V�rdtypGrupp == "Prim�rv�rd")
              , by=c("utbudNodeId" = "nodeId"))
  
  



# plotta utbudspunkt i Dalarna

net <- (dist_dalarna_lm %>% pull(net))[[1]]

p <- ggplot()+
  geom_sf(data = st_as_sf(net, "edges"), aes(color=distance.NodeMaxKm))+
  #geom_sf(data = st_as_sf(net, "nodes"), aes(color=distance), size=2)+
  # geom_text_repel(data = st_as_sf(net_falun_lm, "edges"), aes(label = paste0(to, "-", from), x = xEdge, y = yEdge), color = "blue", size=4)  +
  # geom_sf(data =   dist_dalarna_lm %>% head(1), aes(x=utbudX, y=utbudY), color="purple", size=5)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (km)",
                        na.value = "grey100") 


### diskretisera avst�nd

p <- ggplot()+
  geom_sf(data = st_as_sf(net %>% mutate(), "edges") %>% mutate(distance.discrete= cut(distance.NodeMaxKm, breaks = c(0,20,40,60,100,200))), aes(color=distance.discrete))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_viridis_d(option = "magma", direction=-1)





#### Breath-first search Distansanalys  
f <- function(graph, data, extra) {
  print(data)
  x <- data.frame(nodeId = data["vid"]+1, dist = data["dist"])
  result <<- rbind(result,x)
  data['rank'] == 10 # begr�nsa s�kning i djup
}
# lm 
result <- NULL
tmp <- bfs(net_falun_lm, root=1, "all", callback=f, unreachable=FALSE)
bfs_falun_lm <- result %>% arrange(dist)



# nvdb
result <- NULL
tmp <- bfs(net_falun_parts, root=1, "all", callback=f, unreachable=FALSE)
bfs_falun_nvdb <- result %>% arrange(dist)



