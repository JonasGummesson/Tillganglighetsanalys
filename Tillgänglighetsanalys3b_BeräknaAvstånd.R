#### Beräkna avstånd från utbudspunkter

# NVDB <- funkar inte!!
# PROBLEM: en massa noder går inte att nå!!
dist_nvdb = distances(graph = net_vägnät_nvdb_falun,v=1, mode="out")

############# Beräkna avstånd för lm ###################
nodes_falun <- unique(sf_utbud_falun_lm %>% 
                        filter(tolower(VårdtypGrupp) %like% "akut") %>%
                        pull(nodeId))

nodes_dalarna <- unique(sf_utbud_dalarna %>% 
  filter(tolower(VårdtypGrupp) %like% "akut") %>%
  pull(nodeId))


# beräkna avstånd till va
dist_falun_lm <- t(distances(graph = net_falun_lm, v = nodes_falun, mode = "all")) %>% 
  as.data.table() %>%
  rowwise()%>%
  mutate(distance = min(c_across(1:ncol(.))))%>%
  rownames_to_column("nodeId") %>%
  mutate(nodeId = as.integer(nodeId))





dist_dalarna_lm <- sf_utbud_dalarna_lm_närmaste_nod %>%
  filter(VårdtypGrupp == "Somatik akut") %>%
  rename("utbudNodeId" = "nodeId", "utbudX" = "x", utbudY = "y") %>%
  as.data.table() %>%
  select(utbudNodeId, utbudX, utbudY) %>%
  group_by(utbudNodeId, utbudX, utbudY) %>%
  summarise() %>%
 # head(1) %>%
  #select(utbudNamn, utbudNodeId, utbudX, utbudY, VårdtypGrupp) %>%
  mutate(net = map(.x = utbudNodeId, .f = function(utbudNodeId){
    net_dalarna_lm %>%
      activate("nodes") %>%
      left_join(
        t(distances(graph = net_dalarna_lm, v = utbudNodeId, mode = "all")) %>%
          as_tibble() %>%
          rename("distance" = "V1") %>%
          mutate("distanceKm" = distance / 1000) %>%
          rownames_to_column("nodeId") %>%
          mutate(nodeId = as.integer(nodeId)),
        by=c("nodeId" = "nodeId")
      )
    })) %>%
  left_join(sf_utbud_dalarna_lm_närmaste_nod %>% select(Populärnamn, VårdtypGrupp, utbudNamn, nodeId), by=c("utbudNodeId" = "nodeId"))
  
  #mutate(test = map(.x = distanceToNodeList, .f = function(x){ x %>% unnest(cols = c(distanceToNodeList)) %>% inner_join(net_dalarna_lm, by=c("nodeId"="nodeId")) } ))
  

#dist_dalarna_lm %>% head(1) %>% select(utbudNamn, distanceToNodeList) %>% unnest(cols = c(distanceToNodeList))

#dist_dalarna_lm <- t(distances(graph = net_dalarna_lm, v = nodes_dalarna, mode = "all")) %>% 
#  as.data.table() %>%
#  rowwise()%>%
#  mutate(distance = min(c_across(1:ncol(.))))%>%
#  rownames_to_column("nodeId") %>%
#  mutate(nodeId = as.integer(nodeId))




# plotta distans till enskild utbudspunkt
net <- (dist_dalarna_lm %>% head(1) %>% pull(net))[[1]]
p <- ggplot()+
  geom_sf(data = st_as_sf(net, "edges"), color = "blue")+
  geom_sf(data = st_as_sf(net, "nodes"), aes(color=distance), size=2)+
  # geom_text_repel(data = st_as_sf(net_falun_lm, "edges"), aes(label = paste0(to, "-", from), x = xEdge, y = yEdge), color = "blue", size=4)  +
 # geom_sf(data =   dist_dalarna_lm %>% head(1), aes(x=utbudX, y=utbudY), color="purple", size=5)+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                       name="Distans",
                       na.value = "grey100") 





#### Breath-first search Distansanalys  
f <- function(graph, data, extra) {
  print(data)
  x <- data.frame(nodeId = data["vid"]+1, dist = data["dist"])
  result <<- rbind(result,x)
  data['rank'] == 10 # begränsa sökning i djup
}
# lm 
result <- NULL
tmp <- bfs(net_falun_lm, root=1, "all", callback=f, unreachable=FALSE)
bfs_falun_lm <- result %>% arrange(dist)



# nvdb
result <- NULL
tmp <- bfs(net_falun_parts, root=1, "all", callback=f, unreachable=FALSE)
bfs_falun_nvdb <- result %>% arrange(dist)



