
#### Distansanalys med Dijkstra

# NVDB <- funkar inte!!
# PROBLEM: en massa noder går inte att nå!!
dist_nvdb = distances(graph = net_falun_parts,v=1, mode="out")

# LM <- funkar
dist_lm <- t(distances(graph = net_falun_lm, v = 1, mode = "all")) %>% 
  as.data.frame() %>%
  rename("distance" = "V1") %>%
  rownames_to_column("nodeId") %>%
  mutate(nodeId = as.integer(nodeId))

# plotta distanser
ggplot()+
  geom_sf(data = st_as_sf(net_falun_lm, "nodes") %>% left_join(dist_lm, by = c("nodeId" = "nodeId")), color="red", size=2)+
  geom_sf(data = st_as_sf(net_falun_lm, "edges"), color = "blue")+
  # geom_text_repel(data = st_as_sf(net_falun_lm, "edges"), aes(label = paste0(to, "-", from), x = xEdge, y = yEdge), color = "blue", size=4)  +
  geom_text_repel(data = st_as_sf(net_falun_lm, "nodes")%>% left_join(dist_lm, by = c("nodeId" = "nodeId")), aes(label = sprintf("%.0f (%d)", distance, nodeId), x = xNode, y = yNode), color = "red", size=4)  



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



