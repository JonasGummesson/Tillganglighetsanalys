# OBS! Under utveckling!!!!
############################ Nätverksanalys ##############################

# nvdb utan att splitta
net_falun  <- as_sfnetwork(sf_falun_nvdb, directed = FALSE)
nodes = as_sfnetwork(sf_falun_nvdb) %>%
  activate("nodes") %>%
  st_as_sf()

edges = as_sfnetwork(sf_falun_nvdb) %>%
  activate("edges") %>%
  st_as_sf()
st_geometry(edges) = NULL

net_falun_lines  <- sfnetwork(nodes = nodes, edges= edges, directed = FALSE, edges_as_lines = TRUE)

plot(net_falun)
plot(net_falun_lines, add=TRUE, col="red")

ggplot()+
  geom_sf(data = st_as_sf(net_falun, "edges") %>% mutate(name = paste0(from, "-", to)), aes(color=name), linetype="dashed")+
  geom_sf(data = st_as_sf(net_falun, "nodes"))+
  geom_sf(data = st_as_sf(net_falun_lines, "edges"), color = "red")

# nvdb med split

nodes_parts = as_sfnetwork(sf_falun_nvdb_parts) %>%
  activate("nodes") %>%
  st_as_sf()

edges_parts = as_sfnetwork(sf_falun_nvdb_parts) %>%
  activate("edges") %>%
  st_as_sf()
st_geometry(edges_parts) = NULL


net_falun_parts  <- as_sfnetwork(sf_falun_nvdb_parts, directed = FALSE)
net_falun_parts_lines  <- sfnetwork(nodes = nodes_parts, edges= edges_parts, directed = FALSE, edges_as_lines = TRUE)

plot(net_falun_parts)
plot(net_falun_parts_lines, add=TRUE, col="red")


### visualisera traversering av graf
net <- as_sfnetwork(sf_falun_lm, directed=FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = as.integer(V(net))) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(edgeId = paste0(from, "-", to))%>%
  mutate(xEdge = st_coordinates(st_centroid(geometry))[,1], yEdge = st_coordinates(st_centroid(geometry))[,2])


f <- function(graph, data, extra) {
  #result <- rbind(result, data.frame(nodeId = data["vid"], dist = data["dist"]) )
  #  print(paste0("node = ", data["vid"], ", distance = ", data["dist"]))
  print(data)
  x <- data.frame(nodeId = data["vid"], dist = data["dist"])
  result <<- rbind(result,x)
  #print(paste0("node = ", x$nodeId, ", distance = ", x$dist))
  FALSE
}

bfs(net, root=20, "all")

result <- data.frame(nodeId = integer, dist = integer)
tmp <- bfs(net, root=20, "all", callback=f, unreachable=FALSE)
result %>% arrange(dist)

p1<-st_as_sf(net, "nodes")%>%
  left_join(result, by=c("nodeId" = "nodeId")) %>%
  ggplot()+
  geom_sf(color = "red")+
  geom_sf(data = st_as_sf(net, "edges"), color = "blue")+
  geom_text_repel(aes(label = nodeId, x = xNode, y = yNode), color = "red", size=4)+
  geom_text_repel(data = st_as_sf(net, "edges"), aes(label = edgeId, x = xEdge, y = yEdge), color = "blue", size=4)
#geom_text(aes(label = sprintf("NodeId %d -> %d m", nodeId, dist), x = XcoordNode, y = YcoordNode), color = "red")

p2<-st_as_sf(net, "nodes")%>%
  left_join(result, by=c("nodeId" = "nodeId")) %>%
  ggplot()+
  geom_sf(color = "red")+
  geom_sf(data = st_as_sf(net, "edges"), color = "blue")+
  geom_text(aes(label = sprintf("%d m", dist), x = xNode, y = yNode), color = "red")

grid.arrange(p1,p2)


# beräkna vikt på båge, koordinater för noder och bågar, samt identifiera nod närmast sjukhus
net_falun1 <- net_falun  %>%
  activate("edges") %>%
  #mutate(weight = edge_length()/HTHAST*(60/1000)) %>%
  mutate(weight = edge_length()) %>%
  mutate(name = paste0(from, "-", to)) %>%
  mutate(XcoordEdge = st_coordinates(st_centroid(geometry))[,1], YcoordEdge = st_coordinates(st_centroid(geometry))[,2])%>%
  activate("nodes") %>%
  mutate(XcoordNode = st_coordinates(geometry)[,1], YcoordNode = st_coordinates(geometry)[,2])%>%
  mutate(dist = sqrt((XcoordNode -535272.3)^2+(YcoordNode-6719371.765)^2)) %>%
  mutate(dist_rank = rank(dist)) %>%
  mutate(name = 1:length(V(net_falun))) %>%
  mutate(name = ifelse(dist_rank == 1, "sjukhus", name)) 

plot(net_falun)

# beräkna distanser
net_falun2 <- net_falun1 %>%
  # activate("nodes") %>% 
  left_join(as.data.table(t(distances(graph = net_falun1,v="sjukhus", mode="out"))) %>% 
              mutate(id = rownames(.)) %>%
              rename("distans sjukhus" = "sjukhus"),
            by = c("name" = "id")) %>%
  mutate(`distans sjukhus` = replace_na(`distans sjukhus`,0)) 

# lägg till namn på bågar
#net_falun3 <- net_falun2 %>%
#activate("edges") %>%
#inner_join(st_as_sf(net_falun2, "nodes") %>%  rownames_to_column("rowid") %>% as.data.table() %>% mutate(rowid = as.integer(rowid)) %>% select(rowid, `distans sjukhus`), by=c("from" = "rowid")) %>%
#inner_join(st_as_sf(net_falun2, "nodes") %>%  rownames_to_column("rowid") %>% as.data.table() %>% mutate(rowid = as.integer(rowid)) %>% select(rowid,  `distans sjukhus`), by=c("to" = "rowid")) %>%
#mutate(`distans sjukhus` = (`distans sjukhus.x` + `distans sjukhus.y`)/2)


net_result <- net_falun2

# remove distant nodes
#n_result <- n %>% activate("nodes") %>% filter(`distans sjukhus` < 500 | is.na(`distans sjukhus`))











####################### plotta sf ############################


# plotta vägnät mot google maps
ggmap(map_FalunZoom_3006) +
  geom_sf(data = sf_falun, size = 1, aes(color = as.factor(HTHAST)), fill =NA, inherit.aes = FALSE) +
  geom_sf(data = sf_buffer, linetype="dashed", fill = NA, color = "yellow", size = 2, inherit.aes = FALSE)


####################### plotta nätverk ############################

st_as_sf(net_result, "nodes") %>% pull(name)
st_as_sf(net_result, "edges") %>% filter(name %like% "40" | name %like% "sjukhus") %>% pull(geometry)
### plotta med namn på noder och bågar
ggplot(data = st_as_sf(net_result, "edges")) +
  geom_sf(color="blue") +
  geom_sf(data = st_as_sf(net_result, "nodes"), color = "red")+
  geom_text_repel(data = st_as_sf(net_result, "nodes") , aes(label = name, x = XcoordNode, y=YcoordNode), color = "red", max.overlaps = 1000)+
  geom_text_repel(data = st_as_sf(net_result, "edges"), aes(label = name, x = XcoordEdge, y=YcoordEdge), color = "blue", max.overlaps = 1000)+
  geom_sf(data = st_as_sf(net_result, "nodes") %>% filter(name %in% c(40,39,24)), color = "purple", size=3)


## plotta med avstånd till sjukhus
ggplot(data = st_as_sf(net_result, "edges")) +
  geom_sf(color="blue") +
  geom_sf(data = st_as_sf(net_result, "nodes"), color = "red")+
  geom_text_repel(data = st_as_sf(net_result, "nodes") , aes(label = sprintf("%.0f m", `distans sjukhus`), x = XcoordNode, y=YcoordNode), color = "red")




ggmap(map_FalunZoom_3006)+
  geom_sf(data = st_as_sf(n, "edges") , aes(color = as.factor(ifelse(`distans sjukhus` < 500, 1, 0))),  inherit.aes = FALSE, size=2) +
  geom_sf(data = st_as_sf(n, "nodes") ,  aes(color=as.factor(ifelse(`distans sjukhus` < 500, 1, 0))), inherit.aes = FALSE, size=2, color = "yellow")+
  geom_text(data = st_as_sf(n, "nodes"), color = "white", size=5, aes(label = sprintf("%.0f m", `distans sjukhus`), x = XcoordNode, y=YcoordNode),  inherit.aes = FALSE)


sf_result <- st_as_sf(n_result, "edges") %>%
  inner_join(st_as_sf(n_result, "nodes") %>%  rownames_to_column("rowid") %>% as.data.table() %>% mutate(rowid = as.integer(rowid)), by=c("from" = "rowid")) %>%
  inner_join(st_as_sf(n_result, "nodes") %>%  rownames_to_column("rowid") %>% as.data.table() %>% mutate(rowid = as.integer(rowid)), by=c("to" = "rowid")) %>%
  select(from, to, KATEGORI, Xcoord, Ycoord, distans)

ggmap(map_FalunZoom_3006) + geom_sf(data = sf_result, size = 1, color = "red", fill =NA, inherit.aes = FALSE) +
  geom_sf(data = st_buffer(sf_result, 100), alpha = 0.2, color = "red", inherit.aes = FALSE)

##################### Skapa alpha shape #########################

######################### undersöka problem ############################

n2 <- as_sfnetwork( n %>% activate("edges") %>%filter(name == "3-19" | name == "40-44") %>%  st_segments() )
#n3 <- as_sfnetwork( n %>% activate("edges") %>%filter(name == "3-19") %>%  st_segments() )

p1<-ggplot() +
  geom_sf(data =  n2 %>% activate("nodes")%>%st_as_sf() , inherit.aes =FALSE, color="grey") +
  geom_sf(data = n %>% activate("nodes")%>%filter(name %in% c("3", "44", "19","40")) %>% st_as_sf(), color="red", size = 4, inherit.aes =FALSE, alpha=0.5)+
  geom_sf(data = n2 %>% activate("edges") %>% st_as_sf(), color = "blue", inherit.aes =FALSE)
#geom_sf(data = n %>% activate("edges")%>%filter(name == "3-19" | name == "40-44") %>% st_as_sf(), aes(color = name), inherit.aes =FALSE, size=1) 

p2<-ggplot() +
  geom_sf(data =  n3 %>% activate("nodes")%>%st_as_sf() , inherit.aes =FALSE, color="grey") +
  geom_sf(data = n %>% activate("nodes")%>%filter(name %in% c("3", "19")) %>% st_as_sf(), color="red", size = 4, inherit.aes =FALSE, alpha=0.5)+
  geom_sf(data = n3 %>% activate("edges") %>% st_as_sf(), color = "blue", inherit.aes =FALSE)
#geom_sf(data = n %>% activate("edges")%>%filter(name == "3-19") %>% st_as_sf(), aes(color = name), inherit.aes =FALSE, size=1) 

grid.arrange(p1,p2)

n3_unsegmented <- n %>% filter(name == "3-19" | name == "40-44")

distances(graph = n2,v=1, mode="out")
V(n3_unsegmented)
distances(graph = n3_unsegmented,v=3, mode="out")



n2_dist <- n2  %>%
  activate("edges") %>%
  mutate(name = paste0(from, "-", to)) 

n <- n %>% activate("nodes") %>% 
  left_join(as.data.table(t(distances(graph = n,v="sjukhus", mode="out"))) %>% 
              mutate(id = rownames(.)) %>%
              rename("distans sjukhus" = "sjukhus"),
            by = c("name" = "id")) %>%
  #left_join(as.data.table(t(distances(graph = n,v="sjukhus", mode="out", weights = n %>% activate("edges")  %>% pull(weight)))) %>% 
  #mutate(id = rownames(.)) %>%
  #rename("distans sjukhus" = "sjukhus"),
  #by = c("name" = "id")) %>% 
  mutate(`distans sjukhus` = replace_na(`distans sjukhus`,0))

