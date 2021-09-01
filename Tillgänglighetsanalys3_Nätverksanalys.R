# OBS! Under utveckling!!!!
############################ Nätverksanalys ##############################

# lm
net_falun_lm  <- as_sfnetwork(sf_falun_lm, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(edgeId = paste0(from, "-", to))


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


net_falun_parts  <- as_sfnetwork(sf_falun_nvdb_parts, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(edgeId = paste0(from, "-", to))%>%
  mutate(xEdge = st_coordinates(st_centroid(geometry))[,1], yEdge = st_coordinates(st_centroid(geometry))[,2])

#### utvärdera net_falun mot net_falun_parts


ggplot()+
  geom_sf(data = st_as_sf(net_falun, "nodes"), color="green", size=3)+
  geom_sf(data = st_as_sf(net_falun_parts, "nodes"), color=  "blue")+
  geom_sf(data = st_as_sf(net_falun_parts, "edges"), color = "red", linetype="dashed")+
  geom_sf(data = st_as_sf(net_falun_parts_lines, "edges"), color = "red")

ggplot()+
  geom_sf(data = st_as_sf(net_falun, "nodes"), color="green", size=3)+
  geom_sf(data = st_as_sf(net_falun_parts, "nodes"), color=  "blue")+
  geom_sf(data = st_as_sf(net_falun_parts, "edges"), color = "red")+
  geom_text_repel(data = st_as_sf(net_falun_parts, "edges"), aes(label = edgeId, x = xEdge, y = yEdge), color = "blue", size=4)  +
  geom_text_repel(data = st_as_sf(net_falun_parts, "nodes"), aes(label = nodeId, x = xNode, y = yNode), color = "red", size=4)  


### identifiera utbudspunkter

utbud_test <- sf_utbud %>% 
  filter(Populärnamn %like% "Falu Lasarett") %>% group_by(Populärnamn, geometry) %>% 
  summarise() %>% 
  as.data.frame() %>%
  mutate(x =  st_coordinates(geometry)[,1], y =  st_coordinates(geometry)[,2]) %>%
  select(Populärnamn, x, y)

net_falun_lm %>%
  activate("nodes") %>%
  mutate(k=1) %>%
  inner_join(utbud_test %>% mutate(k=1), by=c("k"="k")) %>%
  mutate(distUtbud = sqrt((xNode-x)^2+(yNode-y)^2))%>%
  mutate(rankUtbud = rank(distUtbud))%>%
  mutate(utbudNamn = ifelse())