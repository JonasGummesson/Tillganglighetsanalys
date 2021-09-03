# OBS! Under utveckling!!!!
############################ Nätverksanalys ##############################

# lm enbart falun
net_falun_lm  <- as_sfnetwork(sf_falun_lm, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(edgeId = paste0(from, "-", to))

#lm dalarna
net_dalarna_lm  <- as_sfnetwork(sf_vägnät_dalarna_lm, directed = FALSE) %>%
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



utbud_noder <- sf_utbud %>% 
  mutate(x =  st_coordinates(geometry)[,1], y =  st_coordinates(geometry)[,2]) %>%
  group_by(Populärnamn, x, y, VårdtypGrupp) %>%
  summarise() %>% 
  as.data.frame() %>%
  select(Populärnamn, x, y, VårdtypGrupp) #%>%
 #filter(Populärnamn %like% "Falu Lasarett" | Populärnamn %like% "Mora lasarett") 


vägnät_noder_falun_lm <- st_as_sf(net_falun_lm, "nodes") %>% as.data.frame() %>% select(nodeId, xNode, yNode)
vägnät_noder_dalarna_lm <- st_as_sf(net_dalarna_lm, "nodes") %>% as.data.frame() %>% select(nodeId, xNode, yNode)

#lm närmaste nod falun
sf_utbud_falun_lm_närmaste_nod <-  utbud_noder %>% 
  mutate(k=1) %>%
  inner_join(vägnät_noder_falun_lm %>% mutate(k=1), by=c("k" = "k")) %>%
  group_by(VårdtypGrupp,Populärnamn) %>%
  mutate(distUtbud = sqrt((xNode-x)^2+(yNode-y)^2))%>%
  mutate(rankUtbud = rank(distUtbud))%>%
  mutate(utbudNamn = ifelse(rankUtbud == 1, Populärnamn, "")) %>%
  filter(utbudNamn != "") %>%
 #mutate(test = which(colnames(.) == "xNode")) %>%
  sf::st_as_sf(coords = c(which(colnames(.) == "xNode"), which(colnames(.) == "yNode"))) 
st_crs(sf_utbud_falun_lm_närmaste_nod) = st_crs(3006)


#lm närmaste nod dalarna
sf_utbud_dalarna_lm_närmaste_nod <-  utbud_noder %>% 
  #filter(tolower(Populärnamn) %like% "lasarett") %>%
  mutate(k=1) %>%
  inner_join(vägnät_noder_dalarna_lm %>% mutate(k=1), by=c("k" = "k")) %>%
  group_by(VårdtypGrupp, Populärnamn) %>%
  mutate(distUtbud = sqrt((xNode-x)^2+(yNode-y)^2))%>%
  mutate(rankUtbud = rank(distUtbud))%>%
  mutate(utbudNamn = ifelse(rankUtbud == 1, Populärnamn, "")) %>%
  arrange(rankUtbud) %>%
  filter(utbudNamn != "") %>%
  #mutate(test = which(colnames(.) == "xNode")) %>%
  sf::st_as_sf(coords = c(which(colnames(.) == "xNode"), which(colnames(.) == "yNode"))) 
st_crs(sf_utbud_dalarna_lm_närmaste_nod) = st_crs(3006)


#sf_utbud_närmaste_nod %>% mutate(t = tolower(Populärnamn)) %>% filter(t )

ggplot()+
  geom_sf(data = st_as_sf(net_falun_lm, "nodes"), color=  "blue")+
  geom_sf(data = st_as_sf(net_falun_lm, "edges"), color = "red")+
  geom_sf(data = sf_utbud_falun_lm_närmaste_nod %>% filter(tolower(Populärnamn) %like%"lasarett"), aes(color=Populärnamn), size=5)

net_falun_lm %>%
  activate("nodes") %>%
  left_join(sf_utbud_falun_lm_närmaste_nod %>% as.data.frame() %>% filter(tolower(Populärnamn) %like%"lasarett"), by=c("nodeId" = "nodeId")) %>%
  filter(!is.na(Populärnamn))


ggplot()+
  geom_sf(data = st_as_sf(net_falun_lm, "nodes"), color=  "blue")+
  geom_sf(data = st_as_sf(net_falun_lm, "edges"), color = "red")+
  geom_sf(data = st_as_sf(net_falun_lm %>%
            activate("nodes") %>%
            filter(nodeId %in% (sf_utbud_falun_lm_närmaste_nod %>% filter(tolower(Populärnamn) %like%"lasarett") %>% pull(nodeId)))
          , aes(color=Populärnamn), size=5)

#   geom_text_repel(data = st_as_sf(net_dalarna_lm, "nodes"), aes(label = distUtbud, x = xNode, y = yNode), color = "red", size=4)  

ggplot()+
  geom_sf(data = st_as_sf(net_dalarna_lm, "nodes"), color=  "blue")+
  geom_sf(data = st_as_sf(net_dalarna_lm, "edges"), color = "red")+
  geom_sf(data = sf_utbud_dalarna_lm_närmaste_nod %>% filter(tolower(Populärnamn) %like%"lasarett"), aes(color=Populärnamn), size=5)
  #geom_text_repel(data = st_as_sf(net_falun_lm_utbud, "nodes"), aes(label = distUtbud, x = xNode, y = yNode), color = "red", size=4)  



#net_falun_lm_utbud %>% activate(nodes) %>% filter(utbudNamn == "utbudspunkt")

