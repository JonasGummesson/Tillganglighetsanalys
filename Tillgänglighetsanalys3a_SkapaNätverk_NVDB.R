############################ Skapa nätverk ##############################
# skapar nätverk av vägnät samt kopplar ihop med utbudsdata

############## Data från lantmäteriet ################

# nvdb enbart falun
net_vägnät_nvdb_falun  <- as_sfnetwork(sf_vägnät_nvdb_falun, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(edgeId = paste0(from, "-", to)) 

# lägg till koordinater för noderna till varje båge
net_vägnät_nvdb_falun <- net_vägnät_nvdb_falun %>%
  activate(edges) %>%
  mutate(xNode=NA, yNode=NA) %>%
  inner_join(st_as_sf(net_vägnät_nvdb_falun, "nodes") %>% as.data.table(), by=c("to"="nodeId"), suffix=c("", ".node1"))%>%
  inner_join(st_as_sf(net_vägnät_nvdb_falun, "nodes") %>% as.data.table(), by=c("from"="nodeId"), suffix=c("", ".node2"))

#lm dalarna
net_vägnät_nvdb  <- as_sfnetwork(sf_vägnät_nvdb, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) 



p1 <- ggplot(sf_vägnät_lm_falun)+geom_sf()

p2 <- ggplot()+
  geom_sf(data = st_as_sf(net_vägnät_lm_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_vägnät_lm_falun, "edges"), color = "red", alpha=0.5)

ggmap(map_Falun)+
  geom_sf(data = st_as_sf(net_vägnät_nvdb_falun, "nodes"), color=  "darkblue", inherit.aes = FALSE)+
  geom_sf(data = st_as_sf(net_vägnät_nvdb_falun, "edges"), color = "red", alpha=0.5,  inherit.aes = FALSE)



#grid.arrange(p1,p2, ncol=2)

p <- ggplot()+
  geom_sf(data = st_as_sf(net_vägnät_lm_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_vägnät_lm_falun, "edges"), color = "red", alpha=0.5)+
  geom_text(data = st_as_sf(net_vägnät_lm_falun, "edges"), aes(label=sprintf("%.0f m", weight), x = xEdge, y = yEdge))


############## Data från NVDB ################

# nvdb utan att splitta
net_vägnät_nvdb_falun  <- as_sfnetwork(sf_vägnät_nvdb_falun, directed = FALSE)

nodes_vägnät_nvdb_falun = as_sfnetwork(sf_vägnät_nvdb_falun) %>%
  activate("nodes") %>%
  st_as_sf()

edges_vägnät_nvdb_falun = as_sfnetwork(sf_vägnät_nvdb_falun) %>%
  activate("edges") %>%
  st_as_sf()
st_geometry(edges_vägnät_nvdb_falun) = NULL

net_vägnät_nvdb_falun_lines  <- sfnetwork(nodes = nodes_vägnät_nvdb_falun, edges = edges_vägnät_nvdb_falun, directed = FALSE, edges_as_lines = TRUE)

# ojojoj, stora problem här!
p <- ggplot()+
  geom_sf(data = st_as_sf(net_vägnät_nvdb_falun, "edges") %>% mutate(name = paste0(from, "-", to)), aes(color=name), linetype="dashed")+
  geom_sf(data = st_as_sf(net_vägnät_nvdb_falun, "nodes"))+
  geom_sf(data = st_as_sf(net_vägnät_nvdb_falun_lines, "edges"), color = "red")

# kolla slaskfil för ytterligare kod för att splitta bågar etc!

################### identifiera utbudspunkter vdbn #################################

utbud_noder <- sf_utbud %>% 
  mutate(x =  st_coordinates(geometry)[,1], y =  st_coordinates(geometry)[,2]) %>%
  group_by(Populärnamn, x, y, VårdtypGrupp) %>%
  summarise() %>% 
  as.data.frame() %>%
  select(Populärnamn, x, y, VårdtypGrupp)


vägnät_noder_nvdb_falun <- st_as_sf(net_vägnät_nvdb_falun, "nodes") %>% as.data.frame() %>% select(nodeId, xNode, yNode)
vägnät_noder_nvdb <- st_as_sf(net_vägnät_nvdb, "nodes") %>% as.data.frame() %>% select(nodeId, xNode, yNode)

#lm närmaste nod falun
sf_utbud_nvdb_falun <-  utbud_noder %>% 
  full_join(vägnät_noder_nvdb_falun , by=character()) %>%
  group_by(VårdtypGrupp,Populärnamn) %>%
  mutate(distUtbud = sqrt((xNode-x)^2+(yNode-y)^2))%>%
  mutate(rankUtbud = rank(distUtbud))%>%
  mutate(utbudNamn = ifelse(rankUtbud == 1, Populärnamn, "")) %>%
  filter(utbudNamn != "") %>%
 #mutate(test = which(colnames(.) == "xNode")) %>%
  sf::st_as_sf(coords = c(which(colnames(.) == "xNode"), which(colnames(.) == "yNode")))%>%
  st_set_crs(3006)


#lm närmaste nod dalarna
sf_utbud_nvdb <-  utbud_noder %>% 
  full_join(vägnät_noder_nvdb  , by=character()) %>%
  group_by(VårdtypGrupp, Populärnamn) %>%
  mutate(distUtbud = sqrt((xNode-x)^2+(yNode-y)^2))%>%
  mutate(rankUtbud = rank(distUtbud))%>%
  mutate(utbudNamn = ifelse(rankUtbud == 1, Populärnamn, "")) %>%
  arrange(rankUtbud) %>%
  filter(utbudNamn != "") %>%
  sf::st_as_sf(coords = c(which(colnames(.) == "xNode"), which(colnames(.) == "yNode"))) %>%
  st_set_crs(3006)


#plotta Falun mot lasarett i Dalarna
p <- ggplot()+
  geom_sf(data = st_as_sf(net_vägnät_nvdb_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_vägnät_nvdb_falun, "edges"), color = "red", alpha=0.5)+
  geom_sf(data = sf_utbud_nvdb_falun %>% filter(tolower(Populärnamn) %like%"lasarett"), aes(color=Populärnamn), size=5)

#plotta Dalarna mot lasarett i Dalarna
p <- ggplot()+
  geom_sf(data = st_as_sf(net_vägnät_nvdb, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_vägnät_nvdb, "edges"), color = "darkred", alpha=1)+
  geom_sf(data = sf_utbud_nvdb %>% filter(tolower(Populärnamn) %like%"lasarett"), aes(color=Populärnamn), size=5)


