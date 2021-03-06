############################ Skapa n�tverk ##############################
# skapar n�tverk av v�gn�t samt kopplar ihop med utbudsdata

############## Data fr�n lantm�teriet ################

# lm enbart falun
net_v�gn�t_lm_falun  <- as_sfnetwork(sf_v�gn�t_lm_falun, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(edgeId = paste0(from, "-", to)) 

# l�gg till koordinater f�r noderna till varje b�ge
net_v�gn�t_lm_falun <- net_v�gn�t_lm_falun %>%
  activate(edges) %>%
  mutate(xNode=NA, yNode=NA) %>%
  inner_join(st_as_sf(net_v�gn�t_lm_falun, "nodes") %>% as.data.table(), by=c("to"="nodeId"), suffix=c("", ".node1"))%>%
  inner_join(st_as_sf(net_v�gn�t_lm_falun, "nodes") %>% as.data.table(), by=c("from"="nodeId"), suffix=c("", ".node2"))

#lm dalarna
net_v�gn�t_lm  <- as_sfnetwork(sf_v�gn�t_lm, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) 

#net_v�gn�t_lm <- net_v�gn�t_lm %>%
  #activate(edges) %>%
  #mutate(xNode=NA, yNode=NA) %>%
  #inner_join(st_as_sf(net_v�gn�t_lm, "nodes") %>% as.data.table(), by=c("to"="nodeId"), suffix=c("", ".node1"))%>%
  #inner_join(st_as_sf(net_v�gn�t_lm, "nodes") %>% as.data.table(), by=c("from"="nodeId"), suffix=c("", ".node2")) 
  

#plotta Falun 

p1 <- ggplot(sf_v�gn�t_lm_falun)+geom_sf()

p2 <- ggplot()+
  geom_sf(data = st_as_sf(net_v�gn�t_lm_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_v�gn�t_lm_falun, "edges"), color = "red", alpha=0.5)

#grid.arrange(p1,p2, ncol=2)

p <- ggplot()+
  geom_sf(data = st_as_sf(net_v�gn�t_lm_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_v�gn�t_lm_falun, "edges"), color = "red", alpha=0.5)+
  geom_text(data = st_as_sf(net_v�gn�t_lm_falun, "edges"), aes(label=sprintf("%.0f m", weight), x = xEdge, y = yEdge))


############## Data fr�n NVDB ################

# nvdb utan att splitta
net_v�gn�t_nvdb_falun  <- as_sfnetwork(sf_v�gn�t_nvdb_falun, directed = FALSE)

nodes_v�gn�t_nvdb_falun = as_sfnetwork(sf_v�gn�t_nvdb_falun) %>%
  activate("nodes") %>%
  st_as_sf()

edges_v�gn�t_nvdb_falun = as_sfnetwork(sf_v�gn�t_nvdb_falun) %>%
  activate("edges") %>%
  st_as_sf()
st_geometry(edges_v�gn�t_nvdb_falun) = NULL

net_v�gn�t_nvdb_falun_lines  <- sfnetwork(nodes = nodes_v�gn�t_nvdb_falun, edges = edges_v�gn�t_nvdb_falun, directed = FALSE, edges_as_lines = TRUE)

# ojojoj, stora problem h�r!
p <- ggplot()+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "edges") %>% mutate(name = paste0(from, "-", to)), aes(color=name), linetype="dashed")+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "nodes"))+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun_lines, "edges"), color = "red")

# kolla slaskfil f�r ytterligare kod f�r att splitta b�gar etc!

################### identifiera utbudspunkter lm #################################

utbud_noder <- sf_utbud %>% 
  mutate(x =  st_coordinates(geometry)[,1], y =  st_coordinates(geometry)[,2]) %>%
  group_by(Popul�rnamn, x, y, V�rdtypGrupp) %>%
  summarise() %>% 
  as.data.frame() %>%
  select(Popul�rnamn, x, y, V�rdtypGrupp)


v�gn�t_noder_lm_falun <- st_as_sf(net_v�gn�t_lm_falun, "nodes") %>% as.data.frame() %>% select(nodeId, xNode, yNode)
v�gn�t_noder_lm <- st_as_sf(net_v�gn�t_lm, "nodes") %>% as.data.frame() %>% select(nodeId, xNode, yNode)

#lm n�rmaste nod falun
sf_utbud_lm_falun <-  utbud_noder %>% 
  full_join(v�gn�t_noder_lm_falun , by=character()) %>%
  group_by(V�rdtypGrupp,Popul�rnamn) %>%
  mutate(distUtbud = sqrt((xNode-x)^2+(yNode-y)^2))%>%
  mutate(rankUtbud = rank(distUtbud))%>%
  mutate(utbudNamn = ifelse(rankUtbud == 1, Popul�rnamn, "")) %>%
  filter(utbudNamn != "") %>%
 #mutate(test = which(colnames(.) == "xNode")) %>%
  sf::st_as_sf(coords = c(which(colnames(.) == "xNode"), which(colnames(.) == "yNode")))%>%
  st_set_crs(3006)


#lm n�rmaste nod dalarna
sf_utbud_lm <-  utbud_noder %>% 
  full_join(v�gn�t_noder_lm , by=character()) %>%
  group_by(V�rdtypGrupp, Popul�rnamn) %>%
  mutate(distUtbud = sqrt((xNode-x)^2+(yNode-y)^2))%>%
  mutate(rankUtbud = rank(distUtbud))%>%
  mutate(utbudNamn = ifelse(rankUtbud == 1, Popul�rnamn, "")) %>%
  arrange(rankUtbud) %>%
  filter(utbudNamn != "") %>%
  sf::st_as_sf(coords = c(which(colnames(.) == "xNode"), which(colnames(.) == "yNode"))) %>%
  st_set_crs(3006)


#plotta Falun mot lasarett i Dalarna
p <- ggplot()+
  geom_sf(data = st_as_sf(net_v�gn�t_lm_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_v�gn�t_lm_falun, "edges"), color = "red", alpha=0.5)+
  geom_sf(data = sf_utbud_lm_falun %>% filter(tolower(Popul�rnamn) %like%"lasarett"), aes(color=Popul�rnamn), size=5)

#plotta Dalarna mot lasarett i Dalarna
p <- ggplot()+
  geom_sf(data = st_as_sf(net_v�gn�t_lm, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_v�gn�t_lm, "edges"), color = "darkred", alpha=1)+
  geom_sf(data = sf_utbud_lm %>% filter(tolower(Popul�rnamn) %like%"lasarett"), aes(color=Popul�rnamn), size=5)


