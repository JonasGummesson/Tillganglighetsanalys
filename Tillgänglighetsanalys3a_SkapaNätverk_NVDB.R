library(DBI)
############################ Skapa n�tverk NVDB ##############################
# skapar n�tverk av v�gn�t samt kopplar ihop med utbudsdata

############## Data fr�n lantm�teriet ################


con_Sandbox <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = "WFALMITSR036",
                      Database = "Sandbox_Jonas",
                      Trusted_Connection = "True")

# nvdb enbart falun
net_v�gn�t_nvdb_falun  <- as_sfnetwork(sf_v�gn�t_nvdb_falun, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  mutate(edgeId = paste0(from, "-", to)) 

# l�gg till koordinater f�r noderna till varje b�ge
net_v�gn�t_nvdb_falun <- net_v�gn�t_nvdb_falun %>%
  activate(edges) %>%
  mutate(xNode=NA, yNode=NA) %>%
  inner_join(st_as_sf(net_v�gn�t_nvdb_falun, "nodes") %>% as.data.table(), by=c("to"="nodeId"), suffix=c("", ".node1"))%>%
  inner_join(st_as_sf(net_v�gn�t_nvdb_falun, "nodes") %>% as.data.table(), by=c("from"="nodeId"), suffix=c("", ".node2"))

#hela dalarna
net_v�gn�t_nvdb  <- as_sfnetwork(sf_v�gn�t_nvdb, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(nodeId = row_number()) %>%
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2])%>%
  activate("edges") %>%
  mutate(str�cka_m =  edge_length()) %>%
  mutate(hastighet_m_per_minut = (HTHAST*1000/60)) %>%
  mutate(tid_m = str�cka_m/hastighet_m_per_minut) %>%
  mutate(weight =tid_m)  %>%
  select(sf_edge_id, from, to, HTHAST, str�cka_m, hastighet_m_per_minut, tid_m, weight)


dbWriteTable(con_Sandbox, name = "Vagnat_NVDB_nodes", value = net_v�gn�t_nvdb %>% st_as_sf("nodes") %>% as.data.table() %>% mutate(test = sf::st_as_text(geometry)), overwrite=TRUE)
dbWriteTable(con_Sandbox, name =  "Vagnat_NVDB_edges", value = net_v�gn�t_nvdb %>% st_as_sf("edges")  %>% as.data.table() %>% mutate(test = sf::st_as_text(geometry)), overwrite=TRUE)


net_v�gn�t_nvdb_str�cka <- net_v�gn�t_nvdb %>%
  activate("edges") %>%
  mutate(weight = str�cka_m)
         
p1 <- ggplot(sf_v�gn�t_nvdb_falun)+geom_sf()

p2 <- ggplot()+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "edges"), color = "red", alpha=0.5)

p <- ggmap(map_Falun)+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "nodes"), color=  "darkblue", inherit.aes = FALSE)+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "edges"), color = "red", alpha=0.5,  inherit.aes = FALSE)



#grid.arrange(p1,p2, ncol=2)

p <- ggplot()+
  geom_sf(data = st_as_sf(net_v�gn�t_lm_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_v�gn�t_lm_falun, "edges"), color = "red", alpha=0.5)+
  geom_text(data = st_as_sf(net_v�gn�t_lm_falun, "edges"), aes(label=sprintf("%.0f m", weight), x = xEdge, y = yEdge))


############## Data fr�n NVDB ################


#net_v�gn�t_nvdb_falun  <- as_sfnetwork(sf_v�gn�t_nvdb_falun, directed = FALSE)

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
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "nodes"), size=1)+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun_lines, "edges"), color = "red")+
  theme(legend.position = "None")

# kolla slaskfil f�r ytterligare kod f�r att splitta b�gar etc!

################### identifiera utbudspunkter nvdb #################################

utbud_noder <- sf_utbud %>% 
  mutate(x =  st_coordinates(geometry)[,1], y =  st_coordinates(geometry)[,2]) %>%
  group_by(Popul�rnamn, x, y, V�rdtypGrupp) %>%
  summarise() %>% 
  as.data.frame() %>%
  select(Popul�rnamn, x, y, V�rdtypGrupp)


v�gn�t_noder_nvdb_falun <- st_as_sf(net_v�gn�t_nvdb_falun, "nodes") %>% as.data.frame() %>% select(nodeId, xNode, yNode)
v�gn�t_noder_nvdb <- st_as_sf(net_v�gn�t_nvdb, "nodes") %>% as.data.frame() %>% select(nodeId, xNode, yNode)

#lm n�rmaste nod falun
sf_utbud_nvdb_falun <-  utbud_noder %>% 
  full_join(v�gn�t_noder_nvdb_falun , by=character()) %>%
  group_by(V�rdtypGrupp,Popul�rnamn) %>%
  mutate(distUtbud = sqrt((xNode-x)^2+(yNode-y)^2))%>%
  mutate(rankUtbud = rank(distUtbud))%>%
  mutate(utbudNamn = ifelse(rankUtbud == 1, Popul�rnamn, "")) %>%
  filter(utbudNamn != "") %>%
 #mutate(test = which(colnames(.) == "xNode")) %>%
  sf::st_as_sf(coords = c(which(colnames(.) == "xNode"), which(colnames(.) == "yNode")))%>%
  st_set_crs(3006)


#lm n�rmaste nod dalarna
sf_utbud_nvdb <-  utbud_noder %>% 
  full_join(v�gn�t_noder_nvdb  , by=character()) %>%
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
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb_falun, "edges"), color = "red", alpha=0.5)+
  geom_sf(data = sf_utbud_nvdb_falun %>% filter(tolower(Popul�rnamn) %like%"lasarett"), aes(color=Popul�rnamn), size=5)

#plotta Dalarna mot lasarett i Dalarna
p <- ggplot()+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb, "nodes"), color=  "darkblue")+
  geom_sf(data = st_as_sf(net_v�gn�t_nvdb, "edges"), color = "darkred", alpha=1)+
  geom_sf(data = sf_utbud_nvdb %>% filter(tolower(Popul�rnamn) %like%"lasarett"), aes(color=Popul�rnamn), size=5)


