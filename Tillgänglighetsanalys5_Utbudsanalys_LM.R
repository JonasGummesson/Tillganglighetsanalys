# indata
# sf_isokroner
# sf_individer


library(microbenchmark)
### beräkna andelar per kommun och intervall

mbm <- microbenchmark(
sf_result <-
  sf_individer %>%
  sample_n(nrow(sf_individer)*0.005) %>% # sampla % av folkbokförda
  st_join(sf_isokroner_intervall %>% filter(VårdtypGrupp == "Somatik akut"), join = st_within, left=TRUE) %>%
  mutate(intervallDistans = ifelse(is.na(intervallDistans), "Övriga", intervallDistans)) %>%
  as.data.table() %>%
  group_by(Kommun, intervallDistans) %>% 
  summarise(AntalFolkbokförda = n()) %>%
  pivot_wider(id_cols = c(Kommun), names_from = "intervallDistans", values_from=AntalFolkbokförda) %>%
  ungroup()%>%
  rowwise() %>%
  mutate(Sum = as.integer(rowSums(across(where(is.integer)),na.rm=TRUE))) %>%
  mutate(across(where(is_integer), ~replace_na(.,0))) %>%
  mutate(across(.cols = -one_of("Kommun"), .fns = ~ .x/Sum)) %>%
  #mutate(pct = across(.cols = -one_of("Kommun"), .fns = ~ .x/Sum)) %>%
  select(-Sum),
  times = 1
)
?rowSums
#autoplot(mbm)
?pivot_longer
sf_result %>%
  pivot_longer(cols = -Kommun, names_to = "Intervall", values_to="Pct")%>%
  ggplot()+
  geom_bar(aes(x=Kommun, y=Pct, fill=Intervall), stat="identity")+
  theme_minimal()+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  #theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_viridis_d(option = "plasma", direction=-1)+
  theme(axis.text.x = element_text(angle = 90))

## Aggregera utbudspunkter innan isokronberäkning

#dist_vårdtypgrupp_nodes %>% nest(cols = VårdtypGrupp)

dist_vårdtypgrupp_nodes <-
dist_dalarna_lm %>% 
  ungroup()%>%
  #filter(VårdtypGrupp == "Somatik akut") %>%
  rowwise()%>%
  select(VårdtypGrupp, utbudNamn, net) %>%
  mutate(dt = list(map(.x = net, .f = function(x) { 
    net %>% 
      activate("nodes") %>% 
      as.data.table() %>% 
      select(nodeId, distance) 
  })[[1]]))%>%
  select(VårdtypGrupp, utbudNamn, dt) %>%
  unnest(col = dt) %>%
  unnest(col = c(nodeId, distance)) %>%
  group_by(VårdtypGrupp, nodeId) %>%
  summarise(distance = min(distance))

dist_vårdtypgrupp_edges <- 
st_as_sf(net_vägnät_lm, "edges") %>%
  as_tibble() %>%
  full_join(dist_vårdtypgrupp_nodes %>% distinct(VårdtypGrupp), by=character()) %>%
  select(VårdtypGrupp, from, to, sf_edge_id) %>%
  inner_join(dist_vårdtypgrupp_nodes %>% ungroup() %>% select(nodeId, distance, VårdtypGrupp), by=c("from"="nodeId", "VårdtypGrupp" = "VårdtypGrupp")) %>%
  rename("distance.node1" = "distance") %>%
  inner_join(dist_vårdtypgrupp_nodes %>% ungroup() %>% select(nodeId, distance, VårdtypGrupp), by=c("to"="nodeId", "VårdtypGrupp" = "VårdtypGrupp")) %>%
  rename("distance.node2" = "distance") %>%
  rowwise()%>%
  mutate(distance.nodeMax = max(distance.node1, distance.node2),
         distance.nodeMean = mean(distance.node1, distance.node2)) %>%
  mutate(distance.nodeMaxKm = distance.nodeMax/1000,
         distance.nodeMeanKm = distance.nodeMean/1000)



p <- ggplot()+
  geom_sf(data = st_as_sf(net, "edges") %>% inner_join(dist_vårdtypgrupp_edges, by=c("sf_edge_id"="sf_edge_id")), aes(color=distance.nodeMaxKm))+
  facet_wrap(~VårdtypGrupp)+
  #geom_sf(data = st_as_sf(net, "nodes") %>% select(nodeId) %>% inner_join(dist_vårdtypgrupp %>% mutate(distanceKm = distance/1000), by=c("nodeId"="nodeId")), aes(color=distance), size=2)+
  # geom_text_repel(data = st_as_sf(net_falun_lm, "edges"), aes(label = paste0(to, "-", from), x = xEdge, y = yEdge), color = "blue", size=4)  +
  # geom_sf(data =   dist_dalarna_lm %>% head(1), aes(x=utbudX, y=utbudY), color="purple", size=5)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.background = element_rect(fill = 'black', colour = 'red'))+
  scale_color_gradientn(colours=magma(256, begin=1, end=0),
                        values = c(0,0.2,1),
                        name="Distans (km)",
                        na.value = "black") 

# noder som inte kan nås
p <- ggplot()+
  geom_sf(data = st_as_sf(net, "nodes") %>% select(nodeId) %>% inner_join(dist_vårdtypgrupp %>% filter(distance == Inf), by=c("nodeId"="nodeId")), color="blue", size=2)+
    geom_sf(data = st_as_sf(net, "edges") %>% inner_join(dist_vårdtypgrupp_edges, by=c("sf_edge_id"="sf_edge_id")) %>% filter(distance.nodeMaxKm == Inf), color = "red")+

  # geom_text_repel(data = st_as_sf(net_falun_lm, "edges"), aes(label = paste0(to, "-", from), x = xEdge, y = yEdge), color = "blue", size=4)  +
  # geom_sf(data =   dist_dalarna_lm %>% head(1), aes(x=utbudX, y=utbudY), color="purple", size=5)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (km)",
                        na.value = "grey100") 

