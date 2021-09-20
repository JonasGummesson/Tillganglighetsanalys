# indata
# sf_isokroner
# sf_individer


library(microbenchmark)
### beräkna andelar per kommun och intervall

mbm <- microbenchmark(
sf_result <-
  sf_individer %>%
  sample_n(nrow(sf_individer)*0.01) %>% # sampla % av folkbokförda
  st_join(sf_isokroner_restid_intervall %>% filter(VårdtypGrupp == "Somatik akut"), join = st_within, left=TRUE) %>%
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
#?rowSums
#autoplot(mbm)
#?pivot_longer
sf_result %>%
  pivot_longer(cols = -Kommun, names_to = "Intervall", values_to="Pct")%>%
  ggplot()+
  geom_bar(aes(x=Kommun, y=Pct, fill=Intervall), stat="identity")+
  theme_minimal()+
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  #theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_viridis_d(option = "plasma", direction=-1)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = "Andel folkbokförda", fill = "Restid minuter", title = "Restid till somatisk akutmottagning")+
  scale_y_continuous(labels=scales::percent)

## Aggregera utbudspunkter innan isokronberäkning

#dist_vårdtypgrupp_nodes %>% nest(cols = VårdtypGrupp)

mbm_dist_vårdtypgrupp_nodes = microbenchmark(
dist_vårdtypgrupp_nodes <-
dist_dalarna_nvdb %>% 
  ungroup()%>%
  #filter(VårdtypGrupp == "Somatik akut") %>%
  rowwise()%>%
  select(VårdtypGrupp, utbudNamn, net) %>%
  mutate(dt = list(map(.x = net, .f = function(x) { 
    net %>% 
      activate("nodes") %>% 
      as.data.table() %>% 
      select(nodeId, distans, restid_minuter) 
  })[[1]]))%>%
  select(VårdtypGrupp, utbudNamn, dt) %>%
  unnest(col = dt) %>%
  unnest(col = c(nodeId, distans, restid_minuter)) %>%
  group_by(VårdtypGrupp, nodeId) %>%
  summarise(distans = min(distans),
            resväg_minuter = min(restid_minuter)),
times = 1)

mbm_dist_vårdtypgrupp_edges = microbenchmark(
{
dist_vårdtypgrupp_edges <- 
st_as_sf(net_vägnät_nvdb, "edges") %>%
  as_tibble() %>%
  full_join(dist_vårdtypgrupp_nodes %>% distinct(VårdtypGrupp), by=character()) %>%
  select(VårdtypGrupp, from, to, sf_edge_id) %>%
  inner_join(dist_vårdtypgrupp_nodes %>% ungroup() %>% select(nodeId, distans, resväg_minuter, VårdtypGrupp), by=c("from"="nodeId", "VårdtypGrupp" = "VårdtypGrupp")) %>%
  rename("distans.nod1" = "distans",
         "restid_minuter.nod1" = "restid_minuter") %>%
  inner_join(dist_vårdtypgrupp_nodes %>% ungroup() %>% select(nodeId, distans, resväg_minuter, VårdtypGrupp), by=c("to"="nodeId", "VårdtypGrupp" = "VårdtypGrupp")) %>%
  rename("distans.nod2" = "distans",
         "restid_minuter.nod2" = "restid_minuter")) %>%
  rowwise()%>%
  mutate(distans.nodMax = max(distans.nod1, distans.nod2),
         distans.nodMean = mean(distans.nod1, distans.nod2),
         restid_minuter.nodMax = max(restid_minuter.nod1, restid_minuter.nod2)) %>%
  mutate(distans.nodMaxKm = distans.nodeMax/1000,
         distans.nodMeanKm = distans.nodeMean/1000)
},
times = 1)



p <- ggplot()+
  geom_sf(data = st_as_sf(net_vägnät_nvdb, "edges") %>% inner_join(dist_vårdtypgrupp_edges %>% filter(VårdtypGrupp != "AmbulansSatellitstation"), by=c("sf_edge_id"="sf_edge_id")), aes(color=distance.nodeMaxKm))+
  facet_wrap(~VårdtypGrupp)+
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
  geom_sf(data = st_as_sf(net_vägnät_nvdb, "nodes") %>% select(nodeId) %>% inner_join(dist_vårdtypgrupp %>% filter(distance == Inf), by=c("nodeId"="nodeId")), color="blue", size=2)+
    geom_sf(data = st_as_sf(net_vägnät_nvdb, "edges") %>% inner_join(dist_vårdtypgrupp_edges, by=c("sf_edge_id"="sf_edge_id")) %>% filter(distance.nodeMaxKm == Inf), color = "red")+

  # geom_text_repel(data = st_as_sf(net_falun_lm, "edges"), aes(label = paste0(to, "-", from), x = xEdge, y = yEdge), color = "blue", size=4)  +
  # geom_sf(data =   dist_dalarna_lm %>% head(1), aes(x=utbudX, y=utbudY), color="purple", size=5)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (km)",
                        na.value = "grey100") 

