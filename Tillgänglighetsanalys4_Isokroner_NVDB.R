library(concaveman)

################################# beräkna isokroner NVDB ############################

sf_isokroner_resväg <- dist_dalarna_nvdb %>% 
  select(VårdtypGrupp, utbudNamn, net) %>%
  full_join(tibble(isokronDistans = c(10000,20000,30000,50000)), by=character()) %>%
  mutate(sf_alphaShape = map2(.x = isokronDistans, .y = net, .f = function(x,y) { 
        concaveman( st_as_sf(y, "nodes") %>% filter(distans < x), concavity = 1)
    }))%>%
  unnest(cols = "sf_alphaShape") %>%
  st_as_sf() %>%
  select(utbudNodeId, VårdtypGrupp, utbudNamn, isokronDistans, polygons) %>%
  st_buffer(200) %>% # lägg till 200 m buffertzon kring varje väg
  st_simplify(dTolerance = 500) %>%
  arrange(desc(isokronDistans)) %>%
  mutate(isokronDistansFct=factor(isokronDistans, levels = unique(isokronDistans)))


sf_isokroner_restid <- dist_dalarna_nvdb %>% 
  select(VårdtypGrupp, utbudNamn, net) %>%
  full_join(tibble(isokronDistans = c(15,30,45,60)), by=character()) %>%
  mutate(sf_alphaShape = map2(.x = isokronDistans, .y = net, .f = function(x,y) { 
    concaveman( st_as_sf(y, "nodes") %>% filter(restid_minuter < x), concavity = 1)
  }))%>%
  unnest(cols = "sf_alphaShape") %>%
  st_as_sf() %>%
  select(utbudNodeId, VårdtypGrupp, utbudNamn, isokronDistans, polygons) %>%
  st_buffer(200) %>% # lägg till 200 m buffertzon kring varje väg
  st_simplify(dTolerance = 500) %>%
  arrange(desc(isokronDistans)) %>%
  mutate(isokronDistansFct=factor(isokronDistans, levels = unique(isokronDistans)))


p1<-ggplot(data = sf_isokroner_resväg) + 
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(color = "black", aes(fill = isokronDistansFct)) + 
  facet_wrap(~VårdtypGrupp)+
  scale_fill_viridis_d(option = "plasma", direction=1)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


p2<-ggplot(data = sf_isokroner_restid) + 
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(color = "black", aes(fill = isokronDistansFct)) + 
  facet_wrap(~VårdtypGrupp)+
  scale_fill_viridis_d(option = "plasma", direction=1)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

grid.arrange(p1,p2)

# skapa intervall-shapes
sf_isokroner_resväg_intervall <- sf_isokroner_resväg %>%
  group_by(VårdtypGrupp, isokronDistans) %>%
  summarise(geometry = st_union(polygons)) %>%
  mutate(föregåendeGeometry = lag(geometry, 1, order_by = isokronDistans),
         isokronDistansMin = replace_na(lag(isokronDistans, 1, order_by = isokronDistans),0),
         isokronDistansMax = isokronDistans) %>%
  mutate(intervallDistans = paste0(isokronDistansMin, "-", isokronDistansMax)) %>%
  ungroup()%>%
  rowwise() %>%
  mutate(intervallGeometry = st_difference(geometry, föregåendeGeometry)) %>%
  mutate(geometry = intervallGeometry) %>%
  select(VårdtypGrupp, isokronDistansMin, isokronDistansMax, intervallDistans, geometry)

sf_isokroner_restid_intervall <- sf_isokroner_restid %>%
  group_by(VårdtypGrupp, isokronDistans) %>%
  summarise(geometry = st_union(polygons)) %>%
  mutate(föregåendeGeometry = lag(geometry, 1, order_by = isokronDistans),
         isokronDistansMin = replace_na(lag(isokronDistans, 1, order_by = isokronDistans),0),
         isokronDistansMax = isokronDistans) %>%
  mutate(intervallDistans = paste0(isokronDistansMin, "-", isokronDistansMax)) %>%
  ungroup()%>%
  rowwise() %>%
  mutate(intervallGeometry = st_difference(geometry, föregåendeGeometry)) %>%
  mutate(geometry = intervallGeometry) %>%
  select(VårdtypGrupp, isokronDistansMin, isokronDistansMax, intervallDistans, geometry)


p1<-  ggplot(data = sf_isokroner_intervall) + 
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(color = "black", aes(fill = intervallDistans)) + 
  facet_wrap(~intervallDistans + VårdtypGrupp)+
  scale_fill_viridis_d(option = "plasma", direction=-1)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p2<-ggplot(data = sf_isokroner_intervall) + 
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(color = "black", aes(fill = intervallDistans))+ 
  facet_wrap(~VårdtypGrupp)+
  scale_fill_viridis_d(option = "plasma", direction=-1)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  #grid.arrange(p1,p2, ncol = 2)
  
  p<-ggplot(data = sf_isokroner_restid_intervall %>% filter(VårdtypGrupp == "Primärvård" | VårdtypGrupp == "Somatik akut")) + 
    geom_sf(data = sf_kommuner_dalarna)+
    geom_sf( aes(fill = intervallDistans), color = NA, alpha = 0.8)+ 
    facet_wrap(~VårdtypGrupp)+
    scale_fill_viridis_d(option = "plasma", direction=-1)+
    theme_minimal()+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    labs(fill="Resväg (minuter)")

