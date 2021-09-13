library(concaveman)

# examples

utbudspunktExempel <- (dist_dalarna_lm %>% head(1) %>% pull(net))[[1]]

# create convex hull

chull <- st_convex_hull(st_union(st_as_sf(utbudspunktExempel, "nodes")))
p1<-ggplot() +
  geom_sf(data = st_as_sf(net_dalarna_lm))+
  geom_sf(data = chull, color = "green", fill =NA, size=2) +
  labs(title="Convex hull")

# create concave hull

conc1 <- concaveman(st_as_sf(utbudspunktExempel), concavity = 1)
conc2 <- concaveman(st_as_sf(utbudspunktExempel), concavity = 2.5)

p2<-ggplot() +
  geom_sf(data = conc1, color = "red", fill= NA, size=2) +
  geom_sf(data = conc2, color = "blue", fill = NA, size=2) +
  geom_sf(data = st_as_sf(net_dalarna_lm))+
  labs(title="Alpha shape, concavity 1 & 2,5")

#grid.arrange(p1,p2,ncol=2)
## create isochrones

################################# beräkna isokroner ############################

sf_isokroner <- dist_dalarna_lm %>% 
  #filter(VårdtypGrupp == "Somatik akut" | VårdtypGrupp == "Primärvård") %>% 
 # filter(Populärnamn == "Avesta lasarett") %>% 
  select(VårdtypGrupp, utbudNamn, net) %>%
  full_join(tibble(isokronDistans = c(10000,20000,30000,50000)), by=character()) %>%
  mutate(sf_alphaShape = map2(.x = isokronDistans, .y = net, .f = function(x,y) { 
      #st_buffer(
        concaveman( st_as_sf(y, "nodes") %>% filter(distance < x), concavity = 1)
       # ,100)
    }))%>%
  unnest(cols = "sf_alphaShape") %>%
  st_as_sf() %>%
  select(utbudNodeId, VårdtypGrupp, utbudNamn, isokronDistans, polygons) %>%
  st_buffer(200) %>% # lägg till 200 m buffertzon kring varje väg
  st_simplify(dTolerance = 500) %>%
  arrange(desc(isokronDistans)) %>%
  mutate(isokronDistansFct=factor(isokronDistans, levels = unique(isokronDistans)))

ggplot(data = sf_isokroner) + 
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(color = "black", aes(fill = isokronDistansFct)) + 
  facet_wrap(~VårdtypGrupp)+
  scale_fill_viridis_d(option = "plasma", direction=1)+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# skapa intervall-shapes
sf_isokroner_intervall <- sf_isokroner %>%
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

  grid.arrange(p1,p2, ncol = 2)

