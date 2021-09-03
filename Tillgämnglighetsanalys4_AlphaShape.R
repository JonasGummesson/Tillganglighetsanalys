library(concaveman)

# examples

utbudspunktExempel <- (dist_dalarna_lm %>% head(1) %>% pull(net))[[1]]

# create convex hull

chull <- st_convex_hull(st_union(st_as_sf(utbudspunktExempel, "nodes") %>% filter(distance<=50000)))
ggplot() +
  geom_sf(data = sf_dalarna_lm_dist)+
  geom_sf(data = chull, color = "red", fill =NA) 

# create concave hull

conc1 <- concaveman(st_as_sf(utbudspunktExempel), concavity = 1)
conc2 <- concaveman(st_as_sf(utbudspunktExempel), concavity = 2.5)

ggplot() +
  geom_sf(data = conc1, color = "red", fill= NA) +
  geom_sf(data = conc2, color = "blue", fill = NA) +
  geom_sf(data = st_as_sf(net_dalarna_lm))


#?concaveman
sf_isokroner <- dist_dalarna_lm %>% 
  filter(VårdtypGrupp == "Somatik akut") %>% 
 # filter(Populärnamn == "Avesta lasarett") %>% 
  select(VårdtypGrupp, utbudNamn, net) %>%
  full_join(tibble(isokronDistans = c(10000,20000,30000,50000)), by=character()) %>%
  mutate(sf_alphaShape = map2(.x = isokronDistans, .y = net, .f = function(x,y) { 
      st_buffer(
        concaveman( st_as_sf(y) %>% filter(distance < x), concavity = 1)
        ,100)
    }))%>%
  unnest(cols = "sf_alphaShape") %>%
  st_as_sf()

?st_union
sf_test <- sf_isokroner %>%
  group_by(VårdtypGrupp, isokronDistans) %>%
  summarise(geometry = st_union(polygons)) %>%
  mutate(föregåendeGeometry = lag(geometry, 1, order_by = isokronDistans)) %>%
  mutate(intervallDistans = paste0(lag(isokronDistans, 1, order_by = isokronDistans), "-", isokronDistans)) %>%
  #mutate(test = ifelse(!is.na(st_dimension(föregåendePolygon)), 1, 0))
  ungroup()%>%
  rowwise() %>%
  mutate(diffPolygons = ifelse(!is.na(st_dimension(föregåendeGeometry)), st_union(st_difference(föregåendeGeometry))[1], NA)) %>%
  filter(isokronDistans == 30000) %>% pull(diffPolygons)

  ggplot() + geom_sf(data = sf_test, aes(geometry = diffPolygons), color = "green", fill = "white")
  
  
sf1 <- sf_isokroner %>% filter(isokronDistans == 30000) %>% group_by(isokronDistans) %>%  summarise(geometry = st_union(polygons))
sf2 <- sf_isokroner %>% filter(isokronDistans == 50000)%>% group_by(isokronDistans) %>%  summarise(geometry = st_union(polygons))
sf3 <- st_difference(sf2, sf1)

st_geometry(sf1) = "geometry"
st_geometry(sf2) = "geometry"
st_geometry(sf3) = "geometry"


grid.arrange(
ggplot() + geom_sf(data = sf1, aes(geometry = geometry), color = "red", fill = "white") ,
ggplot() + geom_sf(data = sf2, aes(geometry = geometry), color = "blue", fill = "white"),
ggplot() + geom_sf(data = sf3, color = "green", fill = "white")
)
  
  
ggplot(.) +
  geom_sf(data = sf_kommuner_dalarna) +
  #geom_sf(aes(fill = intervallDistans, geometry=polygons), alpha = 1)+
  geom_sf(aes(fill = intervallDistans, geometry=diffPolygons), alpha = 1)+
  facet_wrap(~intervallDistans)+
  scale_fill_viridis_d(option = "plasma")



?replace
sf_isokroner %>% 
  select(VårdtypGrupp, isokronDistans, polygons) %>%
  group_by(VårdtypGrupp, isokronDistans) %>%
  summarise(polygons = st_union(polygons)) %>%
  arrange(desc(isokronDistans)) %>%
  mutate(isokronDistans=factor(isokronDistans, levels = unique(isokronDistans)))  %>%
  #mutate(isokronDistans = as.double(as.character(isokronDistans))) %>%
ggplot(.) +
  geom_sf(data = sf_kommuner_dalarna) +
  geom_sf(aes(fill = isokronDistans), alpha = 1)+
  scale_fill_viridis_d(option = "plasma")

