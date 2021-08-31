library(concaveman)


sf_falun_lm_dist <- st_as_sf(net_falun_lm, "nodes") %>% 
  left_join(dist_lm, by = c("nodeId" = "nodeId"))



# create convex hull

chull <- st_convex_hull(st_union(sf_falun_lm_dist))
ggplot() +
  geom_sf(data = chull) +
  geom_sf(data = sf_falun_lm_dist)

# create concave hull

conc1 <- concaveman(st_as_sf(sf_falun_lm_dist), concavity = 1)
conc2 <- concaveman(st_as_sf(sf_falun_lm_dist), concavity = 2.5)

ggplot() +
  geom_sf(data = conc1, color = "red", fill= NA) +
  geom_sf(data = conc2, color = "blue", fill = NA) +
  geom_sf(data = sf_falun_lm_dist)
