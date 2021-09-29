library(microbenchmark)

library(DBI)

#nodes_utbud <- unique(sf_utbud_nvdb %>% 
#  filter(tolower(VårdtypGrupp) %like% "Somatik akut") %>%
#  pull(nodeId))

#?distances
#sf_utbud_nvdb %>% pull(utbudNamn)
############################### Beräkna för hela Dalarna ###################################################
dist_dalarna_nvdb <- NULL
mbm_dist_dalarna_nvdb = microbenchmark(
  dist_dalarna_nvdb <- sf_utbud_nvdb %>%
    #filter(VårdtypGrupp == "Somatik akut" | VårdtypGrupp == "Primärvård")  %>%
    rename("utbudNodeId" = "nodeId", "utbudX" = "x", utbudY = "y") %>%
    as.data.table() %>%
    select(utbudNodeId, utbudX, utbudY) %>%
    group_by(utbudNodeId, utbudX, utbudY) %>%
    summarise() %>%
    mutate(net = map(.x = utbudNodeId, .f = function(utbudNodeId){
      resvägOchRestid <- net_vägnät_nvdb %>%
        activate("nodes") %>%
        left_join(
          t(distances(graph = net_vägnät_nvdb_sträcka, v = utbudNodeId, mode = "all")) %>% # beräkna resväg
            as_tibble() %>%
            rename("distans" = "V1") %>%
            mutate("distansKm" = distans / 1000) %>%
            rownames_to_column("nodeId") %>%
            mutate(nodeId = as.integer(nodeId)) %>%
            select(nodeId, distans, distansKm),
          by=c("nodeId" = "nodeId")
        )%>%
        left_join(
          t(distances(graph = net_vägnät_nvdb, v = utbudNodeId, mode = "all")) %>% # beräkna restid
            as_tibble() %>%
            rename("restid_minuter" = "V1") %>%
            rownames_to_column("nodeId") %>%
            mutate(nodeId = as.integer(nodeId)) %>%
            select(nodeId, restid_minuter),
          by=c("nodeId" = "nodeId")
        )
      resvägOchRestid %>%
        activate("edges") %>%
        left_join(
          st_as_sf(resvägOchRestid, "edges") %>% 
            as.data.table() %>%
            select(from, to, sf_edge_id)  %>%
            inner_join(st_as_sf(resvägOchRestid, "nodes") %>% as.data.table() %>% select(nodeId, distans) %>% rename(distans.Nod1 = distans), by=c("from"="nodeId")) %>%
            inner_join(st_as_sf(resvägOchRestid, "nodes") %>% as.data.table() %>% select(nodeId, distans) %>% rename(distans.Nod2 = distans), by=c("to"="nodeId")) %>%
            rowwise() %>%
            mutate(distans.NodMax = max(distans.Nod1, distans.Nod2)) %>%
            mutate(distans.NodMaxKm = distans.NodMax / 1000) %>%
            select(sf_edge_id, distans.NodMax, distans.NodMaxKm)
          ,by=c("sf_edge_id"="sf_edge_id")) %>%
        left_join(
          st_as_sf(resvägOchRestid, "edges") %>% 
            as.data.table() %>%
            select(from, to, sf_edge_id)  %>%
            inner_join(st_as_sf(resvägOchRestid, "nodes") %>% as.data.table() %>% select(nodeId, restid_minuter) %>% rename(restid_minuter.Nod1 = restid_minuter), by=c("from"="nodeId")) %>%
            inner_join(st_as_sf(resvägOchRestid, "nodes") %>% as.data.table() %>% select(nodeId, restid_minuter) %>% rename(restid_minuter.Nod2 = restid_minuter), by=c("to"="nodeId")) %>%
            rowwise() %>%
            mutate(restid_minuter.NodMax = max(restid_minuter.Nod1, restid_minuter.Nod2)) %>%
            select(sf_edge_id, restid_minuter.NodMax)
          ,by=c("sf_edge_id"="sf_edge_id"))
      })) %>%
    left_join(sf_utbud_nvdb %>% select(Populärnamn, VårdtypGrupp, utbudNamn, nodeId) #%>% filter(VårdtypGrupp == "Somatik akut" | VårdtypGrupp == "Primärvård")
                , by=c("utbudNodeId" = "nodeId")),
  times = 1)
#mbm_dist_dalarna_nvdb
  
  
nrow(dist_dalarna_nvdb)
#dbWriteTable(con_Sandbox, name = Id(schema = "dbo", table = "Vägnät_NVDB_AvståndTillUtbud"), value = dist_dalarna_nvdb, overwrite=TRUE)


# plotta utbudspunkt i Dalarna

net <- (dist_dalarna_nvdb %>% filter(tolower(Populärnamn) %like% "falu lasarett") %>% pull(net))[[1]]

p1 <- ggplot()+
  geom_sf(data = sf_kommuner_dalarna, color = "grey", linetype="dashed")+
  geom_sf(data = st_as_sf(net, "edges"), aes(color=distans.NodMax))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),name="Distans (km)",na.value = "green") 

p2 <- ggplot()+
  geom_sf(data = sf_kommuner_dalarna, color = "grey", linetype="dashed")+
  geom_sf(data = st_as_sf(net, "edges"), aes(color=restid_minuter.NodMax))+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),name="Distans (km)",na.value = "green")


grid.arrange(p1,p2)
############################################################################################################
############################################################################################################

# plocka ut ändpunkterna från varje båge
#sf_vägnät_nvdb_points <- rbind(
  #st_cast(st_line_sample(sf_vägnät_nvdb, sample = 1), "POINT") %>% as.data.frame() ,
  #st_cast(st_line_sample(sf_vägnät_nvdb, sample = 0), "POINT") %>% as.data.frame()
#) %>% st_as_sf()

#hitta ensamma ändpunkter
#sf_vägnät_nvdb_points_single <- sf_vägnät_nvdb_points %>% 
  #group_by(st_coordinates(geometry)) %>% 
  #summarise(n = n()) %>% arrange(desc(n)) %>%
  #filter(n == 1) %>%
  #select(geometry)


# klipp av bågar i närheten ensamma ändpunkter (TEST!!)
#buffer_points <- st_combine(st_buffer(sf_vägnät_nvdb_points_single, 5))
#sf_vägnät_nvdb_parts = st_collection_extract(lwgeom::st_split(sf_vägnät_nvdb, buffer_points),"LINESTRING")



