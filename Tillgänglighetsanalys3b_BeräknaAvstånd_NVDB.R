library(microbenchmark)

library(DBI)

#nodes_utbud <- unique(sf_utbud_nvdb %>% 
#  filter(tolower(V�rdtypGrupp) %like% "Somatik akut") %>%
#  pull(nodeId))

#?distances
#sf_utbud_nvdb %>% pull(utbudNamn)
############################### Ber�kna f�r hela Dalarna ###################################################
dist_dalarna_nvdb <- NULL
mbm_dist_dalarna_nvdb = microbenchmark(
  dist_dalarna_nvdb <- sf_utbud_nvdb %>%
    #filter(V�rdtypGrupp == "Somatik akut" | V�rdtypGrupp == "Prim�rv�rd")  %>%
    rename("utbudNodeId" = "nodeId", "utbudX" = "x", utbudY = "y") %>%
    as.data.table() %>%
    select(utbudNodeId, utbudX, utbudY) %>%
    group_by(utbudNodeId, utbudX, utbudY) %>%
    summarise() %>%
    mutate(net = map(.x = utbudNodeId, .f = function(utbudNodeId){
      resv�gOchRestid <- net_v�gn�t_nvdb %>%
        activate("nodes") %>%
        left_join(
          t(distances(graph = net_v�gn�t_nvdb_str�cka, v = utbudNodeId, mode = "all")) %>% # ber�kna resv�g
            as_tibble() %>%
            rename("distans" = "V1") %>%
            mutate("distansKm" = distans / 1000) %>%
            rownames_to_column("nodeId") %>%
            mutate(nodeId = as.integer(nodeId)) %>%
            select(nodeId, distans, distansKm),
          by=c("nodeId" = "nodeId")
        )%>%
        left_join(
          t(distances(graph = net_v�gn�t_nvdb, v = utbudNodeId, mode = "all")) %>% # ber�kna restid
            as_tibble() %>%
            rename("restid_minuter" = "V1") %>%
            rownames_to_column("nodeId") %>%
            mutate(nodeId = as.integer(nodeId)) %>%
            select(nodeId, restid_minuter),
          by=c("nodeId" = "nodeId")
        )
      resv�gOchRestid %>%
        activate("edges") %>%
        left_join(
          st_as_sf(resv�gOchRestid, "edges") %>% 
            as.data.table() %>%
            select(from, to, sf_edge_id)  %>%
            inner_join(st_as_sf(resv�gOchRestid, "nodes") %>% as.data.table() %>% select(nodeId, distans) %>% rename(distans.Nod1 = distans), by=c("from"="nodeId")) %>%
            inner_join(st_as_sf(resv�gOchRestid, "nodes") %>% as.data.table() %>% select(nodeId, distans) %>% rename(distans.Nod2 = distans), by=c("to"="nodeId")) %>%
            rowwise() %>%
            mutate(distans.NodMax = max(distans.Nod1, distans.Nod2)) %>%
            mutate(distans.NodMaxKm = distans.NodMax / 1000) %>%
            select(sf_edge_id, distans.NodMax, distans.NodMaxKm)
          ,by=c("sf_edge_id"="sf_edge_id")) %>%
        left_join(
          st_as_sf(resv�gOchRestid, "edges") %>% 
            as.data.table() %>%
            select(from, to, sf_edge_id)  %>%
            inner_join(st_as_sf(resv�gOchRestid, "nodes") %>% as.data.table() %>% select(nodeId, restid_minuter) %>% rename(restid_minuter.Nod1 = restid_minuter), by=c("from"="nodeId")) %>%
            inner_join(st_as_sf(resv�gOchRestid, "nodes") %>% as.data.table() %>% select(nodeId, restid_minuter) %>% rename(restid_minuter.Nod2 = restid_minuter), by=c("to"="nodeId")) %>%
            rowwise() %>%
            mutate(restid_minuter.NodMax = max(restid_minuter.Nod1, restid_minuter.Nod2)) %>%
            select(sf_edge_id, restid_minuter.NodMax)
          ,by=c("sf_edge_id"="sf_edge_id"))
      })) %>%
    left_join(sf_utbud_nvdb %>% select(Popul�rnamn, V�rdtypGrupp, utbudNamn, nodeId) #%>% filter(V�rdtypGrupp == "Somatik akut" | V�rdtypGrupp == "Prim�rv�rd")
                , by=c("utbudNodeId" = "nodeId")),
  times = 1)
#mbm_dist_dalarna_nvdb
  
  
nrow(dist_dalarna_nvdb)
#dbWriteTable(con_Sandbox, name = Id(schema = "dbo", table = "V�gn�t_NVDB_Avst�ndTillUtbud"), value = dist_dalarna_nvdb, overwrite=TRUE)


# plotta utbudspunkt i Dalarna

net <- (dist_dalarna_nvdb %>% filter(tolower(Popul�rnamn) %like% "falu lasarett") %>% pull(net))[[1]]

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

# plocka ut �ndpunkterna fr�n varje b�ge
#sf_v�gn�t_nvdb_points <- rbind(
  #st_cast(st_line_sample(sf_v�gn�t_nvdb, sample = 1), "POINT") %>% as.data.frame() ,
  #st_cast(st_line_sample(sf_v�gn�t_nvdb, sample = 0), "POINT") %>% as.data.frame()
#) %>% st_as_sf()

#hitta ensamma �ndpunkter
#sf_v�gn�t_nvdb_points_single <- sf_v�gn�t_nvdb_points %>% 
  #group_by(st_coordinates(geometry)) %>% 
  #summarise(n = n()) %>% arrange(desc(n)) %>%
  #filter(n == 1) %>%
  #select(geometry)


# klipp av b�gar i n�rheten ensamma �ndpunkter (TEST!!)
#buffer_points <- st_combine(st_buffer(sf_v�gn�t_nvdb_points_single, 5))
#sf_v�gn�t_nvdb_parts = st_collection_extract(lwgeom::st_split(sf_v�gn�t_nvdb, buffer_points),"LINESTRING")



