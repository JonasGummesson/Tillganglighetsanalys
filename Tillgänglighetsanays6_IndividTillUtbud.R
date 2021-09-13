
dt_individer <- sf_individer %>% as.data.table() %>% select(individId, xIndivid, yIndivid)
dt_vägnät <- vägnät_noder_lm %>% as.data.table()
sf_vägnät <- st_as_sf(net_dalarna_lm, "nodes") 



dt1 <- dt_individer %>% head(1000)
dt2 <- dt_vägnät #%>% head(1000)

mbm = microbenchmark(
  {
    dt <- CJ(individId = dt1$individId,nodeId = dt2$nodeId)
    dt <- dt[dt1, on="individId", nomatch=0]
    dt <- dt[dt2, on="nodeId", nomatch=0]
    #dt$distans = sqrt((dt$xNode-dt$xIndivid)^2+(dt$yNode-dt$yIndivid)^2)
    #dt$rank <- dt[,rank(distans),by="nodeId"]
    dt[, distans := sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2)]
    dt[, rank := frank(distans),by = individId]
    dt <- dt[rank == 1]
  },
  {
    dt <- dt1 %>%
      full_join(dt2 , by=character()) %>%
      mutate(distansTillNod = sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2))%>%
      group_by(individId)%>%
      mutate(rankDistans = rank(distansTillNod)) %>%
      filter(rankDistans == 1)
  },
  {
    sf_individer %>% head(1000) %>%
      rowwise() %>%
      mutate(np =  sf::st_nearest_feature( geometry, sf_vägnät))
  },
  times = 1
)
mbm

# data.table          --> 10 sekunder
# dplyr               --> 15 sekunder
# st_nearest_feature  --> 143 sekunder


#dt <-  sf_individer %>% 
  #filter(RecordId == 870292) %>%
  #as.data.table() %>% 
  #select(individId, xIndivid, yIndivid) %>%
  #full_join(vägnät_noder_lm %>% as.data.table() , by=character()) %>%
  #mutate(distansTillNod = sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2))%>%
  #group_by(individId)%>%
  #mutate(rankDistans = rank(distansTillNod)) %>%
  #filter(rankDistans == 1)


##run with data.table

dt_individer <- sf_individer %>% 
  #filter(RecordId == 870292) %>%
  as.data.table() %>%
  select(individId, xIndivid, yIndivid)
dt_vägnät <- st_as_sf(net_vägnät_lm, "nodes") %>% as.data.table() %>% select(nodeId, xNode, yNode)
dt_individer_lm <- dt_individer %>% mutate(closestNodeId = as.integer(NA))


# provade cross join, blev 13 miljarder rader, kör loop istället...
mbm = microbenchmark(
  # tar 3600 s = 1 timme
    for(i in  1:nrow(dt_individer)) 
    {
        xIndivid <- dt_individer[i, "xIndivid"] %>% pull(xIndivid)
        yIndivid <- dt_individer[i, "yIndivid"] %>% pull(yIndivid)
  
        closestNodeId <-
        dt_vägnät %>%
          mutate(distansTillNod = sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2)) %>%
          mutate(rankDistans = rank(distansTillNod)) %>%
          filter(rankDistans == 1) %>%
          pull(nodeId)
        
        dt_individer_lm[i, "closestNodeId"] <- closestNodeId
    },
    times=1)

mbm

write.table(dt_individer_lm, file = "E:/filer/admgumjon/dt_individer_lm",sep = "\t", row.names = T)

#test <- read.table(file = "E:/filer/admgumjon/dt_individer_lm",sep = "\t", header = T)




ggplot()+
  geom_sf(data = st_as_sf(net_dalarna_lm, "nodes"))+
  geom_sf(data = st_as_sf(net_dalarna_lm, "nodes") %>% inner_join(dt, by=c("nodeId"="nodeId")), color = "red")
  

sf_individer %>% filter(RecordId == 870292)
dt_individer_lm %>% filter(individId == 67874)

### individer och somatisk akut


sf_individer_lm_dist <- sf_individer %>% 
  select(RecordId, Ålder, Kön, Kommun, individId) %>%
  inner_join(dt_individer_lm, by=c("individId" = "individId")) %>%
  inner_join(
    dist_vårdtypgrupp_nodes %>%
      filter(VårdtypGrupp == "Somatik akut"),
    by=c("closestNodeId" = "nodeId")
  ) %>%
  filter(!is.na(distance) & !is.infinite(distance))
  

sf_individer_lm_dist %>% 
  as.data.table() %>%
  group_by(Kommun) %>%
  summarise(Distans.medel = as.integer(mean(distance)),
            Distans.median = as.integer(median(distance)),
            n = n()) %>% 
  mutate(across(where(is_character), function(x) { iconv(x, "windows-1252", "UTF-8")})) %>%
  #rowwise() %>%
  #mutate(across(where(is_integer), function(x) { sprintf("%### ###.#d", as.integer(x)) })) %>%
  mutate(across(where(is_integer), function(x) { format(x, digits=9, decimal.mark=",",big.mark=" ",small.mark=".", small.interval=3)}))
  #mutate(across(where(is_integer), function(x) { str_replace(x, "(\\d{1,3})(\\d{3})", "\\1 \\2")})) %>%
  #rename_with(~iconv(., "windows-1252", "UTF-8"), everything()) %>%
  as.data.table() %>%
  addHtmlTableStyle(align = "lrrr") %>%
  htmlTable()
?htmlTable

str_replace("11223", "(\\d{1,3})(\\d{3})", "\\1 \\2")


str_replace("1122355566778", "(\\d{1,3})(\\d.)", "\\1 \\2")

ggplot(sf_individer_lm_dist) +
  geom_sf(aes(color = distance))+
  geom_sf(data=sf_kommuner_dalarna, fill = NA, color = "grey", linetype="dashed")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (km)",
                        na.value = "grey100") 




  



