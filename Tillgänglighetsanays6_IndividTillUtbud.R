library(htmlTable)

#################### test performance ###########################

#dt_individer <- sf_individer %>% as.data.table() %>% select(individId, xIndivid, yIndivid)
#dt_v?gn?t <- v?gn?t_noder_lm %>% as.data.table()
#sf_v?gn?t <- st_as_sf(net_dalarna_lm, "nodes") 

#dt1 <- dt_individer %>% head(1000)
#dt2 <- dt_v?gn?t #%>% head(1000)

#mbm = microbenchmark(
  #{
    #dt <- CJ(individId = dt1$individId,nodeId = dt2$nodeId)
    #dt <- dt[dt1, on="individId", nomatch=0]
    #dt <- dt[dt2, on="nodeId", nomatch=0]
    #dt[, distans := sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2)]
    #dt[, rank := frank(distans),by = individId]
    #dt <- dt[rank == 1]
  #},
  #{
    #dt <- dt1 %>%
      #full_join(dt2 , by=character()) %>%
      #mutate(distansTillNod = sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2))%>%
      #group_by(individId)%>%
      #mutate(rankDistans = rank(distansTillNod)) %>%
      #filter(rankDistans == 1)
  #},
  #{
    #sf_individer %>% head(1000) %>%
      #rowwise() %>%
      #mutate(np =  sf::st_nearest_feature( geometry, sf_v?gn?t))
  #},
  #times = 1
#)
#mbm

# data.table          --> 10 sekunder
# dplyr               --> 15 sekunder
# st_nearest_feature  --> 143 sekunder


#dt <-  sf_individer %>% 
  #filter(RecordId == 870292) %>%
  #as.data.table() %>% 
  #select(individId, xIndivid, yIndivid) %>%
  #full_join(v?gn?t_noder_lm %>% as.data.table() , by=character()) %>%
  #mutate(distansTillNod = sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2))%>%
  #group_by(individId)%>%
  #mutate(rankDistans = rank(distansTillNod)) %>%
  #filter(rankDistans == 1)


##run with data.table

dt_individer <- sf_individer %>% 
  #filter(RecordId == 870292) %>%
  as.data.table() %>%
  select(individId, xIndivid, yIndivid)
dt_vägnät <- st_as_sf(net_v?gn?t_lm, "nodes") %>% as.data.table() %>% select(nodeId, xNode, yNode)
dt_individer_lm <- dt_individer %>% mutate(closestNodeId = as.integer(NA))


# provade cross join, blev 13 miljarder rader, k?r loop ist?llet...
mbm = microbenchmark(
  # tar 3600 s = 1 timme
    for(i in  1:nrow(dt_individer)) 
    {
        xIndivid <- dt_individer[i, "xIndivid"] %>% pull(xIndivid)
        yIndivid <- dt_individer[i, "yIndivid"] %>% pull(yIndivid)
  
        closestNodeId <-
        dt_v?gn?t %>%
          mutate(distansTillNod = sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2)) %>%
          mutate(rankDistans = rank(distansTillNod)) %>%
          filter(rankDistans == 1) %>%
          pull(nodeId)
        
        dt_individer_lm[i, "closestNodeId"] <- closestNodeId
    },
    times=0) ## change to 1 when you want to run this process

mbm

# save processed dat
#write.table(dt_individer_lm, file = "E:/filer/admgumjon/dt_individer_lm",sep = "\t", row.names = T)

# load last processed data
#dt_individer_lm <- read.table(file = "E:/filer/admgumjon/dt_individer_lm",sep = "\t", header = T)




ggplot()+
  geom_sf(data = st_as_sf(net_dalarna_lm, "nodes"))+
  geom_sf(data = st_as_sf(net_dalarna_lm, "nodes") %>% inner_join(dt, by=c("nodeId"="nodeId")), color = "red")
  

sf_individer %>% filter(RecordId == 870292)
dt_individer_lm %>% filter(individId == 67874)

### individer och somatisk akut


sf_individer_lm_dist <- sf_individer %>% 
  select(RecordId, ?lder, K?n, Kommun, individId) %>%
  inner_join(dt_individer_lm, by=c("individId" = "individId")) %>%
  inner_join(
    dist_v?rdtypgrupp_nodes %>%
      filter(V?rdtypGrupp == "Somatik akut"),
    by=c("closestNodeId" = "nodeId")
  ) %>%
  filter(!is.na(distance) & !is.infinite(distance))
  
sf_individer_lm_dist %>% 
  as.data.table() %>%
  group_by(Kommun) %>%
  summarise(Distans.medel = as.integer(mean(distance)),
            Distans.median = as.integer(median(distance)),
            n = n()) %>% 
  #rename_with(~iconv(., "windows-1252", "UTF-8"), everything()) %>%
  as.data.table() %>%
  rename(Medel = Distans.medel, Median = Distans.median) %>%
  pivot_longer(cols = Medel:n, names_to = "parameter", values_to = "value") %>%
  mutate(group = ifelse(parameter == "n", "Folkbokförda", "Distans")) %>%
  mutate(group = iconv(group, from="", to="UTF-8", sub="byte")) %>%
  mutate(Kommun = iconv(Kommun, from="",to="UTF-8")) %>% 
  #distinct(Kommun)
         #group = iconv(group, "UTF-8", "latin1")) %>%
 # mutate(across(where(is_character), function(x) { iconv(x, "UTF-8", "latin1")})) %>%
 # mutate(across(where(is_character), function(x) { iconv(x, "windows-1252", "latin1")})) %>%
  mutate(across(where(is_integer), function(x) { format(x, digits=9, decimal.mark=",",big.mark=" ",small.mark=".", small.interval=3)})) %>%
  addHtmlTableStyle(align = "lrrr", 
                  align.header = "lrrr", 
                  css.cell = "padding-left: 1em; padding-right: 1em;",
                  css.header = "padding-left: 1em; padding-right: 1em;") %>%
  #tidyHtmlTable(header = parameter,cgroup = group,rnames = Kommun)
  htmlTable()



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




  



