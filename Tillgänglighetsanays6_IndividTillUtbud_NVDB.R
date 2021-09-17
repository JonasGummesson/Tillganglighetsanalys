library(htmlTable)
library(foreach)
library(doParallel)

#install.packages("foreach")
#install.packages("doParallel")
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




dt_individer <- sf_individer %>% 
  #filter(RecordId == 870292) %>%
  as.data.table() %>%
  select(PatientID_AES, individId, xIndivid, yIndivid)
dt_vägnät <- st_as_sf(net_vägnät_nvdb, "nodes") %>% as.data.table() %>% select(nodeId, xNode, yNode)
dt_individer_nvdb <- dt_individer %>% mutate(closestNodeId = as.integer(NA))




# provade cross join, blev 13 miljarder rader, k?r loop ist?llet...
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
        
        dt_individer_nvdb[i, "closestNodeId"] <- closestNodeId

    },
    times=1) ## change to 1 when you want to run this process

mbm


################## test parallel processing ######################


#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(14) #not to overload your computer
registerDoParallel(cl)
dt_individer_test <- dt_individer %>% mutate(closestNodeId = as.integer(NA))
mbm_parallel = microbenchmark(
finalMatrix <- foreach(i=1:5000, .combine=rbind) %dopar% {
  library(tidyr)
  library(dplyr)
  library(data.table)
     xIndivid <- dt_individer[i, "xIndivid"] %>% pull(xIndivid)
  yIndivid <- dt_individer[i, "yIndivid"] %>% pull(yIndivid)

  closestNodeId <-
    dt_vägnät %>%
    mutate(distansTillNod = sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2)) %>%
    mutate(rankDistans = rank(distansTillNod)) %>%
    filter(rankDistans == 1) %>%
    pull(nodeId)
  
  dt_individer_test[i, "closestNodeId"] <- closestNodeId
  dt_individer_test[i,]
},
times=1
)
#stop cluster
stopCluster(cl)

mbm_serial = microbenchmark(
  finalMatrix <- foreach(i=1:5000, .combine=rbind) %do% {
    xIndivid <- dt_individer[i, "xIndivid"] %>% pull(xIndivid)
    yIndivid <- dt_individer[i, "yIndivid"] %>% pull(yIndivid)
    
    closestNodeId <-
      dt_vägnät %>%
      mutate(distansTillNod = sqrt((xNode-xIndivid)^2+(yNode-yIndivid)^2)) %>%
      mutate(rankDistans = rank(distansTillNod)) %>%
      filter(rankDistans == 1) %>%
      pull(nodeId)
    
    dt_individer_test[i, "closestNodeId"] <- closestNodeId
    dt_individer_test[i,]
  },
  times=1
)

########################################################

# save processed dat
#write.table(dt_individer_lm, file = "E:/filer/admgumjon/dt_individer_lm",sep = "\t", row.names = T)

# load last processed data
#dt_individer_lm <- read.table(file = "E:/filer/admgumjon/dt_individer_lm",sep = "\t", header = T)

ggplot()+
  geom_sf(data = st_as_sf(net_dalarna_lm, "nodes"))+
  geom_sf(data = st_as_sf(net_dalarna_lm, "nodes") %>% inner_join(dt, by=c("nodeId"="nodeId")), color = "red")
  

#sf_individer %>% filter(RecordId == 870292)
#dt_individer_lm %>% filter(individId == 67874)

### individer och somatisk akut


sf_individer_nvdb_dist <- sf_individer %>% 
  select(PatientID_AES, Ålder, Kön, Kommun) %>%
  inner_join(dt_individer_nvdb, by=c("PatientID_AES" = "PatientID_AES")) %>%
  inner_join(
    dist_vårdtypgrupp_nodes %>%
      filter(VårdtypGrupp %in% c("Somatik akut", "Primärvård")),
    by=c("closestNodeId" = "nodeId")
  ) %>%
  filter(!is.na(distans) & !is.infinite(distans))%>%
  mutate(distansKm = distans/1000)
  
sf_individer_nvdb_dist %>% 
  as.data.table() %>%
  group_by(Kommun) %>%
  summarise(Distans.medel = as.integer(mean(distans)),
            Distans.median = as.integer(median(distans)),
            Restid_minuter.medel = as.integer(mean(resväg_minuter)),
            Restid_minuter.median = as.integer(median(resväg_minuter)),
            n = n()) %>% 
  #rename_with(~iconv(., "windows-1252", "UTF-8"), everything()) %>%
  as.data.table() %>%
  pivot_longer(cols = -c(Kommun), names_to = "parameter", values_to = "value") %>%
  mutate(group = recode(parameter, 
                        "n" = "Individer", 
                        "Distans.medel" = "Distans (km)", 
                        "Distans.median" = "Distans (km)", 
                        "Restid_minuter.medel" = "Restid (min)", 
                        "Restid_minuter.median" = "Restid (min)"))%>%
  mutate(value = ifelse(parameter %in% c("Distans.medel", "Distans.median"), value/1000, value)) %>%
  mutate(parameter = str_remove(parameter, "Distans.")) %>%
  mutate(parameter = str_remove(parameter, "Restid_minuter.")) %>%
  #rename(Medel = Distans.medel, Median = Distans.median) %>%
  
  
  mutate(group = iconv(group, from="", to="UTF-8", sub="byte")) %>%
  mutate(Kommun = iconv(Kommun, from="",to="UTF-8")) %>% 
  mutate(group = factor(group, levels = c("Distans (km)", "Restid (min)", "Individer"))) %>%
  #distinct(Kommun)
         #group = iconv(group, "UTF-8", "latin1")) %>%
 # mutate(across(where(is_character), function(x) { iconv(x, "UTF-8", "latin1")})) %>%
 # mutate(across(where(is_character), function(x) { iconv(x, "windows-1252", "latin1")})) %>%
  mutate(value = format(round(value,0), digits=9,nsmall=0, decimal.mark=",",big.mark=" ",small.mark=" ", small.interval=3)) %>%
  addHtmlTableStyle(align = "lrrr", 
                  align.header = "lrrr", 
                  css.cell = "padding-left: 1em; padding-right: 1em;",
                  css.header = "padding-left: 1em; padding-right: 1em;") %>%
  tidyHtmlTable(header = parameter,cgroup = group,rnames = Kommun)



sf_individer_nvdb_dist %>% 
  as.data.table() %>%
  group_by(VårdtypGrupp, Kommun) %>%
  summarise(Distans.medel = as.integer(mean(distans)),
            Distans.median = as.integer(median(distans)),
            Restid_minuter.medel = as.integer(mean(resväg_minuter)),
            Restid_minuter.median = as.integer(median(resväg_minuter)),
            n = n()) %>% 
  #rename_with(~iconv(., "windows-1252", "UTF-8"), everything()) %>%
  as.data.table() %>%
  pivot_longer(cols = -c(Kommun, VårdtypGrupp), names_to = "parameter", values_to = "value") %>%
  mutate(group = recode(parameter, 
                        "n" = "Individer", 
                        "Distans.medel" = "Distans (km)", 
                        "Distans.median" = "Distans (km)", 
                        "Restid_minuter.medel" = "Restid (min)", 
                        "Restid_minuter.median" = "Restid (min)"))%>%
  mutate(value = ifelse(parameter %in% c("Distans.medel", "Distans.median"), value/1000, value)) %>%
  mutate(parameter = str_remove(parameter, "Distans.")) %>%
  mutate(parameter = str_remove(parameter, "Restid_minuter.")) %>%
  #rename(Medel = Distans.medel, Median = Distans.median) %>%
  
  
  mutate(group = iconv(group, from="", to="UTF-8", sub="byte")) %>%
  mutate(Kommun = iconv(Kommun, from="",to="UTF-8")) %>% 
  mutate(group = factor(group, levels = c("Distans (km)", "Restid (min)", "Individer"))) %>%
  #distinct(Kommun)
  #group = iconv(group, "UTF-8", "latin1")) %>%
  # mutate(across(where(is_character), function(x) { iconv(x, "UTF-8", "latin1")})) %>%
  # mutate(across(where(is_character), function(x) { iconv(x, "windows-1252", "latin1")})) %>%
  mutate(value = format(round(value,0), digits=9,nsmall=0, decimal.mark=",",big.mark=" ",small.mark=" ", small.interval=3)) %>%
  addHtmlTableStyle(align = "lrrr", 
                    align.header = "lrrr", 
                    css.cell = "padding-left: 1em; padding-right: 1em;",
                    css.header = "padding-left: 1em; padding-right: 1em;") %>%
  tidyHtmlTable(header = parameter,cgroup = group,rnames = Kommun)
  #htmlTable()

?format

ggplot(sf_individer_nvdb_dist) +
  geom_sf(data=sf_vägnät_nvdb, color="grey")+
  geom_sf(aes(color = distansKm))+
  geom_sf(data=sf_kommuner_dalarna, fill = NA, color = "black", linetype="dashed")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_gradientn(colours=magma(20, begin=1, end=0),
                        name="Distans (km)",
                        na.value = "grey100") 


