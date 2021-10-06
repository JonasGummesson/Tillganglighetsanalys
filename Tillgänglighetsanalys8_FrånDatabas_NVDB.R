library(odbc)
library(DBI)
library(sfheaders)
library(data.table)
library(tidyverse)

con_Sandbox <- DBI::dbConnect(odbc::odbc(),
                              Driver = "SQL Server",
                              Server = "WFALMITSR036",
                              Database = "Sandbox_Jonas",
                              Trusted_Connection = "True", 
                              encoding = "latin1")


dt_nodes <- as.data.table(dbGetQuery(con_Sandbox, "SELECT * FROM Vagnat_NVDB_nodes"))
dt_edges <- as.data.table(dbGetQuery(con_Sandbox, "SELECT * FROM Vagnat_NVDB_edges"))
dt_distans_utbud_nodes <- as.data.table(dbGetQuery(con_Sandbox, "SELECT * FROM [dbo].[Vagnat_NVDB_Distans_Utbud_nodes]"))
dt_distans_utbud_edges <- as.data.table(dbGetQuery(con_Sandbox, "SELECT * FROM [dbo].[Vagnat_NVDB_Distans_Utbud_edges]"))


sf <- st_sf()

dt <-
dt_distans_utbud_edges %>%
  filter(VårdtypGrupp == "Primärvård") %>%
  select(from, to, sf_edge_id, restid_minuter.nodMax) %>%
  inner_join(dt_edges %>% select(sf_edge_id, geometry), on=c("sf_edge_id"="sf_edge_id")) %>%

  ?str_replace
dt <-  dt_edges %>% head(10) %>% 
  mutate(geometry = str_replace(geometry, "c", "LINESTRING ")) %>% 
  select(sf_edge_id, geometry)
  
dt
  sf <- dt_as_sf( dt )
library(sfheaders)
  sfheaders::

?sfc_linestring
sfc_linestring( obj = dt_edges %>% head(10), linestring_id = which(colnames(dt)=="geometry") )

sf::st_crs( sf ) <- 3006

sf

  
  
dt %>%  
  head(1) %>%
  mutate(geometry = str_replace(geometry, "c", "LINESTRING ")) %>%
  #mutate(geometry = st_geometrycollection(geometry))
  sf::st_as_sf(sf_column_name = "geometry") %>%
  st_set_crs(3006) 
  
  
  
  
  
  st_sf(
    dt %>% head(1) %>% mutate(geometry = str_replace(geometry, "c", "LINESTRING ")),
    stringsAsFactors = sf_stringsAsFactors(),
    crs = 3006,
    sf_column_name = "geometry",
    check_ring_dir = FALSE,
    sfc_last = TRUE
  )
  

sf_vägnät_nvdb

sf_noder = dt_noder %>% 
  
  mutate(xNode = st_coordinates(geometry)[,1], yNode = st_coordinates(geometry)[,2]) %>%
  st_set_crs(3006) 

p <- ggplot()+
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(data = sf_noder, color = "red", inherit.aes = FALSE)
p



sf_vägnät_lm %>% head(1)%>% pull(geometry 
