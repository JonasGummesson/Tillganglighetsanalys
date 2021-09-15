

sql_individer = "
SELECT * FROM Tillgänglighet_Vårdtagare
"

dt_individer <- as.data.table(dbGetQuery(connection, sql_individer))
#nrow(dt_individer)
#dt$Datumperiod = iconv(dt$Datumperiod    , "windows-1252", "latin1")
#str(dt_individer)

sf_individer = dt_individer %>% 
  sf::st_as_sf(coords = c(which(colnames(dt_individer) == "Sweref99Y"), which(colnames(dt_individer) == "Sweref99X"))) %>%
  mutate(xIndivid = st_coordinates(geometry)[,1], yIndivid = st_coordinates(geometry)[,2]) %>%
  mutate(individId = row_number()) %>%
  st_set_crs(3006) 

p <- ggplot()+
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(data = sf_individer, color = "red", inherit.aes = FALSE, alpha = 0.3)
