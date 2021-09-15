
sql_Besök = "SELECT * FROM Tillgänglighet_Vårdkonsumtion"

dt_besök <- as.data.table(dbGetQuery(connection, sql_Besök))
nrow(dt_besök)
str(dt_besök)
dt_besök_folkbokförd <-dt_besök %>% 
  pivot_wider(id_cols = c("PatientID", "PatientID_AES", "Kön", "Ålder", "Kommun"), names_from = c("Akut", "Vårdtyp"), values_from = "AntalBesök") %>% 
  replace(is.na(.), 0) 

