
sql_Bes�k = "SELECT * FROM Tillg�nglighet_V�rdkonsumtion"

dt_bes�k <- as.data.table(dbGetQuery(connection, sql_Bes�k))
nrow(dt_bes�k)
str(dt_bes�k)
dt_bes�k_folkbokf�rd <-dt_bes�k %>% 
  pivot_wider(id_cols = c("PatientID", "PatientID_AES", "K�n", "�lder", "Kommun"), names_from = c("Akut", "V�rdtyp"), values_from = "AntalBes�k") %>% 
  replace(is.na(.), 0) 

