

sql_individer = "
SELECT 
  Dalfolke.RecordId,
	Ålder,
	adress.Kommun,
	Kön,
	adress.Sweref99X,
	adress.Sweref99Y
FROM staging.Dalfolke
INNER JOIN staging.DalfolkeSourceSnapshot snap on snap.RecordID = Dalfolke.RecordId
INNER JOIN staging.Bridge_Dalfolke_AdressKommunUnika bridge on bridge.RecordId = Dalfolke.RecordId
INNER JOIN staging.Adress_Kommun_Unika adress on adress.Adress_Kommun_Unika_ID = bridge.Adress_Kommun_Unika_ID
WHERE AvregistreringsorsakKod IS NULL
AND snap.År = 2021
AND snap.Månad = 1
"

dt_individer <- as.data.table(dbGetQuery(connection, sql_individer))
#nrow(dt_individer)
#dt$Datumperiod = iconv(dt$Datumperiod    , "windows-1252", "latin1")
#str(dt_individer)

sf_individer = dt_individer %>% sf::st_as_sf(coords = c(which(colnames(dt_individer) == "Sweref99Y"), which(colnames(dt_individer) == "Sweref99X")))
st_crs(sf_individer) <- st_crs(3006)
