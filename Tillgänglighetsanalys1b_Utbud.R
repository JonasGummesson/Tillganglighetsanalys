library(odbc)

sql_utbudspunkt = "
SELECT 
	plats.*
	,bridge.UtbudspunktID 
	,punkt.Populärnamn
    ,[Sweref99X]
      ,[Sweref99Y]
      ,[Latitude]
      ,[Longitude]
FROM dbo.UtbudKommunPlats plats
LEFT JOIN dbo.UtbudPunkterPerPlats bridge on bridge.UtbudsplatsID = plats.UtbudsplatsID
LEFT JOIN dbo.UtbudPunkt punkt on punkt.UtbudspunktID = bridge.UtbudspunktID
WHERE Sweref99X IS NOT NULL
"


connection <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "analys.ltdalarna.se", Database = "Analys", Trusted_Connection = "True", Encoding = "windows-1252")
dt_utbud <- as.data.table(dbGetQuery(connection, sql_utbudspunkt))

# pivotera och städa dataset
dt_utbud_long <- 
  dt_utbud %>% 
  pivot_longer(cols = SomatikSluten:Privat, names_to = "Vårdtyp", values_to="VårdtypFinns") %>%
  filter(VårdtypFinns == "X") %>%
  select(-VårdtypFinns) %>%
  mutate(
    Regi = ifelse(Vårdtyp == "Privat", "Privat", "Offentlig"),
    Vårdtyp = recode(Vårdtyp, 
                     "Privat" = "Privat vårdcentral",
                     "SomatikSluten" = "Somatik sluten",
                     "SomatikAkut" = "Somatik akut",
                     "SomatikNärakut" = "Somatik närakut",
                     "PsykiatriVixenSluten" = "Psykiatri vuxen sluten",
                     "PsykiatriVuxenÖppen" = "Psykiatri vuxen öppen",
                     "PsykiatriBarnOchUngdomSluten" = "Psykiatri barn och ungdom sluten",
                     "PsykiatriBarnOchUngdomÖppen" = "Psykiatri barn och ungdom öppen",
                     "PsykiatriAkut" = "Psykiatri akut",
                     "HabiliteringVixen" = "Habilitering vuxen",
                     "HabiliteringBarn" = "Habilitering barn",
                     "Ambulansstation" = "Ambulansstation"),
    VårdtypGrupp = recode(Vårdtyp, 
                          "Privat" = "Privat vårdcentral",
                          "Somatik sluten" = "Somatik sluten",
                          "Somatik akut" = "Somatik akut",
                          "Somatik närakut" = "Somatik akut",
                          "Psykiatri vuxen sluten" = "Psykiatri vuxen sluten",
                          "Psykiatri vuxen öppen" = "Psykiatri vuxen öppen",
                          "Psykiatri barn och ungdom sluten" = "Psykiatri barn och ungdom sluten",
                          "Psykiatri barn och ungdom öppen" = "Psykiatri barn och ungdom öppen",
                          "Psykiatri akut" = "Psykiatri akut",
                          "Habilitering vuxen" = "Habilitering vuxen",
                          "Habilitering barn" = "Habilitering barn",
                          "Ambulansstation" = "Ambulansstation"))


# konvertera till sf
sf_utbud = dt_utbud_long %>% 
  sf::st_as_sf(coords = c(which(colnames(dt_utbud_long) == "Sweref99Y"), which(colnames(dt_utbud_long) == "Sweref99X"))) %>%
  mutate(xUtbud = st_coordinates(geometry)[,1], yUtbud = st_coordinates(geometry)[,2]) %>%
  select(Populärnamn, Vårdtyp, VårdtypGrupp, xUtbud, yUtbud, geometry)
st_crs(sf_utbud) <- st_crs(3006)

# plotta resultat
p <- ggplot(sf_utbud)+
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(data = sf_utbud %>% st_jitter(1000), aes(color=Vårdtyp), size=5, alpha=0.4)+
  geom_text_repel(data = sf_utbud %>% group_by(Populärnamn, xUtbud, yUtbud) %>% summarise(), aes(label = Populärnamn, x = xUtbud, y = yUtbud), max.overlaps=1000)

