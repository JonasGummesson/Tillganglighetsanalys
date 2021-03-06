library(odbc)

sql_utbudspunkt = "
SELECT 
	plats.*
	,bridge.UtbudspunktID 
	,punkt.Popul�rnamn
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

# pivotera och st�da dataset
dt_utbud_long <- 
  dt_utbud %>% 
  pivot_longer(cols = SomatikSluten:Privat, names_to = "V�rdtyp", values_to="V�rdtypFinns") %>%
  filter(V�rdtypFinns == "X") %>%
  select(-V�rdtypFinns) %>%
  mutate(
    Regi = ifelse(V�rdtyp == "Privat", "Privat", "Offentlig"),
    V�rdtyp = recode(V�rdtyp, 
                     "Privat" = "Privat v�rdcentral",
                     "SomatikSluten" = "Somatik sluten",
                     "SomatikAkut" = "Somatik akut",
                     "SomatikN�rakut" = "Somatik n�rakut",
                     "PsykiatriVixenSluten" = "Psykiatri vuxen sluten",
                     "PsykiatriVuxen�ppen" = "Psykiatri vuxen �ppen",
                     "PsykiatriBarnOchUngdomSluten" = "Psykiatri barn och ungdom sluten",
                     "PsykiatriBarnOchUngdom�ppen" = "Psykiatri barn och ungdom �ppen",
                     "PsykiatriAkut" = "Psykiatri akut",
                     "HabiliteringVixen" = "Habilitering vuxen",
                     "HabiliteringBarn" = "Habilitering barn",
                     "Ambulansstation" = "Ambulansstation"),
    V�rdtypGrupp = recode(V�rdtyp, 
                          "Privat" = "Privat v�rdcentral",
                          "Somatik sluten" = "Somatik sluten",
                          "Somatik akut" = "Somatik akut",
                          "Somatik n�rakut" = "Somatik akut",
                          "Psykiatri vuxen sluten" = "Psykiatri vuxen sluten",
                          "Psykiatri vuxen �ppen" = "Psykiatri vuxen �ppen",
                          "Psykiatri barn och ungdom sluten" = "Psykiatri barn och ungdom sluten",
                          "Psykiatri barn och ungdom �ppen" = "Psykiatri barn och ungdom �ppen",
                          "Psykiatri akut" = "Psykiatri akut",
                          "Habilitering vuxen" = "Habilitering vuxen",
                          "Habilitering barn" = "Habilitering barn",
                          "Ambulansstation" = "Ambulansstation"))


# konvertera till sf
sf_utbud = dt_utbud_long %>% 
  sf::st_as_sf(coords = c(which(colnames(dt_utbud_long) == "Sweref99Y"), which(colnames(dt_utbud_long) == "Sweref99X"))) %>%
  mutate(xUtbud = st_coordinates(geometry)[,1], yUtbud = st_coordinates(geometry)[,2]) %>%
  select(Popul�rnamn, V�rdtyp, V�rdtypGrupp, xUtbud, yUtbud, geometry)
st_crs(sf_utbud) <- st_crs(3006)

# plotta resultat
p <- ggplot(sf_utbud)+
  geom_sf(data = sf_kommuner_dalarna)+
  geom_sf(data = sf_utbud %>% st_jitter(1000), aes(color=V�rdtyp), size=5, alpha=0.4)+
  geom_text_repel(data = sf_utbud %>% group_by(Popul�rnamn, xUtbud, yUtbud) %>% summarise(), aes(label = Popul�rnamn, x = xUtbud, y = yUtbud), max.overlaps=1000)

