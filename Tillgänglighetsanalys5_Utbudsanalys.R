# indata
# sf_isokroner
# sf_individer


sf_result <-
  sf_individer %>%
  sample_n(nrow(sf_individer)*0.01) # sampla % av folkbokförda
  st_join(sf_isokroner_intervall, join = st_within, left=TRUE) %>%
  mutate(intervallDistans = ifelse(is.na(intervallDistans), "Övriga", intervallDistans)) %>%
  as.data.table() %>%
  group_by(Kommun, intervallDistans) %>% 
  summarise(AntalFolkbokförda = n()) %>%
  pivot_wider(id_cols = c(Kommun), names_from = "intervallDistans", values_from=AntalFolkbokförda) %>%
  ungroup()%>%
  rowwise() %>%
  mutate(Sum = rowSums(across(where(is.numeric)))) %>%
  mutate(pct = across(.cols = -one_of("Kommun"), .fns = ~ .x/Sum)) %>%
  select(Kommun, contains("pct"))

        
sf_result
