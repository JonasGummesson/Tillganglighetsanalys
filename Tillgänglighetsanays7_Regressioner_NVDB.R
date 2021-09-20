library(zoo)

################# regression utbud och konsumtion ##########################

# regression somatisk akutvård
sf_individer_nvdb_dist_besök <- sf_individer_nvdb_dist %>%
  as.data.table() %>%
  rename(DistansTillSomatiskAkut = distans) %>%
  rename(RestidTillSomatiskAkut = resväg_minuter) %>%
  select(PatientID_AES, Kön, Ålder, DistansTillSomatiskAkut, RestidTillSomatiskAkut) %>%
  inner_join(
    dt_besök_folkbokförd %>% select(-PatientID, -Kön, -Ålder, -Kommun), by=c("PatientID_AES" = "PatientID_AES")
  ) %>%
  mutate(DistansTillSomatiskAkutKm = DistansTillSomatiskAkut / 1000)

modelformula_AkutSV <- as.formula("Akut_SV ~ DistansTillSomatiskAkutKm")
modelformula_ElektivSV <- as.formula("Elektiv_SV ~ DistansTillSomatiskAkutKm")

fit.AkutSV <- lm(modelformula_AkutSV, data = sf_individer_nvdb_dist_besök %>%  filter(DistansTillSomatiskAkutKm <= 90))
fit.ElektivSV <- lm(modelformula_ElektivSV, data = sf_individer_nvdb_dist_besök)

summary(fit.AkutSV)
summary(fit.ElektivSV)


modelformula_AkutSV <- as.formula("Akut_SV ~ RestidTillSomatiskAkut")
modelformula_ElektivSV <- as.formula("Elektiv_SV ~ RestidTillSomatiskAkut")

fit.AkutSV <- lm(modelformula_AkutSV, data = sf_individer_nvdb_dist_besök %>%  filter(RestidTillSomatiskAkut <= 90))
#fit.ElektivSV <- lm(modelformula_ElektivSV, data = sf_individer_nvdb_dist_besök)

summary(fit.AkutSV)
#summary(fit.ElektivSV)


modelformula_AkutSV <- as.formula("log(Akut_SV) ~ log(RestidTillSomatiskAkut)")
fit.AkutSV <- lm(modelformula_AkutSV, data = sf_individer_nvdb_dist_besök %>%  filter(Akut_SV != 0 & RestidTillSomatiskAkut != 0))
summary(fit.AkutSV)

sf_individer_nvdb_dist_besök %>%
  select(Akut_SV, DistansTillSomatiskAkutKm) %>%
  mutate(DistansTillSomatiskAkutKm = round(DistansTillSomatiskAkutKm,0)) %>%
  group_by(DistansTillSomatiskAkutKm) %>%
  summarise(Akut_SV.mean = mean(Akut_SV), observationer = n()) %>%
  mutate(Akut_SV.mean.R7=rollapply(Akut_SV.mean,7,mean,align='right',fill=NA))%>%
  filter(DistansTillSomatiskAkutKm <= 90) %>%
  ggplot()+
  geom_line(aes(x=DistansTillSomatiskAkutKm, y=Akut_SV.mean), color = "grey")+
    geom_line(aes(x=DistansTillSomatiskAkutKm, y=Akut_SV.mean.R7), color ="blue", size=1.5)+
  labs(x="Antal km till närmaste akutmottagning", y = "Snitt, antal akutbesök per invånare")


sf_individer_nvdb_dist_besök %>%
  select(Akut_SV, RestidTillSomatiskAkut) %>%
  mutate(RestidTillSomatiskAkut = round(RestidTillSomatiskAkut,0)) %>%
  group_by(RestidTillSomatiskAkut) %>%
  summarise(Akut_SV.mean = mean(Akut_SV), observationer = n()) %>%
  mutate(Akut_SV.mean.R7=rollapply(Akut_SV.mean,7,mean,align='right',fill=NA))%>%
  filter(RestidTillSomatiskAkut <= 90) %>%
  ggplot()+
  geom_line(aes(x=RestidTillSomatiskAkut, y=Akut_SV.mean), color = "grey")+
  geom_line(aes(x=RestidTillSomatiskAkut, y=Akut_SV.mean.R7), color ="blue", size=1.5)+
  labs(x="Restid minuter till närmaste akutmottagning", y = "Snitt, antal akutbesök per invånare")
############################################################################





