########################### ladda dataset #####################################
# läs in vägkartor från Lantmäteriet & Nationella vägdatabasen (NVDB)
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys1a_Vägnät.R")
# läs in vårdutbudspunkter i Dalarna
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys1b_Utbud.R")
# läs in adresser och koordinater för folkbokförda i Dalarna
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys1c_Vårdtagare.R")

########################### förbereda vägnät för nätverksanalys ####################
# förbereda vägnätsobjekt
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys2a_FörberedaVägnät.R")
# skapa separat testdata för enbart Falun
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys2b_TestdataFalun.R")

########################### skapa nätverksobjekt och beräkna avstånd ####################
#source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys3a_SkapaNätverk_LM.R")
#source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys3b_BeräknaAvstånd_LM.R")


source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys3a_SkapaNätverk_NVDB.R")
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys3b_BeräknaAvstånd_NVDB.R")

########################### skapa isokroner ####################
#source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys4_Isokroner_LM.R")
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys4_Isokroner_NVDB.R")

########################### analys av utbud ####################
#source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys5_Utbudsanalys_LM.R")
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys5_Utbudsanalys_NVDB.R")





########################### beräkna kontinuerlig distans mellan utbud och individer ####################
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys6_IndividTillUtbud.R")



########################### kör regressioner på utbud och avstånd ####################
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys7_Regressioner.R")
