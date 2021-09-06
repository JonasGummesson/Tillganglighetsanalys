########################### ladda dataset #####################################
# läs in vägkartor från Lantmäteriet & Nationella vägdatabasen (NVDB)
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys1a_Vägnät.R")
# läs in vårdutbudspunkter i Dalarna
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys1b_Utbud.R")
# läs in adresser och koordinater för folkbokförda i Dalarna
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys1c_Adresser.R")

########################### förbereda vägnät för nätverksanalys ####################
# förbereda vägnätsobjekt
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys2a_FörberedaVägnät.R")
# skapa separat testdata för enbart Falun
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys2b_TestdataFalun.R")

########################### skapa nätverksobjekt och beräkna avstånd ####################
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys3a_SkapaNätverk.R")
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys3b_BeräknaAvståd.R")

########################### skapa isokroner ####################
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys4_Isokroner.R")

########################### skapa nätverksobjekt och beräkna avstånd ####################
source("~/GitHub/Tillganglighetsanalys/Tillgänglighetsanalys5_Utbudsanalys.R")