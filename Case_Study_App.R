if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}
if (!require(data.table)) {
  install.packages("data.table")
  require(data.table)
}
if (!require(stringr)) {
  install.packages("stringr")
  require(stringr)
}
if (!require(readr)) {
  install.packages("readr")
  require(readr)
}
if (!require(geosphere)) {
  install.packages("geosphere")
  require(geosphere)
}

source("Case_Study_App_functions.R")

# Generally the original data is in "wide" format, with seperate columns for different types of components and parts.
# Therefor, the data is transposed to "long" format. That is what all the reshape() are for.


###########################
# FAHRZEUGE / VEHICLES
###########################

# there are no special functions for reading the vehicle data, since there are only two metadata and link files respectively

#################
# read Fahrzeug -> Komponente link files and append into one table

fz_komp_files <- c("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv","Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv")

fz_zu_komp <- lapply(fz_komp_files,fread,select=c("ID_Fahrzeug","ID_Motor","ID_Schaltung"),header=TRUE) %>%
	rbindlist() %>%
	reshape(varying=c("ID_Motor","ID_Schaltung"), v.names = "ID_Komponente", times=c("Motor","Schaltung"), direction="long") %>%
	rename(Typ_Komponente = time) %>%
	select(-id)
	


# read Fahrzeug metadata files and append into one table

fz_files <- c("Data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv","Data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv")

fz_meta <- lapply(fz_files,fread,select=c("ID_Fahrzeug","Produktionsdatum","Werksnummer","xxxx"),header=TRUE) %>%
	rbindlist() %>%
	rename(Werksnummer = Werksnummer) %>%
	filter(Produktionsdatum >= "2015-01-01" & Produktionsdatum <= "2015-12-31")


# join metadata and link data

fz_zu_komp <- fz_zu_komp %>%
	inner_join(fz_meta,by="ID_Fahrzeug")
	

# from the Komponente IDs, create a list of all component types included
list_komp <- unique(str_match(fz_zu_komp$ID_Komponente, "(K[1-7]\\w*)")[,1])
	

	
###########################
# KOMPONENTE / COMPONENT
###########################


# read Komponente metadata files and append into one table

komp_meta <- lapply(list_komp, load_metadata_komp) %>%
	rbindlist()

	
# read Komponente -> Einzelteil link files and append into one table, then join metadata and link data
	
komp_zu_teile <- lapply(list_komp, load_komp_zu_teile) %>%
	rbindlist() %>%
	inner_join(komp_meta, by="ID_Komponente")



# from the Einzelteil IDs, create a list of all part types included


list_teil <- str_match(unique(komp_zu_teile$Typ_Einzelteil), "(ID_T[:digit:]+)")[,1]


############################
# EINZELTEILE / SINGLE PARTS
############################

# no link files here since there are no further branches

teil_meta <- lapply(list_teil, load_metadata_teil) %>%
  rbindlist()
komp_zu_teile_werk<-inner_join(komp_zu_teile, teil_meta,by="ID_Einzelteil", suffix=c("_Komponente","_Einzelteil"))


############################
# GEODATA
############################


geodata <- lapply(dir(file.path("Data","Geodaten"), pattern="Werke", full.names=TRUE), fread, select=1:5, keepLeadingZeros=FALSE) %>%
	rbindlist() %>%
	mutate(Werk = as.numeric(gsub("O","",Werk))) %>%
	rename(Längengrad = 5)%>%
  na.omit()%>%
  select(-contains(c("PLZ")))

zulassung<- read_csv2("Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv")%>%
  select(-contains("...1"))
gemeinde_geo <- read_csv2("Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv")
names(gemeinde_geo)<-  gsub("Laengengrad","Längengrad",names(gemeinde_geo))

geo_state_city <- read_csv("Additional_files/Gemeinde_Bund_HS_Geo.csv")


#############################
# REGISTRATION DATA
#############################

regdata <- fread(file.path("Data","Zulassungen","Zulassungen_alle_Fahrzeuge.csv"),drop=1) %>%
  rename(Datum_Zulassung = "Zulassung",ORT_Zulassung = "Gemeinden")



#############################
# LINKING EVERYTHING TOGETHER
#############################

	
# join Fahrzeug->Komponente and Komponente->Einzelteil table
names(fz_zu_komp)<-gsub("Werksnummer","Werksnummer_Fahrzeug",names(fz_zu_komp))
fz_komp_teile <- fz_zu_komp %>%
	inner_join(komp_zu_teile_werk,by="ID_Komponente")
fz_komp_teile$Werksnummer_Einzelteil<-as.double(fz_komp_teile$Werksnummer_Einzelteil)
fz_komp_teile_zul <- fz_komp_teile %>%
  inner_join(zulassung,by=c("ID_Fahrzeug" = "IDNummer"))#%>%


summary(fz_komp_teile_zul)

# join geodata (for vehicles and component manufacturer
fz_komp_teile_geo <- fz_komp_teile_zul %>%
	inner_join(geodata, by=c(Werksnummer_Fahrzeug="Werk")) %>%
	inner_join(geodata, by=c(Werksnummer_Komponente="Werk"), suffix=c("", "_Komponente"))%>%
  inner_join(geodata, by=c(Werksnummer_Einzelteil="Werk"), suffix=c("", "_Einzelteil"))%>%
  inner_join(gemeinde_geo, by=c("Gemeinden" = "Gemeinde"), suffix=c("_Fahrzeug",""))%>%
  inner_join(geo_state_city, by=c("Gemeinden"="Gemeinde"), suffix=c("_Gemeinde","_Hauptstadt"))



# calculate distances of material flow

fz_komp_teile_geo_dist <- fz_komp_teile_geo %>%
  mutate(Distanz_Einzelteil_zu_Komponente_in_km = calc_distance_in_km(Längengrad_Einzelteil, Breitengrad_Einzelteil, Längengrad_Komponente, Breitengrad_Komponente)) %>%
  mutate(Distanz_Komponente_zu_Fahrzeug_in_km = calc_distance_in_km(Längengrad_Komponente, Breitengrad_Komponente, Längengrad_Fahrzeug, Breitengrad_Fahrzeug)) %>%
  mutate(Distanz_Fahrzeug_zu_Hauptstadt_in_km = calc_distance_in_km(Längengrad_Fahrzeug, Breitengrad_Fahrzeug, Längengrad_Hauptstadt, Breitengrad_Hauptstadt))%>%
  mutate(Distanz_Hauptstadt_zu_Gemeinde_in_km = calc_distance_in_km(Längengrad_Hauptstadt, Breitengrad_Hauptstadt, Längengrad_Gemeinde, Breitengrad_Gemeinde ))
summary(fz_komp_teile_geo)

fwrite(fz_komp_teile_geo_dist,file=paste0(getwd(),"/Final_Data_Group_11.csv"), row.names = FALSE)
