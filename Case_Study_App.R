library(dplyr)
library(data.table)
library(stringr)
library(readr)

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

#list_teil <- unique(fread(text=komp_zu_teile$ID_Einzelteil, sep="-", select=1))
list_teil <- unique(str_match(komp_zu_teile$Typ_Einzelteil, "(ID_T[:digit:])")[,1])
print(list_teil)



############################
# EINZELTEILE / SINGLE PARTS
############################

# no link files here since there are no further branches

# TODO

teil_meta <- lapply(list_teil, load_metadata_teil)



############################
# GEODATA
############################


geodata <- lapply(dir(file.path("Data","Geodaten"), pattern="Werke", full.names=TRUE), fread, select=1:5, keepLeadingZeros=FALSE) %>%
	rbindlist() %>%
	mutate(Werk = as.numeric(gsub("O","",Werk))) %>%
	rename(Längengrad = 5)
	


#############################
# LINKING EVERYTHING TOGETHER
#############################

	
# join Fahrzeug->Komponente and Komponente->Einzelteil table
fz_komp_teile <- fz_zu_komp %>%
	inner_join(komp_zu_teile,by="ID_Komponente", suffix=c("_Fahrzeug","_Komponente"))


summary(fz_komp_teile)

# join geodata (for vehicles and component manufacturer, TODO: add single part manufacturer
fz_komp_teile %>%
	inner_join(geodata, by=c(Werksnummer_Fahrzeug="Werk")) %>%
	inner_join(geodata, by=c(Werksnummer_Komponente="Werk"), suffix=c("_Fahrzeug","_Komponente"))



