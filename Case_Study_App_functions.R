library(data.table)
library(dplyr)

##########################
# FAHRZEUGE -> KOMPONENTEN
##########################

#read a Komponente->Einzelteil file and sanitise the data
load_komp_zu_teile <- function(ID_Komp) {
	filename = paste("Data/Komponente/Bestandteile_Komponente_", ID_Komp, ".csv",sep="")
	print(filename)
	frame <- fread(filename,header=TRUE,drop=1) 
	print(nrow(frame))

	columns = colnames(frame)
	teile=character(0)
	for (i in columns[1:length(columns)-1]) {
		beginning_of_colname <- unlist(strsplit(i,split="_"))[1] 
		if (beginning_of_colname == "ID") {
			teile <- append(teile, i)
		}
	}

	print(teile)
	frame <- frame %>% 
		reshape(varying=teile, v.names = "ID_Einzelteil", direction="long",times=teile) %>%
		select(ID_Komponente=paste("ID_",ID_Komp,sep=""), ID_Einzelteil,time) %>%
		rename(Typ_Einzelteil=time)
		
	return(frame)
}





########################
# KOMPONENTEN/COMPONENTS
########################

# helper function: get filepath for Komponente type
komp_meta_file.path <- function(komp_type) {
	dir(file.path("Data","Komponente"), pattern=paste("^Komponente_",komp_type,".*",sep=""), full.names=TRUE) 
}



load_metadata_komp <- function(ID_Komp) {
	
	print(komp_meta_file.path(ID_Komp))
	

	komp_meta <- fread(file=komp_meta_file.path(ID_Komp), select=c("ID_Motor","ID_Schaltung","ID_Schaltung.x","ID_Schaltung.y","Werksnummer","Werksnummer.x","Werksnummer.y"), header=TRUE)

	print("Columns present:")
	print(names(komp_meta))
	
	# Problem: Some data is spread over multiple (apparently wrongly joined) columns.
	# These are the columns we want in the end.
	target_columns <- list("ID_Motor","ID_Schaltung","Werksnummer")
	
	for (i in target_columns) {
		print("Now consolidating column:")
		print(i)
		columns_to_combine <- intersect(grep(paste(i,"($|..$)",sep=""),names(komp_meta),value=TRUE), names(komp_meta))
		print(columns_to_combine)
		if (length(columns_to_combine > 0)) {

			komp_meta <- komp_meta %>%
				reshape(varying=columns_to_combine, direction="long", v.name=i) %>%
				select(-id,-time) %>%
				# This creates a lot of triplicate entries that are just NA. Remove them.
				#   .data[[i]] is to get a column name from a variable (i in this case)
				filter(!is.na(.data[[i]]))
		}
	}
	
	colnames(komp_meta) <- c("ID_Komponente","Werksnummer")


	print(summary(komp_meta))
	
	return(komp_meta)


}

