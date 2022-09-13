library(data.table)
library(dplyr)

##########################
# GENERAL FUNCTIONS
##########################

# consolidate similar columns
consolidate_cols <- function(frame, target_columns) {
	
	frame <- as.data.frame(frame)

	for (i in target_columns) {
		print("Now consolidating column:")
		print(i)
		columns_to_combine <- intersect(grep(paste(i,"($|..$)",sep=""),names(frame),value=TRUE), names(frame))
		print(columns_to_combine)
		if (length(columns_to_combine > 0)) {

			frame <- frame %>%
				reshape(varying=columns_to_combine, direction="long", v.name=i) %>%
				select(-id,-time) %>%
				# This creates a lot of triplicate entries that are just NA. Remove them.
				#   .data[[i]] is to get a column name from a variable (i in this case)
				filter(!is.na(.data[[i]]))
		}
	}
	
	return(frame)
}


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
	
	komp_meta <- consolidate_cols(komp_meta, target_columns)
	
	
	colnames(komp_meta) <- c("ID_Komponente","Werksnummer")


	print(summary(komp_meta))
	
	return(komp_meta)


}



########################
# EINZELTEIL/PARTS
########################
teil_meta_file.path <- function(teil_type) {
  dir(file.path("Data","Einzelteil"), pattern=paste("^Einzelteil_",teil_type,".*",sep=""), full.names=TRUE) 
}

load_metadata_teil <- function(ID_Teil) {
  ID_Teil_num<- str_replace_all(ID_Teil,"ID_T","")
  if(as.numeric(ID_Teil_num) >= 10){
    ID_Teil_num<-paste0("T",ID_Teil_num)
  }else {
    ID_Teil_num<-paste0("T0",ID_Teil_num)
  }
  
  ID_Teil<-paste0("ID_",ID_Teil_num)
  print(teil_meta_file.path(ID_Teil_num))
  
  if(ID_Teil_num == "T01") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1" | | ', .) %>%
      str_replace_all(" \\| \\| ", ";")%>%
      str_replace_all("\\s", "\n")%>%
      read_delim()%>%
      select(contains(".x"))
    names(teil_meta)<-gsub(".x","",names(teil_meta))
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T02") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1"  ',.)%>%
      str_replace_all("  ", ";")%>%
      str_replace_all("\t", "\n")%>%
      read_delim()%>%
      select(contains(".x"))
    names(teil_meta)<-gsub(".x","",names(teil_meta))
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum < "2016-01-01") 
  }
  
  if(ID_Teil_num == "T03") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1"|',.)%>%
      str_replace_all("\\|", ",")%>%
      str_replace_all("\v", "\n")%>%
      read_delim()
  }
  
  if(ID_Teil_num == "T04") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))
  }
  
  if(ID_Teil_num == "T05") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      select(contains(".x"))
    names(teil_meta)<-gsub(".x","",names(teil_meta))
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T06") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))
  }
  
  if(ID_Teil_num == "T21") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))
  }
  
  if(ID_Teil_num == "T22") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1"\t',.)%>%
      str_replace_all("\\t", ",")%>%
      str_replace_all('"Fehlerhaft_Fahrleistung"','"Fehlerhaft_Fahrleistung" ')%>%
      str_replace_all("NA","NA ")%>%
      str_replace_all(' "\\d+"', "\n")%>%
      read_delim()%>%
      select(contains(".x"))
    names(teil_meta)<-gsub(".x","",names(teil_meta))
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T23") {
    print("hier")
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      select(contains(".x"))
    names(teil_meta)<-gsub(".x","",names(teil_meta))
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T24") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1"  ',.) %>%
      str_replace_all("\f", "\n")%>%
      str_replace_all("  ", ",")%>%
      read_delim()%>%
      select(contains(".x"))
    names(teil_meta)<-gsub(".x","",names(teil_meta))
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T25") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))
  }
  
  
  teil_meta <- teil_meta %>%
  	rename("ID_Einzelteil" = .data[[ID_Teil]])
  
  
  target_columns <- c("ID_Einzelteil","Werksnummer")
  
  teil_meta <- consolidate_cols(teil_meta, target_columns)
  
  
  teil_meta <- teil_meta %>% select("ID_Einzelteil","Werksnummer")
  
  
  #teil_meta <- fread(file=teil_meta_file.path(ID_Teil), select=c(ID_Teil,"Werksnummer"), header=TRUE)


   print("Columns present:")
   print(names(teil_meta))
  
  # Problem: Some data is spread over multiple (apparently wrongly joined) columns.
  # These are the columns we want in the end.
  #target_columns <- list("ID_Motor","ID_Schaltung","Werksnummer")
}
