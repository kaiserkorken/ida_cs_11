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



########################
# EINZELTEIL/PARTS
########################

# Function to get path of the specific Documents
# Args:
#       teil_type = "T<XX>"
# Return:
#       path
teil_meta_file.path <- function(teil_type) {
  dir(file.path("Data","Einzelteil"), pattern=paste("^Einzelteil_",teil_type,".*",sep=""), full.names=TRUE) 
}

# Function combines columns with same name seperated by ".x" and ".y"
# Args:
#       df_xy [data frame where column names contain ".x" and ".y"]
# Return:
#       df_comb [combined columns of df_xy]
combine_columns<-function(df_xy){
  df_x<-select(df_xy,ends_with(".x"))%>%
    na.omit()
  names(df_x)<-gsub(".x","",names(df_x))
  
  df_y<-select(df_xy,ends_with(".y"))%>%
    na.omit()
  names(df_y)<-gsub(".y","",names(df_y))
  df_comb<- full_join(df_x,df_y)
  
  return(df_comb)
}

# Function to parse through the single parts files
# different parsing technique are needed because of different delimeters and seperators in txts
# Args:
#       ID_Teil = "ID_T<X>
# Return:
#       teil_meta = [files converted to data frame with needed columns]
load_metadata_teil <- function(ID_Teil) {
  column_of_interest<-c("ID","Werksnummer","Produktionsdatum")

  # change string to needed form
  ID_Teil_num<- str_replace_all(ID_Teil,"ID_T","") 
  
  if(strtoi(ID_Teil_num) > 10){
    ID_Teil_num<-paste0("T",ID_Teil_num)
  }else {
    ID_Teil_num<-paste0("T0",ID_Teil_num)
  }

  ID_Teil<-paste0("ID_",ID_Teil_num)
  
  # parse through files and save to teil_meta with needed columns
  
  if(ID_Teil_num == "T01") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1" | | ', .) %>%
      str_replace_all(" \\| \\| ", ";")%>%
      str_replace_all("\\s", "\n")%>%
      read_delim()%>%
      select(starts_with(column_of_interest))%>%
      combine_columns()
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum >= "2015-01-01" & Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T02") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1"  ',.)%>%
      str_replace_all("  ", ";")%>%
      str_replace_all("\t", "\n")%>%
      read_delim()%>%
      select(starts_with(column_of_interest))%>%
      combine_columns()
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum >= "2015-01-01" & Produktionsdatum < "2016-01-01") 
  }
  
  if(ID_Teil_num == "T03") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1"|',.)%>%
      str_replace_all("\\|", ",")%>%
      str_replace_all("\v", "\n")%>%
      read_delim()%>%
      select(starts_with(column_of_interest))
  }
  
  if(ID_Teil_num == "T04") {
    teil_meta<-read_delim(teil_meta_file.path(ID_Teil_num))%>%
      select(starts_with(column_of_interest))

  }
  
  if(ID_Teil_num == "T05") {
    teil_meta<-read_delim(teil_meta_file.path(ID_Teil_num))%>%

      select(starts_with(column_of_interest))%>%
      combine_columns()
    #names(teil_meta)<-gsub(".x","",names(teil_meta))

    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum >= "2015-01-01" & Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T06") {
    
    teil_meta<-read_delim(teil_meta_file.path(ID_Teil_num))%>%
      select(starts_with(column_of_interest))
  }
  
  if(ID_Teil_num == "T21") {
    teil_meta<-read_delim(teil_meta_file.path(ID_Teil_num))%>%
      select(starts_with(column_of_interest))
  }
  
  if(ID_Teil_num == "T22") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1"\t',.)%>%
      str_replace_all("\\t", ",")%>%
      str_replace_all('"Fehlerhaft_Fahrleistung"','"Fehlerhaft_Fahrleistung" ')%>%
      str_replace_all("NA","NA ")%>%
      str_replace_all(' "\\d+"', "\n")%>%
      read_delim()%>%
      select(starts_with(column_of_interest))
     teil_meta[teil_meta=="NA "]<-NA
     teil_meta <- combine_columns(teil_meta)
    
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum >= "2015-01-01" & Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T23") {
    teil_meta<-read_delim(teil_meta_file.path(ID_Teil_num))%>%

      select(starts_with(column_of_interest))%>%
      combine_columns()

    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum >= "2015-01-01" & Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T24") {
    teil_meta<-read_file(teil_meta_file.path(ID_Teil_num))%>%
      paste0('"X1_1"  ',.) %>%
      str_replace_all("\f", "\n")%>%
      str_replace_all("  ", ",")%>%
      read_delim()%>%
      select(starts_with(column_of_interest))%>%
      combine_columns()
    teil_meta$Produktionsdatum <- as.Date(teil_meta$Produktionsdatum, format= "%Y-%m-%d")
    teil_meta <- subset(teil_meta, Produktionsdatum >= "2015-01-01" & Produktionsdatum < "2016-01-01")
  }
  
  if(ID_Teil_num == "T25") {
    
    teil_meta<-read_delim(teil_meta_file.path(ID_Teil_num))%>%
      select(starts_with(column_of_interest))

  }

  # select columns and change ID_Teil to "ID_Einzelteil" for combining
  teil_meta<-teil_meta %>% 
    select(ID_Teil,"Werksnummer")
  names(teil_meta)<-gsub(ID_Teil,"ID_Einzelteil",names(teil_meta))
  
  # reduce size of df by semi_join with komp_zu_teile by "ID_Einzelteil" 
  teil_meta<- semi_join(data.frame(teil_meta), komp_zu_teile, by = "ID_Einzelteil")

  
  print("Columns present:")
  print(names(teil_meta))
  
  return(teil_meta)
}
