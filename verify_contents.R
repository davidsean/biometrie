#################################


rm(list = ls()) #vider l'espace de travail
gc() #vider la mémoire vive
if (dir.exists("~/Temp")==F) dir.create("~/Temp")
setwd("~/Temp") #définir le répertoire de travail
require(data.table, quietly = T)



# read filtered table data
filter_table <-function(src_fname, t_name, no_releve, cod_source_info, error_at_empty=T) {
  #' Read table t_name and filter data by year and cod_serie_hist
  #'
  #' src_fname: filename of MSAccess file
  #' t_name: table name containing data
  #' no_releve: value for survey number filter
  #' cod_source_info: value for cod_source_info filter
  #' error_at_empty: will cause an error if the results are empty
  #'
  #' The cod_source_info parameter needs to be one of the two: 
  #' 18 -> Évaluation de stocks IML - Pétoncle Minganie
  #' 19 -> Évaluation de stocks IML - Pétoncle I de M
  #' 
  #'

  #parameter validation
  allowed_vals <- c(18, 19)
  if (! cod_source_info %in% allowed_vals ) {
    stop("invalid value for cod_source_info")
  }
  
  require(RODBC, quietly = T, warn.conflicts = T)
  full_path <- paste("S:/Petoncle/Recherche/Mission/BaseDonnées/", src_fname, sep="")
  db <- RODBC::odbcDriverConnect(paste("Driver=Microsoft Access Driver (*.mdb, *.accdb); DBQ=", full_path, sep=""))

  if (t_name == "PROJET_MOLLUSQUE") {
    sort_statement <- ""
  } else if (t_name == "CAPTURE_MOLLUSQUE"){
    sort_statement <- "ORDER BY IDENT_NO_TRAIT, COD_ESP_GEN ASC"
  } else if (t_name == "FREQ_LONG_MOLLUSQUE"){
    sort_statement <- "ORDER BY IDENT_NO_TRAIT, COD_ESP_GEN, NO_MOLLUSQUE ASC"
  } else if (t_name == "SUBSTRAT_MOLLUSQUE"){
    sort_statement <- "ORDER BY IDENT_NO_TRAIT, COD_TYP_SUBSTRAT ASC"
  } else {
    sort_statement <- "ORDER BY IDENT_NO_TRAIT ASC"
  }
  
  query <- sprintf('SELECT * FROM %s WHERE NO_RELEVE=%d AND COD_SOURCE_INFO=%d %s', t_name, no_releve, cod_source_info, sort_statement)
  
  x <- RODBC::sqlQuery(db, query)

  RODBC::odbcClose(db)
  if ( nrow(x)==0){
    print(sprintf("Empty %s:%s:%d:%d ...", source_file, t_name, no_releve, cod_source_info))
    if (error_at_empty){
      stop("empty table")
    }
  }
  
  return(x)
}

# test the function...
#master  <- filter_table("maitre.mdb", "TRAIT_MOLLUSQUE", 16, 18)
#src_data  <- filter_table("Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb", "SUBSTRAT_MOLLUSQUE", 16, 18)


#require(RODBC, quietly = T, warn.conflicts = T)
#full_path <- paste("S:/Petoncle/Recherche/Mission/BaseDonnées/", "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb", sep="")
#db <- RODBC::odbcDriverConnect(paste("Driver=Microsoft Access Driver (*.mdb, *.accdb); DBQ=", full_path, sep=""))
#sort_statement <- "ORDER BY IDENT_NO_TRAIT, COD_TYP_SUBSTRAT ASC"
#query <- sprintf('SELECT * FROM %s WHERE NO_RELEVE=%d AND COD_SOURCE_INFO=%d %s', "SUBSTRAT_MOLLUSQUE", 16, 18, sort_statement)
#x <- RODBC::sqlQuery(db, query)
#RODBC::odbcClose(db)



# The tables that actually contain survey data
data_tables <- c('PROJET_MOLLUSQUE', 
                 'TRAIT_MOLLUSQUE', 
                 'ENGIN_MOLLUSQUE',
                 'CAPTURE_MOLLUSQUE', 
                 'FREQ_LONG_MOLLUSQUE', 
                 'SUBSTRAT_MOLLUSQUE'
                 )
#                 'BIOMETRIE_MOLLUSQUE', # no need to compare this one!

# create a dataframe that contains the pertinent scope for the (year+cod_serie_hist) for the survey
df = data.frame(matrix(nrow = 0, ncol = 3))
df <- rbind(df, list(2, 19, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(3, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(4, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(5, 19, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(6, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(7, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(8, 19, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(9, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(10, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(11, 19, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(12, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(13, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(15, 19, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(16, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(17, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(28, 18, "Relevés_Pétoncle_Globale_juin2019_BD.mdb"))
df <- rbind(df, list(29, 18, "Relevés_Pétoncle_Globale_juin2019_BD.mdb"))
df <- rbind(df, list(34, 19, "Relevé35_Pétoncle_IdM_2019-10-04_BD_DS.mdb"))
df <- rbind(df, list(35, 19, "Relevé35_Pétoncle_IdM_AS_17-11-2021.mdb"))
df <- rbind(df, list(30, 18, "Relevés_Pétoncle_Minganie_juin2022_ASS.mdb"))
df <- rbind(df, list(31, 18, "Relevés_Pétoncle_Minganie_juin2022_ASS.mdb"))
df <- rbind(df, list(36, 19, "Relevé36_Pétoncle_IdM_2022_oct_ASS.mdb"))
colnames(df) = c('no_releve', 'cod_source_info', 'src_file')


####
# The loop that does the work
####


# iterate over data tables
err <- NULL
for (t_name in data_tables){
  # iterate over data sources
  for(i in 1:nrow(df)) {
    source_file <- df[i,3]
    no_releve <- df[i,1]
    cod_source_info <- df[i,2]
    
    print(sprintf("Checking %s:%s:%d:%d ...", source_file, t_name, no_releve, cod_source_info))
    master  <- filter_table("maitre.mdb", t_name, no_releve, cod_source_info)
    src_data  <- filter_table(source_file, t_name, no_releve, cod_source_info)
    if (! isTRUE(all.equal(master,src_data))) {
      error_msg = sprintf("Problem at table:%s:%s", source_file, t_name)
      print(error_msg)
      err <- c(err,error_msg)
    } else {
      print("OK!")
    }
  
  }
  
}

