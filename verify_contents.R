#################################


rm(list = ls()) #vider l'espace de travail
gc() #vider la mémoire vive
if (dir.exists("~/Temp")==F) dir.create("~/Temp")
setwd("~/Temp") #définir le répertoire de travail
require(data.table, quietly = T)



# read filtered table data
filter_table <-function(src_fname, t_name, annee, cod_serie_hist) {
  #' Read table t_name and filter data by year and cod_serie_hist
  #'
  #' The table name t_name(string), is filtered by annee (int) and cod_serie_hist (int)
  #' 
  require(RODBC, quietly = T, warn.conflicts = T)
  full_path <- paste("S:/Petoncle/Recherche/Mission/BaseDonnées/",src_fname,sep = "")
  db <- RODBC::odbcDriverConnect(paste("Driver=Microsoft Access Driver (*.mdb, *.accdb); DBQ=",full_path , sep = ""))
  query <- sprintf('SELECT * FROM %s WHERE ANNEE=%d AND COD_SERIE_HIST=%d', t_name, annee, cod_serie_hist)
  
  x <- RODBC::sqlQuery(db, query)
  RODBC::odbcClose(db)
  return(x)
}



a <-filter_table("S:/Petoncle/Recherche/Mission/BaseDonnées/maitre.mdb", "PROJET_MOLLUSQUE", 2009, 18)


# The tables that actually contain survey data
data_tables <- c('PROJET_MOLLUSQUE', 
                 'TRAIT_MOLLUSQUE', 
                 'ENGIN_MOLLUSQUE',
                 'CAPTURE_MOLLUSQUE', 
                 'FREQ_LONG_MOLLUSQUE', 
                 'SUBSTRAT_MOLLUSQUE',
#                 'BIOMETRIE_MOLLUSQUE',
                 )


df = data.frame(matrix(nrow = 0, ncol = 3))
colnames(df) = c('annee', 'cod_serie_hist', 'src_file')
df <- rbind(df, list(2009, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2010, 15, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2010, 16, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2011, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2012, 15, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2012, 16, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2013, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2014, 15, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2014, 16, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2015, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2016, 15, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2016, 16, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2016, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2017, 18, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2018, 15, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2018, 16, "Relevés_Pétoncle_Globale_juin2019_PG_Corrigee.mdb"))
df <- rbind(df, list(2019, 15, "Relevés_Pétoncle_Globale_juin2019_BD.mdb"))
df <- rbind(df, list(2019, 16, "Relevés_Pétoncle_Globale_juin2019_BD.mdb"))
df <- rbind(df, list(2019, 18, "Relevé35_Pétoncle_IdM_2019-10-04_BD_DS.mdb"))
df <- rbind(df, list(2021, 18, "Relevé35_Pétoncle_IdM_AS_17-11-2021.mdb"))
df <- rbind(df, list(2022, 15, "Relevés_Pétoncle_Minganie_juin2022_ASS"))
df <- rbind(df, list(2022, 16, "Relevés_Pétoncle_Minganie_juin2022_ASS"))
df <- rbind(df, list(2022, 18, "Relevé36_Pétoncle_IdM_2022_oct_ASS"))



for (t_name in data_tables){
  # compare table between src_data and master
  for(i in 1:nrow(df)) {
    print(sprintf("Checking %s:%s ...", source_file, t_name))
    source_file <- df[i,3]
    year <- df[i,1]
    cod_serie_hist <- df[i,2]
    master  <- filter_table("maitre.mdb",t_name ,year , cod_serie_hist)
    src_data  <- filter_table(source_file, t_name, year, cod_serie_hist)
    if (! all.equal(master,src_data)) {
      error_msg = sprintf("Problem at table:%s:%s", source_file, t_name)
      print(error_msg)
      exit()
    }
    print("OK!")
  
  }
}

