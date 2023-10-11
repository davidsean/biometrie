#Script pour importer les tables access des données de relevé scientifique, 
#formater les données et exporter les fichier requis pour l'éval de stock


rm(list = ls()) #vider l'espace de travail
gc() #vider la mémoire vive
if (dir.exists("~/Temp")==F) dir.create("~/Temp")
setwd("~/Temp") #définir le répertoire de travail
require(data.table, quietly = T)

#fonction lire des fichiers xls et accdb

read.db <- function(file, sheet){
  require(RODBC, quietly = T, warn.conflicts = F)
  ext <- unlist(strsplit(file, split = ".", fixed = T))[length(unlist(strsplit(file, split = ".", fixed = T)))]
  if (ext!="XLS" & ext!="xls" & ext!="xlsx" & ext!="xlsm" & ext!="xlsb" & ext!="mdb" & ext!="accdb") stop("File format not compatible")
  if (ext== "XLS" | ext=="xls" | ext=="xlsx" | ext=="xlsm" | ext=="xlsb"){
    db <- RODBC::odbcDriverConnect(paste("DRIVER=Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb); DBQ=", file, sep = ""))
    x <- RODBC::sqlFetch(db, sheet)
    RODBC::odbcClose(db)
  }
  if (ext =="mdb" | ext=="accdb"){
    db <- RODBC::odbcDriverConnect(paste("Driver=Microsoft Access Driver (*.mdb, *.accdb); DBQ=", file, sep = ""))
    x <- RODBC::sqlFetch(db, sheet)
    RODBC::odbcClose(db)
  }
  return(x)
}


#function pour comparer deux bd s'assurer que la structure est la même
comparo_db <- function(db1, db2){
  for (i in c("PROJET_MOLLUSQUE", "TRAIT_MOLLUSQUE", "TYPE_STRATE_MOLL", "SUBSTRAT_MOLLUSQUE", "ENGIN_MOLLUSQUE", "CAPTURE_MOLLUSQUE", "FREQ_LONG_MOLLUSQUE")){
      p1 <- read.db(db1,i)
      p2 <- read.db(db2,i)
      if (length(colnames(p1))!=length(colnames(p2))) print(paste(i, "different", sep = " ")) else {
        if (any(colnames(p1)==colnames(p2))==F) print(paste(i, "different", sep = " ")) else print(paste(i, "identique", sep = " "))
        
      }
    }
}



#chemin d'accès BD access
dbfile <- "S:/Petoncle/Recherche/Mission/BaseDonnées/Relevés_Pétoncle_Minganie_juin2022_ASS.mdb"

projet <- read.db(dbfile, "PROJET_MOLLUSQUE")
trait <- read.db(dbfile, "TRAIT_MOLLUSQUE")
strate <- read.db(dbfile, "TYPE_STRATE_MOLL")
substrat <- read.db(dbfile, "SUBSTRAT_MOLLUSQUE")
engin <- read.db(dbfile, "ENGIN_MOLLUSQUE")
capture <- read.db(dbfile, "CAPTURE_MOLLUSQUE")
freqL <- read.db(dbfile, "FREQ_LONG_MOLLUSQUE")
z_ges <- read.db(dbfile, "ZONE_GEST_MOLL")

an <- 2022
zone <- "16F"

