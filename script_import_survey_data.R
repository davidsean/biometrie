#################################

#Aight, de 2009 à 2023 les fichiers biometrie .xls sont disponible et devraient être utilisé 


#pour les des et tai, l'utilisation des accdb pour la même période est possible... ça pourrait être beaucoup 
#plus efficace que jouer sur les fichier .dat

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


#fonction pour convertir de minuscule à majuscule
format_CAPS <- function(x){
  x[x=="a"] <- "A"
  x[x=="b"] <- "B"
  x[x=="c"] <- "C"
  x[x=="d"] <- "D"
  x[x=="e"] <- "E"
  x[x=="f"] <- "F"
  x[x=="g"] <- "G"
  x[x=="h"] <- "H"
  x[x=="i"] <- "I"
  x[x=="j"] <- "J"
  x[x=="k"] <- "K"
  x[x=="l"] <- "L"
  x[x=="m"] <- "M"
  x[x=="n"] <- "N"
  x[x=="o"] <- "O"
  x[x=="p"] <- "P"
  x[x=="q"] <- "Q"
  x[x=="r"] <- "R"
  x[x=="s"] <- "S"
  x[x=="t"] <- "T"
  x[x=="u"] <- "U"
  x[x=="v"] <- "V"
  x[x=="w"] <- "W"
  x[x=="x"] <- "X"
  x[x=="y"] <- "Y"
  x[x=="z"] <- "Z"
  return(x)
}

#chlamys islandica
bio_cn_2023 <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2023/Donnees/Biometrie_minganie_2023.xlsx", "Feuil1")
bio_cn_2022 <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2022/Donnees/MCN_2022.xlsx", "Biometrie")
bio_cn_2019 <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2019/Donnees/MCN_2019_Biometrie_Final_PG.xlsx", "Biometrie16E")
bio_cn_2018_16E <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2018/Donnees/Minganie_biometrie_Petoncle_2018_BD.xls", "Biometrie16E")
bio_cn_2018_16F <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2018/Donnees/Minganie_biometrie_Petoncle_2018_BD.xls", "Biometrie16F")
bio_cn_2016_16E <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2016/Donnees/Minganie_biometrie_Petoncle_2016.xls", "Biometrie16E")
bio_cn_2016_16F <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2016/Donnees/Minganie_biometrie_Petoncle_2016.xls", "Biometrie16F")
bio_cn_2014_16E <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2014/Donnees/Minganie_biometrie_Petoncle_2014.xls", "Biométrie16E")
bio_cn_2014_16F <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2014/Donnees/Minganie_biometrie_Petoncle_2014.xls", "Biométrie16F")
bio_cn_2012_16E <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2012/Donnees/Minganie_pétoncles_Biométrie_2012.xls", "Biométrie16E")
bio_cn_2012_16F <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2012/Donnees/Minganie_pétoncles_Biométrie_2012.xls", "Biométrie16F")
bio_cn_2010_16E <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2010/Donnees/biometrie2010.xls", "Bio16E")
bio_cn_2010_16F <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2010/Donnees/biometrie2010.xls", "Bio16F")
#2009... pas de biométrie??
bio_cn_2008_16E <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2008/Donnees/biometrie2008.xls", "16E")
bio_cn_2008_16F <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2008/Donnees/biometrie2008.xls", "16F")
bio_cn_2007 <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2007/Donnees/Irm07bio.xls", "Feuil1")
bio_cn_2005 <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2005/Donnees/Irm05bio.xls", "Biometrie SAISIE")
bio_cn_2004_16E <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2004/MCN04_16E/Donnees/Irm04bio.xls", "Biometrie SAISIE")
bio_cn_2004_16F <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2004/MCN04_16F/Donnees/Irm04bio_16F.xls", "Biometrie SAISIE")
bio_cn_2003 <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2003/Donnees/Irm03bio.xls", "Toto")
bio_cn_2001 <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2001/Donnees/Irm01bio.xls", "Biometrie")
#1999 zone 16A?!?

#placopectens magallanicus

bio_cn_2022_placo <- read.db("S:/Petoncle/Recherche/Mission/Cotenord/2022/Donnees/MCN_2022.xlsx", "Biometrie 16E placopecten")

#les années antérieur à 2001 pour la CN sont en .dat


#IdM

bio_IDM_2022 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2022/donnees/Biometrie/Biometrie_IDM_2022.xlsx", "bio")
bio_IDM_2021 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2021/Données/Biométrie/Pri21_biométrie_2021.AS.xlsx", "biometrie")
bio_IDM_2019 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2019/Données/Biometrie/Pri2019_biometrie_2019-11-21_PG.xlsx", "biometrie")
bio_IDM_2017 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2017/Données/Biometrie_2017.xls", "Feuil2")
bio_IDM_2015 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2015/Données/Biometrie_2015.xls", "Feuil2")
bio_IDM_2013 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2013/Données/Pri13bio.xls", "Saisie")
bio_IDM_2011 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2011/Données/Biometrie_2011.xls", "saisie")
bio_IDM_2009 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2009/Données/Pri09bio.xls", "Saisie")
bio_IDM_2008 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2008/Données/Pri08bio.xls", "Saisie")
bio_IDM_2007 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2007/Données/Pri07bio.xls", "Saisie")
bio_IDM_2005 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2005/Données/Pri05bio.xls", "Biometrie SAISIE")
bio_IDM_2004 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2004/Données/Pri04bio.xls", "Pri04bio")
bio_IDM_2001 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2001/Données/PRI01BIO.xls", "Saisie")
bio_IDM_2000 <- read.db("S:/Petoncle/Recherche/Mission/Îles/2000/donnees/PRI00BIO.xls", "Feuil1")
bio_IDM_1999 <- read.db("S:/Petoncle/Recherche/Mission/Îles/1999/Donnees/Pri99bio.xls", "Pri99bio")
bio_IDM_1998 <- read.db("S:/Petoncle/Recherche/Mission/Îles/1998/donnees/PRI98___.XLS", "biométrie")

#les années antérieurs à 1998 pour les iles sont en .dat


#save.image("Backup_surv_bio.Rdata")
load("Backup_surv_bio.Rdata")

#########################

#structure : 


str_bio <- function(x){
  y <- data.frame(matrix(nrow = nrow(x), ncol = 20))
  colnames(y) <- c("annee", "date", "zone", "s_zone", "trait", "station", "panier", "espece", "no", "taille", "pds_vif", 
                   "pds_musc", "pds_gon", "pds_visc", "sexe", "age", "no_ann", "prem_ann", "dern_ann", "comment")
  return(y)
}  

#Cête-Nord
cn <- NULL
#2023
x <- str_bio(bio_cn_2023)
str(x)
str(bio_cn_2023)
x[,c(3,5,9:15)] <- bio_cn_2023
x$annee <- 2023
x$espece <- "I"
cn <- rbind(x,cn)

#2022
x <- str_bio(bio_cn_2022)
str(x)
str(bio_cn_2022)
x[,c(3:4,9,5,10:15,20)] <- bio_cn_2022[,-11]
x <- x[-1,]
x$annee <- 2022
x$espece <- "I"
x[x$no %in% bio_cn_2022_placo$no, c(4,9,5,10:15)]<- bio_cn_2022_placo[,-c(1,11:12)]
x$espece[x$no %in% bio_cn_2022_placo$no] <- "G"
x$sexe <- format_CAPS(x$sexe)
x$s_zone[x$s_zone=="Extérieur"] <- "Ext"
x$s_zone[x$s_zone=="Intérieur"] <- "Int"
cn <- rbind(x,cn)

#2019
x <- str_bio(bio_cn_2019)
str(x)
str(bio_cn_2019)
x[,c(3:4,9,5,10:15,20)] <- bio_cn_2019[,-11]
x <- x[-1,]
x$annee <- 2019
x$espece <- "I"
x$s_zone[x$s_zone=="Extérieur"] <- "Ext"
x$s_zone[x$s_zone=="Intérieur"] <- "Int"
cn <- rbind(x,cn)

#2018
x <- str_bio(bio_cn_2018_16E)
str(x)
str(bio_cn_2018_16E)
x[,c(3,2,5,9:15,20,6)] <- bio_cn_2018_16E
x <- x[-1,]
x$annee <- 2018
x$espece <- "I"
cn <- rbind(x, cn)
x <- str_bio(bio_cn_2018_16F)
str(x)
str(bio_cn_2018_16F)
x[,c(3,2,5,9:15,20,6)] <- bio_cn_2018_16F
x <- x[-1,]
x$annee <- 2018
x$espece <- "I"
cn <- rbind(x, cn)

#2016
bio_cn_2016_16E <- bio_cn_2016_16E[!is.na(bio_cn_2016_16E$no),]
x <- str_bio(bio_cn_2016_16E)
str(x)
str(bio_cn_2016_16E)
x[-c(1),3:4] <- data.frame(matrix(c(unlist(strsplit(bio_cn_2016_16E$Lieu[-1], split = " "))), ,2, byrow = T))
x[,c(2,5,9:15,20,6)] <- bio_cn_2016_16E[,-1]
x <- x[-1,]
x$annee <- 2016
x$espece <- "I"
cn <- rbind(x, cn)
bio_cn_2016_16F <- bio_cn_2016_16F[!is.na(bio_cn_2016_16F$no),]
x <- str_bio(bio_cn_2016_16F)
str(x)
str(bio_cn_2016_16F)
x[,c(3,2,5,9:15,20,6)] <- bio_cn_2016_16F
x <- x[-1,]
x$annee <- 2016
x$espece <- "I"
cn <- rbind(x, cn)

#2014
x <- str_bio(bio_cn_2014_16E)
str(x)
str(bio_cn_2014_16E)
x[-c(1),3:4] <- data.frame(matrix(c(unlist(strsplit(bio_cn_2014_16E$Lieu[-1], split = " "))), ,2, byrow = T))
x[,c(2,9,5,10:15,20,17,19,6)] <- bio_cn_2014_16E[,-1]
x <- x[-1,]
x$annee <- 2014
x$espece <- "I"
cn <- rbind(x, cn)
x <- str_bio(bio_cn_2014_16F)
str(x)
str(bio_cn_2014_16F)
x[,c(3,2,9,5,10:15,20,6)] <- bio_cn_2014_16F[,-c(12,14)]
x <- x[-1,]
x$annee <- 2014
x$espece <- "I"
cn <- rbind(x, cn)

#2012
!is.na(bio_cn_2012_16E$no)
x <- str_bio(bio_cn_2012_16E)
str(x)
str(bio_cn_2012_16E)
x[,c(3,2,9,5,10:15,20)] <- bio_cn_2012_16E
x <- x[-1,]
x$annee <- 2012
x$espece <- "I"
cn <- rbind(x, cn)
!is.na(bio_cn_2012_16F$no)
x <- str_bio(bio_cn_2012_16F)
str(x)
str(bio_cn_2012_16F)
x[,c(3,2,9,5,10:15,20)] <- bio_cn_2012_16F
x <- x[-1,]
x$annee <- 2012
x$espece <- "I"
cn <- rbind(x, cn)


#2010
bio_cn_2010_16E <- bio_cn_2010_16E[!is.na(bio_cn_2010_16E$NO),]
x <- str_bio(bio_cn_2010_16E)
str(x)
str(bio_cn_2010_16E)
x[,6] <- data.frame(matrix(c(paste(bio_cn_2010_16E$STATION,bio_cn_2010_16E$F2, sep ="")), ,1, byrow = T))
x[,2] <- as.Date(bio_cn_2010_16E$DATE, format = "%d/%m/%Y")
x$date <- as.character(x$date)
x[,c(7,4,8,9,15,10,12,16,19,13,11,14,5)] <- bio_cn_2010_16E[,-c(1:3,5,7,19,20,21,22)]
x[,3] <- "16E"
x$annee <- 2010
x$sexe <- format_CAPS(x$sexe)
x$s_zone[x$s_zone=="EXT"] <- "Ext"
x$s_zone[x$s_zone=="INT"] <- "Int"
cn <- rbind(x, cn)
bio_cn_2010_16F <- bio_cn_2010_16F[!is.na(bio_cn_2010_16F$NO),]
x <- str_bio(bio_cn_2010_16F)
str(x)
str(bio_cn_2010_16F)
x[,6] <- data.frame(matrix(c(paste(bio_cn_2010_16F$STATION,bio_cn_2010_16F$F2, sep ="")), ,1, byrow = T))
x[,2] <- as.Date(bio_cn_2010_16F$DATE, format = "%d/%m/%Y")
x$date <- as.character(x$date)
x[,c(7,3,8,9,15,10,12,16,19,13,11,14,5)] <- bio_cn_2010_16F[,-c(1:3,5,7)]
x$annee <- 2010
x$sexe <- format_CAPS(x$sexe)
cn <- rbind(x, cn)

#2008
bio_cn_2008_16E <- bio_cn_2008_16E[!is.na(bio_cn_2008_16E$`#`),]
x <- str_bio(bio_cn_2008_16E)
str(x)
str(bio_cn_2008_16E)
x[,c(5,9:12,15:20)] <- bio_cn_2008_16E[,-c(12:15)]
x$annee <- 2008
x$espece <-  "I"
cn <- rbind(x, cn)
bio_cn_2008_16F <- bio_cn_2008_16F[!is.na(bio_cn_2008_16F$`#`),]
x <- str_bio(bio_cn_2008_16F)
str(x)
str(bio_cn_2008_16F)
x[,c(5,9:12,15)] <- bio_cn_2008_16F
x$sexe <- format_CAPS(x$sexe)
x$annee <- 2008
x$espece <- "I"
cn <- rbind(x, cn)

#2007
bio_cn_2007 <- bio_cn_2007[!is.na(bio_cn_2007$NO),]
x <- str_bio(bio_cn_2007)
str(x)
str(bio_cn_2007)
x$annee <- 2007
x[,6] <- data.frame(matrix(c(paste(bio_cn_2007$STATION,bio_cn_2007$F2, sep ="")), ,1, byrow = T))
x[,2] <- as.Date(bio_cn_2007$DATE, format = "%d-%m-%Y")
x$date <- as.character(x$date)
x[,c(7,4,8:9,15,10,12,16,19,13,11,14,17,18,20)] <- bio_cn_2007[,-c(1:3,5,7,18,21)]
x$s_zone[x$s_zone=="EXT"] <- "Ext"
x$s_zone[x$s_zone=="INT"] <- "Int"
cn <- rbind(x, cn)

#2005
x <- str_bio(bio_cn_2005)
str(x)
str(bio_cn_2005)
x$annee <- 2005
x[,6] <- data.frame(matrix(c(paste(bio_cn_2005$S,bio_cn_2005$TATION, sep ="")), ,1, byrow = T))
x[,c(3,7,2,8:20)] <- bio_cn_2005[,-c(2:4,20)]
x$s_zone[x$s_zone=="EXT"] <- "Ext"
x$s_zone[x$s_zone=="INT"] <- "Int"
x$date <- as.character(x$date)
cn <- rbind(x, cn)

#2004
x <- str_bio(bio_cn_2004_16E)
str(x)
str(bio_cn_2004_16E)
x$annee <- 2004
x[,6] <- data.frame(matrix(c(paste(bio_cn_2004_16E$S,bio_cn_2004_16E$TATION, sep ="")), ,1, byrow = T))
x[,c(3,7,2,8:20)] <- bio_cn_2004_16E[,-c(2:4,20)]
x$s_zone[x$s_zone=="EXT"] <- "Ext"
x$s_zone[x$s_zone=="INT"] <- "Int"
cn <- rbind(x, cn)
bio_cn_2004_16F <- bio_cn_2004_16F[!is.na(bio_cn_2004_16F$NO),]
x <- str_bio(bio_cn_2004_16F)
str(x)
str(bio_cn_2004_16F)
x$annee <- 2004
x[,2] <- as.Date(bio_cn_2004_16F$DATE, format = "%d-%m-%Y")
x$date <- as.character(x$date)
x[,c(3,6,8:20)] <- bio_cn_2004_16F[,-c(3:5,19)]
x$panier = 1
cn <- rbind(x, cn)

#2003
x <- str_bio(bio_cn_2003)
str(x)
str(bio_cn_2003)
x$annee <- 2003
x[,2] <- as.Date(bio_cn_2003$DATE, format = "%d-%m-%Y")
x$date <- as.character(x$date)
x[,c(6:7,4,8:9,15,10,12,16,19,13,11,14)] <- bio_cn_2003[,-c(2,4,6)]
x$pds_gon <- as.numeric(x$pds_gon)
x$pds_visc <- as.numeric(x$pds_visc)
x$s_zone[x$s_zone=="EXT"] <- "Ext"
x$s_zone[x$s_zone=="INT"] <- "Int"
cn <- rbind(x, cn)

#2001
x <- str_bio(bio_cn_2001)
str(x)
str(bio_cn_2001)
x$annee <- 2001
x$date <- as.Date(bio_cn_2001$DATE, format = "%d-%m-%Y")
x$date <- as.character(x$date)
x[,c(4,6,8:12,16)] <- bio_cn_2001[,-c(3:5)]
x$s_zone[x$s_zone=="EXT"] <- "Ext"
x$s_zone[x$s_zone=="INT"] <- "Int"
cn <- rbind(x, cn)

save(cn, file = "Bio_cote-nord.Rdata")



####

#IdM

#2022
idm <- NULL
x <- str_bio(bio_IDM_2022)
str(x)  
str(bio_IDM_2022)
x$annee <- 2022
x[,c(4,5,9:15,8,20)] <- bio_IDM_2022[,-c(12,13)]
x$sexe <- format_CAPS(x$sexe)
idm <- x

#2021
x <- str_bio(bio_IDM_2021)
str(x)
str(bio_IDM_2021)
x$annee <- 2021
x[,c(4,9,8,10:12,15,13,7)] <- bio_IDM_2021[,-c(1,2,11)]
idm <- rbind(x, idm)

#2019
bio_IDM_2019 <- bio_IDM_2019[-1,]
x <- str_bio(bio_IDM_2019)
str(x)
str(bio_IDM_2019)
x$annee <- 2019
x[,c(9,5,8,10:15,20)] <- bio_IDM_2019
idm <- rbind(x, idm)

#2017
y <- bio_IDM_2017
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2017
x[,2] <- as.Date(y$DATE, format = "%d-%m-%Y")
x$date <- as.character(x$date)
x[,c(6,7,4,8,9,15,10,12,17,19,13,11,14,5)] <- y[,-c(2,4,6)]
idm <- rbind(x, idm)

#2015
y <- bio_IDM_2015[!is.na(bio_IDM_2015$NO), -c(18:22)]
x <- str_bio(y)
x$annee <- 2015
x[,2] <- as.Date(y$DATE, format = "%d-%m-%Y")
x$date <- as.character(x$date)
x[,c(6,7,4,8,9,15,10,12,17,19,13,11,14,5)] <- y[,-c(2,4,6)]
idm <- rbind(x, idm)

#2013
y <- bio_IDM_2013
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2013
x[,c(4:6,7:20)] <- y[,-c(4,18,19)]
idm <- rbind(x, idm)

#2011
y <- bio_IDM_2011[-c(1,2),]
colnames(y) <- c("Station", "Trait", "Espece", "NO", "Taille", "Pds_vif", "pds_musc", "pds_gon", "pds_visc", "sexe", "age", "rgs", "zone")
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2011
x[,c(6,5,8:16,4)] <- y[,-12]
idm <- rbind(x, idm)

#2009
y <- bio_IDM_2009[!is.na(bio_IDM_2009$NO),]
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2009
x[,4:20] <- y[,-c(4,18:19)]
idm <- rbind(x, idm)

#2008
y <- bio_IDM_2008[!is.na(bio_IDM_2008$NO),]
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2008
x[,c(4,6:20)] <- y[,-c(3,17:18)]
x$sexe <- format_CAPS(x$sexe)
idm <- rbind(x, idm)

#2007
y <- bio_IDM_2007[,-18]
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2007
x[,c(4,6:19)] <- y[,-c(3,17)]
idm <- rbind(x, idm)

#2005
y <- bio_IDM_2005[!is.na(bio_IDM_2005$NO),]
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2005
x[,c(4,6:20)] <- y[,-c(2,4,6,15,21)]
idm <- rbind(x, idm)

#2004
y <- bio_IDM_2004
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2004
x[,2] <- as.Date(y$DATE, format = "%d-%m-%Y")
x$date <- as.character(x$date)
x[,c(6,7,4,8,9,15,10,12,16,17,13,11,14)] <- y[,-c(2,4,6)]
idm <- rbind(x, idm)

#2001
y <- bio_IDM_2001
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2001
x[,2] <- as.Date(y$DATE, format = "%d-%m-%Y")
x$date <- as.character(x$date)
x[,c(6,7,4,8,9,15,10:14,16)] <- y[,-c(2,5,15,16)]
idm <- rbind(x, idm)

#2000
y <- bio_IDM_2000
x <- str_bio(y)
str(x)
str(y)
x$annee <- 2000
x[,c(6,8:12,15,13,14)] <- y[,-1]
idm <- rbind(x, idm)

#1999
y <- bio_IDM_1999
x <- str_bio(y)
str(x)
str(y)
x$annee <- 1999
x[,2] <- as.Date(y$DATE, format = "%d-%m-%y")
x$date <- as.character(x$date)
x[,c(6,8:16)] <- y[,-c(1,2,4,5,15,16)]
x$espece[x$espece==1] <- "G"
x$espece[x$espece==2] <- "I"
idm <- rbind(x, idm)

#1998
y <- bio_IDM_1998[-c(1,2),]
colnames(y) <- c("Date", "Gisement", "Type", "Station", "Panier", "Espece", "statut", "no", "taille", "pds_vif", "pds_musc", "pds_gon", "pds_visc", "sexe")
x <- str_bio(y)
str(x)
str(y)
x$annee <- 1998
x[,2] <- as.Date(y$Date, format = "%Y-%m-%d")
x$date <- as.character(x$date)
x[,c(6:8,9:15)] <- y[,-c(1:3,7)]
for (i in 1:ncol(x)) x[x[,i]=="." & !is.na(x[,i]),i] <- NA
x <- type.convert(x, as.is = T)
idm <- rbind(x, idm)

save(idm, file = "Bio_IdM.Rdata")
