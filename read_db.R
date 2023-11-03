read.db <- function(file, sheet, ind=NULL, ord=NULL, asc = T){
  require(RODBC, quietly = T, warn.conflicts = F)
  ext <- unlist(strsplit(file, split = ".", fixed = T))[length(unlist(strsplit(file, split = ".", fixed = T)))]
  if (ext!="XLS" & ext!="xls" & ext!="xlsx" & ext!="xlsm" & ext!="xlsb" & ext!="mdb" & ext!="accdb") stop("File format not compatible")
  if (ext== "XLS" | ext=="xls" | ext=="xlsx" | ext=="xlsm" | ext=="xlsb"){
    db <- RODBC::odbcDriverConnect(paste("DRIVER=Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb); DBQ=", file, sep = ""))
    x <- RODBC::sqlFetch(db, sheet)
    RODBC::odbcClose(db)
  }
  if (ext =="mdb" | ext=="accdb"){
    if (asc==T) rl <- "ASC"
    if (asc==F) rl <- "DESC"
    if (is.null(ind)==T & is.null(ord)==T){ query <- paste("SELECT * FROM", sheet, sep = " ")
    }else if (is.null(ord)==T){ query <- paste("SELECT * FROM", sheet, "WHERE", paste(ind, collapse = " AND "), sep = " ")
    }else if (is.null(ind)==T){ query <- paste("SELECT * FROM", sheet, "ORDER BY", paste(ord, collapse = ", "), rl, sep = " ")
    }else if (is.null(ind)==F & is.null(ord)==F) query <- paste("SELECT * FROM", sheet, "WHERE", paste(ind, collapse = " AND "), "ORDER BY", paste(ord, collapse = ", "), rl, sep = " ")
    db <- RODBC::odbcDriverConnect(paste("Driver=Microsoft Access Driver (*.mdb, *.accdb); DBQ=", file, sep = ""))
    x <- RODBC::sqlQuery(db, query = query)
    RODBC::odbcClose(db)
  }
  return(x)
}

#file = chr : path to xls or accdb file 

#sheet = chr : sheet name on said database 

#ind = chr : a vector containing de variable name(s) and condition(s) used for indexation only works with access files. If not used, leave =NULL, same with xls file

#ord = chr : a vector containing variable name(s) used to order the data same property as ind regarding file and use

#asc = logic : should the ordering by ascending (TRUE) or descending (FALSE). 



#Exemple

#excel file
toto <- read.db(file = "S:/Petoncle/GestionPeches/liste_nbpc_petoncle.xlsx", sheet = "liste_nbpc")

#access database no indexation
toto <- read.db(file = "S:/Petoncle/Recherche/Mission/BaseDonnées/historique.mdb", "PROJET_MOLLUSQUE")

#with no indexation but with sorting
toto <- read.db(file = "S:/Petoncle/Recherche/Mission/BaseDonnées/historique.mdb", "PROJET_MOLLUSQUE", ord = "NO_RELEVE", asc = F)

#with indexation only
toto <- read.db(file = "S:/Petoncle/Recherche/Mission/BaseDonnées/historique.mdb", "PROJET_MOLLUSQUE", ind = c("COD_SOURCE_INFO=18", "ANNEE=2022"))

#indexation and sorting
toto <- read.db(file = "S:/Petoncle/Recherche/Mission/BaseDonnées/historique.mdb", "CAPTURE_MOLLUSQUE", "NO_RELEVE=16", c("IDENT_NO_TRAIT", "COD_ESP_GEN"), asc = T)
