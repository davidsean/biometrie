
#########
# RESET workspace
#########
rm(list = ls()) #vider l'espace de travail
gc() #vider la mémoire vive
if (dir.exists("~/Temp")==F) dir.create("~/Temp")
setwd("~/Temp") #définir le répertoire de travail
require(data.table, quietly = T)


#########
# INPUT all stations sampled during surveys
# opens the historical DB and fetch all station names with their cod_strat
#########
require(RODBC, quietly = T, warn.conflicts = T)
src_fname <- "historique.mdb"
full_path <- paste("S:/Petoncle/Recherche/Mission/BaseDonnées/", src_fname, sep="")
db <- RODBC::odbcDriverConnect(paste("Driver=Microsoft Access Driver (*.mdb, *.accdb); DBQ=", full_path, sep=""))
query <- sprintf('SELECT NO_STATION, COD_STRATE  FROM TRAIT_MOLLUSQUE WHERE COD_SOURCE_INFO=19')
db_data <- RODBC::sqlQuery(db, query)
RODBC::odbcClose(db)

db_data


#########
# INPUT station names
# create a liste of all stations in secteur centre and ouest
# see S:\Petoncle\Recherche\Mission\Îles
########

##
# stations secteur ouest
src_fname <- "ST_ILES_OUEST_2011.csv"
full_path <- sprintf("S:/Petoncle/Recherche/Mission/Îles/%s", src_fname)
x <- read.csv(full_path)
all_stations <- x["station"]

##
# stations secteur centre
src_fname <- "ST_ILES_CENTRE_2015_Sans_les_400.csv"
full_path <- sprintf("S:/Petoncle/Recherche/Mission/Îles/%s", src_fname)
x <- read.csv(full_path)
colnames(x)[1] <- "station"
all_stations <- rbind(all_stations,x["station"])

all_stations



########
# PROCESS
#######

# IMPLEMENTATION INCOMPLETE


codes <- db_data[which(db_data$NO_STATION==533),2]

codes[1]==all.

db_data[which(db_data$NO_STATION==240),2]
all_stations

for (no_station in all_stations){
  
  print(no_station, db_data[which(db_data$NO_STATION==no_station),2])
}
