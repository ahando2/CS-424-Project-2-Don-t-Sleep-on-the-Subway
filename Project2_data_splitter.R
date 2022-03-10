setwd("D:/ahando2/classes/SPRING 2022/CS 424/Project/Project 2")
library(lubridate)

CTA_daily <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv",sep='\t',quote="", header = TRUE)
CTA_LatLon <- read.table(file = "CTA_-_System_Information_-_List_of__L__Stops.tsv",sep='\t',quote="", header = TRUE)

# change first column name from `ï..station_id` to `station_id`
colnames(CTA_daily)[1] <- "station_id"

# change first column name from `ï..station_id` to `station_id`
colnames(CTA_LatLon)[1] <- "STOP_ID"


# check if any of the data is missing
subset(CTA_daily , is.na(CTA_daily))

# format the date
CTA_daily <- CTA_daily[complete.cases(CTA_daily), ]
CTA_daily$date <- mdy(CTA_daily$date)

# get only columns station_id,stationname,date,rides
CTA_daily <- CTA_daily[,c("station_id","stationname","date","rides")]

# get only columns s"MAP_ID","STATION_NAME","Location"
CTA_LatLon <- CTA_LatLon[,c("MAP_ID","Location")]
colnames(CTA_LatLon)[1] <- "station_id"
CTA_LatLon$Location <- gsub('[( )]',"",as.character(CTA_LatLon$Location))

lat_lon<-unlist(strsplit(CTA_LatLon$Location,","))
CTA_LatLon$latitude <- lat_lon[c(TRUE, FALSE)]
CTA_LatLon$longtitude <- lat_lon[c(FALSE, TRUE)]
CTA_LatLon <- subset(CTA_LatLon,select= -c(Location))
CTA_LatLon$latitude <- as.numeric(CTA_LatLon$latitude)
CTA_LatLon$longtitude <- as.numeric(CTA_LatLon$longtitude)
CTA_LatLon <- unique(CTA_LatLon)

CTA_daily$latitude <- NA
CTA_daily$longtitude <- NA
CTA_LatLon$stationname <- NA

# check for duplicates of station_id
n_occur <- data.frame(table(CTA_LatLon$station_id))
CTA_LatLon[CTA_LatLon$station_id %in% n_occur$Var1[n_occur$Freq > 1],]
# duplicate exist for id 41400, remove the first row
CTA_LatLon<- CTA_LatLon[-c(which(CTA_LatLon$station_id == 41400)[1]),]

# merge location table with rides table
for ( station_id in CTA_LatLon$station_id){
  CTA_daily$latitude[which(CTA_daily$station_id == station_id)] <- CTA_LatLon$latitude[[which(CTA_LatLon$station_id== station_id)]]
  CTA_daily$longtitude[which(CTA_daily$station_id == station_id)] <- CTA_LatLon$longtitude[[which(CTA_LatLon$station_id== station_id)]]
  CTA_LatLon$stationname[which(CTA_LatLon$station_id == station_id)] <-  CTA_daily$stationname[which(CTA_daily$station_id == station_id)][1]
}


# check if any of the location data is missing
na_CTA_daily <- subset(CTA_daily , is.na(CTA_daily$latitude))
unique(na_CTA_daily$station_id)
unique(na_CTA_daily$stationname)
# "Randolph/Wabash"(40200)  "Madison/Wabash"(40640)   "Washington/State"(40500) "Homan"(41580) are the missing location
# 41.884431, -87.626149   41.882023, -87.626098   41.8837, -87.6278   41.884914, -87.711327
# location from Wikipedia
na_latitude <- c(41.884431,41.882023,41.8837,41.884914)
na_longtitude <- c(-87.626149,-87.626098,-87.6278,-87.711327)
na_id <- c(40200,40640,40500,41580)

for( i in (1:4)){
  CTA_daily$latitude[which(CTA_daily$station_id == na_id[i])] <- na_latitude[i]
  CTA_daily$longtitude[which(CTA_daily$station_id == na_id[i])] <- na_longtitude[i]
  CTA_LatLon <- rbind(CTA_LatLon, c(na_id[i],na_latitude[i],na_longtitude[i], CTA_daily$stationname[which(CTA_daily$station_id == na_id[i])][1])) 
}

# re-check if any of the location data is missing
subset(CTA_daily , is.na(CTA_daily))

# save to tsv files
write.table(CTA_daily, file=paste("SubwayLarge/CTA_daily", ".tsv", sep=""), quote=FALSE, sep='\t')
write.table(CTA_LatLon, file=paste("SubwayLarge/CTA_Location", ".tsv", sep=""), quote=FALSE, sep='\t')

