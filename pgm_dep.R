# Download data from data gouv
download.file("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7", 
              destfile = "data_dep/covid19.csv",  quiet = FALSE, mode = "w",
              cacheOK = TRUE)

# Import data
library(readr)
covid_data <- read.csv("data_dep/covid19.csv", sep=";", stringsAsFactors = FALSE)
library(sf)
fr <- st_read("data_dep/DEPARTEMENT_CARTO.shp")

# Day recovering
jour <- unique(covid_data$jour)



max_hosp <- max(covid_data$hosp)
max_dc <- max(covid_data$dc)
max_rea <- max(covid_data$rea)
max_rad <- max(covid_data$rad)

# Construction carte animée hospitalisation
library(cartography)
jpeg("example%03d.png")
for (i in 1:length(jour)){
  
temp <- merge(fr, covid_data[covid_data$jour == jour[i] & covid_data$sexe == 0 ,], by.x="INSEE_DEP", by.y="dep")
  par(mar=c(0,0,1.2,0))
  plot(st_geometry(temp), col= NA, border = NA)
  layoutLayer(title="Nombre de cas confirmés du Coronavirus en France métroplitaine", 
              sources = "Sources: Sante publique france", 
              scale = NULL, tabtitle = TRUE, frame = TRUE, bg = "grey60")
  plot(st_geometry(temp), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
  propSymbolsLayer(x = temp,var = "hosp", inches = 0.3,fixmax = max_hosp,
                   border = "white", lwd = 0.2, 
                   col = "darkorange",
                   legend.title.txt = "",
                   legend.pos = c(128011.7, 6993214))
  text(1042084, 7059133, temp$jour[1], font=4, cex=1)
  layoutLayer(title="Nombre de patients hospitalisés (covid19)", 
              sources = "Sources: Sante publique france", 
              scale = NULL, tabtitle = TRUE, frame = TRUE)
}
dev.off()
system("magick -delay 100 *.png nb_hospitalisation_Sante_pub_fr.gif")
file.remove(list.files(pattern=".png"))



# Construction carte animée reanimation
jpeg("example%03d.png")
for (i in 1:length(jour)){
  
  temp <- merge(fr, covid_data[covid_data$jour == jour[i] & covid_data$sexe == 0 ,], by.x="INSEE_DEP", by.y="dep")
  par(mar=c(0,0,1.2,0))
  plot(st_geometry(temp), col= NA, border = NA)
  layoutLayer(title="Nombre de cas confirmés du Coronavirus en France métroplitaine", 
              sources = "Sources: Sante publique france", 
              scale = NULL, tabtitle = TRUE, frame = TRUE, bg = "grey60")
  plot(st_geometry(temp), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
  propSymbolsLayer(x = temp,var = "rea", inches = 0.3,fixmax = max_rea,
                   border = "white", lwd = 0.2,col = "red",
                   legend.title.txt = "",
                   legend.pos = c(128011.7, 6993214))
  text(1042084, 7059133, temp$jour[1], font=4, cex=1)
  layoutLayer(title="Nombre de personnes en réanimation ou soins intensifs (covid19)", 
              sources = "Sources: Sante publique france", 
              scale = NULL, tabtitle = TRUE, frame = TRUE)
}
dev.off()
system("magick -delay 100 *.png nb_reanimation_Sante_pub_fr.gif")
file.remove(list.files(pattern=".png"))

# Construction carte animée retour à domicile
jpeg("example%03d.png")
for (i in 1:length(jour)){
  
  temp <- merge(fr, covid_data[covid_data$jour == jour[i] & covid_data$sexe == 0 ,], by.x="INSEE_DEP", by.y="dep")
  par(mar=c(0,0,1.2,0))
  plot(st_geometry(temp), col= NA, border = NA)
  layoutLayer(title="Nombre de cas confirmés du Coronavirus en France métroplitaine", 
              sources = "Sources: Sante publique france", 
              scale = NULL, tabtitle = TRUE, frame = TRUE, bg = "grey60")
  plot(st_geometry(temp), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
  propSymbolsLayer(x = temp,var = "rad", inches = 0.3,fixmax = max_rad,
                   border = "white", lwd = 0.2,col = "darkolivegreen",
                   legend.title.txt = "",
                   legend.pos = c(128011.7, 6993214))
  text(1042084, 7059133, temp$jour[1], font=4, cex=1)
  layoutLayer(title="Nombre cumulé de personnes retournées à domicile (covid19)", 
              sources = "Sources: Sante publique france", 
              scale = NULL, tabtitle = TRUE, frame = TRUE)
}
dev.off()
system("magick -delay 100 *.png nb_retour_dom_Sante_pub_fr.gif")
file.remove(list.files(pattern=".png"))


# Construction carte animée décés
jpeg("example%03d.png")
for (i in 1:length(jour)){
  
  temp <- merge(fr, covid_data[covid_data$jour == jour[i] & covid_data$sexe == 0 ,], by.x="INSEE_DEP", by.y="dep")
  par(mar=c(0,0,1.2,0))
  plot(st_geometry(temp), col= NA, border = NA)
  layoutLayer(title="Nombre de cas confirmés du Coronavirus en France métroplitaine", 
              sources = "Sources: Sante publique france", 
              scale = NULL, tabtitle = TRUE, frame = TRUE, bg = "grey60")
  plot(st_geometry(temp), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
  propSymbolsLayer(x = temp,var = "dc", inches = 0.3,fixmax = max_dc,
                   border = "white", lwd = 0.2,col = "grey20",
                   legend.title.txt = "",
                   legend.pos = c(128011.7, 6993214))
  text(1042084, 7059133, temp$jour[1], font=4, cex=1)
  layoutLayer(title="Nombre cumulé de personnes décédées (covid19)", 
              sources = "Sources: Sante publique france", 
              scale = NULL, tabtitle = TRUE, frame = TRUE)
}
dev.off()
system("magick -delay 100 *.png nb_deces_Sante_pub_fr.gif")
file.remove(list.files(pattern=".png"))




#### CARTE CAS
# data -> https://www.data.gouv.fr/fr/datasets/r/cc6d6e18-dde9-4f83-901d-725ba5e25879

nbfiles <- length(list.files("data_dep/departements-france-polygons/"))
fr <- st_read("data_dep/departements-france-polygons/departements-france-polygons-2020-03-18.json")
FR_Metro <- fr[!(fr$Province.State %in% c("La Réunion",  "Mayotte", "Martinique", "Guadeloupe", "Guyane")),-c(1,3:9)]
FR_Metro2 <- FR_Metro %>% st_set_geometry(NULL)



for (i in 2:nbfiles){

  if (nchar(i)==1){
    link <- paste0("data_dep/departements-france-polygons/departements-france-polygons-2020-03-0", i, ".json")
    data <- st_read(link)} else {
      link <- paste0("data_dep/departements-france-polygons/departements-france-polygons-2020-03-", i, ".json")
      data <- st_read(link)}

data <- data[, c("Province.State", "Confirmed", "geometry")]
colnames(data) <- c("Province.State", paste0("confirmed_", i), "geometry")
FR_Metro2 <- merge(FR_Metro2, st_drop_geometry(data[, 1:2]), by="Province.State", all.x=TRUE)


 
if(i>2){ 
for (e in 1:nrow(FR_Metro2)){
if(is.na(FR_Metro2[e,  paste0("confirmed_", i)])){
FR_Metro2[e,  paste0("confirmed_", i)] <- FR_Metro2[e,  paste0("confirmed_", i-1)]
}}}


# max_dc <-  max(st_drop_geometry(FR_Metro[,  paste0("DEATH_", i)]), na.rm=T)
}
 
FR_Metro <- merge(FR_Metro, FR_Metro2, by="Province.State", all.x=TRUE)
max_cas <-  max(FR_Metro2[,  paste0("confirmed_", i)], na.rm=T)


# Construction carte cas
days <- c(2:nbfiles)
v <- 1
jpeg("example%03d.png")
for (y in 2:nbfiles){

  tab <- FR_Metro[,c(1, y)]
  jour <- colnames(FR_Metro[y])
  
  par(mar=c(0,0,1.2,0))
  plot(st_geometry(FR_Metro), col= NA, border = NA)
  layoutLayer(title="Nombre de cas confirmés du Coronavirus en France métroplitaine - (Données peu fiables)", 
              sources = "Sources: Kalisio", 
              scale = NULL, tabtitle = TRUE, frame = TRUE, bg = "grey60")
  plot(st_geometry(FR_Metro), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
  propSymbolsLayer(x = tab,var = jour[1], 
                   inches = 0.3,
                   fixmax =   max_cas ,
                   border = "white",
                   lwd = 0.2,
                   col= "goldenrod",
                   legend.title.txt = "",
                   legend.style ="e",
                   legend.pos = c(-4.796034,43.20121))

  text(7.351556, 50.24156, paste0(days[v], " mars"), font=4, cex=1)

  v <- v+1
  
}
dev.off()

system("magick -delay 100 *.png Nb_Cas_confimes_dep_kalisio.gif")
file.remove(list.files(pattern=".png"))




# Construction carte morts
FR_Metro <- fr[!(fr$Province.State %in% c("La Réunion", "Mayotte", "Martinique", "Guadeloupe", "Guyane")),-c(1,3:9)]
FR_Metro2 <- FR_Metro %>% st_set_geometry(NULL)


for (i in 5:nbfiles){
  
  if (nchar(i)==1){
    link <- paste0("data_dep/departements-france-polygons/departements-france-polygons-2020-03-0", i, ".json")
    data <- st_read(link)} else {
      link <- paste0("data_dep/departements-france-polygons/departements-france-polygons-2020-03-", i, ".json")
      data <- st_read(link)}
  
  data <- data[, c("Province.State", "Deaths", "geometry")]
  colnames(data) <- c("Province.State", paste0("deaths_", i), "geometry")
  FR_Metro2 <- merge(FR_Metro2, st_drop_geometry(data[, 1:2]), by="Province.State", all.x=TRUE)
  
  
  
  if(i>5){ 
    for (e in 1:nrow(FR_Metro2)){
      if(is.na(FR_Metro2[e,  paste0("deaths_", i)])){
        FR_Metro2[e,  paste0("deaths_", i)] <- FR_Metro2[e,  paste0("deaths_", i-1)]
      }}}
  
  
  # max_dc <-  max(st_drop_geometry(FR_Metro[,  paste0("DEATH_", i)]), na.rm=T)
}

FR_Metro <- merge(FR_Metro, FR_Metro2, by="Province.State", all.x=TRUE)
max_dc <-  max(FR_Metro2[,  paste0("deaths_", i)], na.rm=T)

nbfiles2 <-nbfiles-3
days <- c(5:nbfiles)
v <- 1
jpeg("example%03d.png")
for (y in 2:nbfiles2){
  tab <- FR_Metro[,c(1, y)]
  jour <- colnames(FR_Metro[y])
  
  par(mar=c(0,0,1.2,0))
  plot(st_geometry(FR_Metro), col= NA, border = NA)
  layoutLayer(title="Nombre de décés du Coronavirus en France métroplitaine - (Données peu fiables)", 
              sources = "Sources: kalisio", 
              scale = NULL, tabtitle = TRUE, frame = TRUE, bg = "grey60")
  plot(st_geometry(FR_Metro), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
  propSymbolsLayer(x = tab,var = jour[1], 
                   inches = 0.3,
                   fixmax =   max_dc ,
                   border = "white",
                   lwd = 0.2,
                   col= "grey20",
                   legend.title.txt = "",
                   legend.style ="e",
                   legend.pos = c(-4.796034,43.20121))
  
  text(7.351556, 50.24156, paste0(days[v], " mars"), font=4, cex=1)
  
  v <- v+1

}
dev.off()

system("magick -delay 100 *.png Nb_décés_dep_kalisio.gif")
file.remove(list.files(pattern=".png"))



