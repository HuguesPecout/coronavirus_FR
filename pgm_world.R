library(readxl)
library(httr)
library(sf)
library(cartography)
library(animation)

# Récuparation donnée
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
covid_world <- read_excel(tf)

# fond de carte
WORLD <- st_read("data/FDC/World_242_units_Eckert_IV.shp", stringsAsFactors = FALSE)
WORLD2 <- st_read("data/FDC/World_242_units_Eckert_IV_zoom.shp", stringsAsFactors = FALSE)
Zoom_box <- st_read("data/FDC/zoomBox.shp", stringsAsFactors = FALSE)
UE_Mask <- st_read("data/FDC/UE_Mask.shp", stringsAsFactors = FALSE)

# Order data
covid_world <- covid_world[order(covid_world$countryterritoryCode, covid_world$dateRep),]

# Calcul cumsum
library(tidyverse)
covid_world <- covid_world %>%
  separate(dateRep,c("day", "year","month"),remove = F) %>%
  group_by(countryterritoryCode)%>%
  mutate(cumsum_cas=cumsum(cases))

covid_world <- covid_world %>%
  separate(dateRep,c("day", "year","month"),remove = F) %>%
  group_by(countryterritoryCode)%>%
  mutate(cumsum_dc=cumsum(deaths))

# PANSEMENT
# Ajout pop 2018 Eritrea
covid_world[covid_world$countryterritoryCode %in% "ERI", "popData2018" ] <- 5187948


# Calcul Taux
covid_world$nb_cas_M <- covid_world$cumsum_cas / covid_world$popData2018 *1000000
covid_world$nb_dc_M <- covid_world$cumsum_dc / covid_world$popData2018 *1000000

# Jointure
covid_world <- merge(WORLD2, covid_world, by.x = "CODE_ISO3", by.y = "countryterritoryCode", all.x = FALSE)

# Sequence temporelle
min_day <- as.Date(min(covid_world$dateRep))
max_day <- as.Date(max(covid_world$dateRep))
cal <- seq(min_day, max_day, by="days")

# Calcul max 
max_cas <- max(covid_world$cumsum_cas)
max_dc <- max(covid_world$cumsum_dc)


# Discretisation
discr <- covid_world[as.Date(covid_world$dateRep) %in%  cal[length(cal)], ]
discr <- discr %>% st_set_geometry(NULL)
breaks_cas <- getBreaks(v = discr[, "nb_cas_M"],  method = "msd")
breaks_dc <- getBreaks(v = discr[, "nb_dc_M"],  method = "msd")

# Carte animée des CAS
graph.ani <- function() {
for (i in 1:length(cal)){

temp <- covid_world[as.Date(covid_world$dateRep) %in%  cal[i], ]

newdate <- format(cal[i], "%d %B %Y")

par(mar=c(0.5,0.5,1.2,0.5))
plot(st_geometry(WORLD2), col = "grey85", border = "grey75", lwd=0.1)
layoutLayer(title = "Nombre de cas confirmés officiels",
            author = "",
            sources = "Source : European Centre for Disease Prevention and Control",
            theme = "sand.pal", scale = 3000,tabtitle=TRUE, bg = "grey65")
plot(st_geometry(WORLD2), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
plot(Zoom_box , lwd = 0.3, col=NA, add=TRUE)
propSymbolsChoroLayer(temp, 
                      var = "cumsum_cas", 
                      var2 = "nb_cas_M", 
                      fixmax= max_cas,
                      inches = 0.3,
                      symbols = "circle", 
                      breaks = breaks_cas,
                      col = carto.pal(pal1 = "sand.pal", n1 = length(breaks_cas)),
                      legend.var.pos = c(-14510629, 1332030),
                      legend.var2.pos =  c(-17741861, 1380988),
                      legend.var2.values.rnd = 0,
                      border="black",
                      lwd=0.2, colNA = NA,
                      legend.var.title.txt = "Nombre de cas",
                      legend.var2.title.txt = "Nombre pour  \n 1M d'hab.", 
                      add = TRUE)
plot(UE_Mask, lwd = 0.3,  col=NA, add=TRUE)
text(-851330.7, 9850732, newdate, font=4, cex=1.4)


} 
}

unlink("Map_world_covid19_cases.gif")
ani.options(ani.width=800)
saveGIF(graph.ani(), interval = 1,  cmd.fun = system,  movie.name = "Map_world_covid19_cases.gif")


# Carte animée des CAS
graph.ani_2 <- function() {
  for (i in 12:length(cal)){

    temp <- covid_world[as.Date(covid_world$dateRep) %in%  cal[i], ]

    newdate <- format(cal[i], "%d %B %Y")
    
    par(mar=c(0.5,0.5,1.2,0.5))
    plot(st_geometry(WORLD2), col = "grey85", border = "grey75", lwd=0.1)

    layoutLayer(title = "Nombre de décés officiels",
                author = "",
                sources = "Source : European Centre for Disease Prevention and Control",
                theme = "red.pal", scale = 3000,tabtitle=TRUE, bg = "grey65")
    plot(st_geometry(WORLD2), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
    plot(Zoom_box , lwd = 0.3, col=NA, add=TRUE)
    propSymbolsChoroLayer(temp, 
                          var = "cumsum_dc", 
                          var2 = "nb_dc_M", 
                          fixmax= max_dc,
                          inches = 0.3,
                          symbols = "circle", 
                          breaks = breaks_dc,
                          col = carto.pal(pal1 = "red.pal", n1 = length(breaks_dc)),
                          legend.var.pos = c(-14510629, 1332030),
                          legend.var2.pos =  c(-17741861, 1380988),
                          legend.var2.values.rnd = 0,
                          border="black",
                          lwd=0.2, colNA = NA,
                          legend.var.title.txt = "Nombre de décés",
                          legend.var2.title.txt = "Nombre pour \n 1M d'hab.", 
                          add = TRUE)
    plot(UE_Mask, lwd = 0.3,  col=NA, add=TRUE)
    text(-851330.7, 9850732, newdate, font=4, cex=1.4)
    
    
  } 
}

unlink("Map_world_covid19_deaths.gif")
ani.options(ani.width=800)
saveGIF(graph.ani_2(), interval = 1,  cmd.fun = system,  movie.name = "Map_world_covid19_deaths.gif")


