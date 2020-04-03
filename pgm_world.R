library(utils)
library(httr)
library(sf)
library(cartography)
library(animation)
library(tidyverse)

# Récuparation donnée
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
covid_world <- read.csv(tf)

# Suppression NA
covid_world <- covid_world %>% drop_na(popData2018)

# fond de carte
WORLD <- st_read("data/FDC/World_242_units_Eckert_IV.shp", stringsAsFactors = FALSE)
WORLD2 <- st_read("data/FDC/World_242_units_Eckert_IV_zoom.shp", stringsAsFactors = FALSE)
Zoom_box <- st_read("data/FDC/zoomBox.shp", stringsAsFactors = FALSE)
UE_Mask <- st_read("data/FDC/UE_Mask.shp", stringsAsFactors = FALSE)

# Change date format
covid_world$dateRep <- as.Date(covid_world$dateRep, "%d/%m/%Y")

# Sequence temporelle
min_day <- min(as.Date(covid_world$dateRep, "%d/%m/%Y"))
max_day <- max(as.Date(covid_world$dateRep, "%d/%m/%Y"))
cal <- seq(min_day, max_day, by="days")


# Liste pays
List_cc <- unique(covid_world$countryterritoryCode)
cal2 <- as.data.frame(cal)
Full_tab <- do.call("rbind", replicate(length(List_cc),cal2,  simplify = FALSE))
Full_tab$cc <- rep(List_cc, each=length(cal))


# Create ID
Full_tab$ID <- paste0(Full_tab$cc  ,Full_tab$cal)
covid_world$ID <- paste0(covid_world$countryterritoryCode  ,covid_world$dateRep)

# Jointure full_table
covid_world <- merge(Full_tab[, c("ID", "cal", "cc")], covid_world, by="ID", all.x=TRUE )
unique_tab <- unique(covid_world[, c("countryterritoryCode" , "popData2018") ])
# Ajout pop 2018 Eritrea
unique_tab[unique_tab$countryterritoryCode %in% "ERI", "popData2018" ] <- 5187948
# Supression NA
unique_tab <- unique_tab[!is.na(unique_tab$popData2018),  ]
covid_world <- merge(covid_world, unique_tab, by.x="cc", by.y="countryterritoryCode", all.x=TRUE )
covid_world <- covid_world[!is.na(covid_world$popData2018.y),  ]


# Remplissage full table
for (i in 1:nrow(covid_world)){
if (is.na(covid_world$cases[i]) & is.na(covid_world$deaths[i])) {covid_world$cases[i] <- 0
                                                              covid_world$deaths[i] <- 0}}
    

# Order data
covid_world <- covid_world[order(covid_world$cc, covid_world$cal),]


# Calcul cumsum
covid_world <- covid_world %>%
  separate(cal,c("day", "year","month"),remove = F) %>%
  group_by(cc)%>%
  mutate(cumsum_cas=cumsum(cases))

covid_world <- covid_world %>%
  separate(cal,c("day", "year","month"),remove = F) %>%
  group_by(cc)%>%
  mutate(cumsum_dc=cumsum(deaths))


# Calcul Taux
covid_world$nb_cas_M <- covid_world$cumsum_cas / covid_world$popData2018.y *1000000
covid_world$nb_dc_M <- covid_world$cumsum_dc / covid_world$popData2018.y *1000000


# Calcul max 
max_cas <- max(covid_world$cumsum_cas)
max_dc <- max(covid_world$cumsum_dc)


# Discretisation
discr <- covid_world[as.Date(covid_world$cal) %in%  cal[length(cal)], ]
discr <- as.data.frame(discr)
# discr <- discr %>% st_set_geometry(NULL)
breaks_cas <- getBreaks(v = discr[, "nb_cas_M"], method = "fisher-jenks")
breaks_dc <- getBreaks(v = discr[, "nb_dc_M"],  method = "fisher-jenks")

# set date in french
Sys.setlocale('LC_TIME', "French_France")

# Carte animée des CAS
graph.ani <- function() {
for (i in 1:length(cal)){


temp <- covid_world[as.Date(covid_world$cal) %in%  cal[i], ]

temp <- merge(WORLD2, temp, by.x="CODE_ISO3", by.y="cc")


cc_cas <- temp[temp$cumsum_cas>0,]

newdate <- format(cal[i], "%d %B %Y")

par(mar=c(0.5,0.5,1.2,0.5))
plot(st_geometry(WORLD2), col = "grey85", border = "grey75", lwd=0.1)
layoutLayer(title = "Nombre de cas confirmés officiels cumulés",
            author = "Auteur : Hugues Pecout (CNRS, FR CIST)",
            sources = "Source : European Centre for Disease Prevention and Control",
            theme = "sand.pal", scale = 3000,tabtitle=TRUE, bg = "grey90")
plot(st_geometry(WORLD2), lwd=0.1, col= "grey85", border = "grey65", add=TRUE)
plot(st_geometry(cc_cas), lwd=0.1, col= "grey65", border = "grey55", add=TRUE)
plot(Zoom_box , lwd = 0.3, col=NA, add=TRUE)
propSymbolsChoroLayer(temp, 
                      var = "cumsum_cas", 
                      var2 = "nb_cas_M", 
                      fixmax= max_cas,
                      inches = 0.3,
                      symbols = "circle", 
                      breaks = breaks_cas,
                      col = carto.pal(pal1 = "sand.pal", n1 = length(breaks_cas)),
                      legend.var.pos = c(-14837116, 1255491),
                      legend.var2.pos =  c(-17741861, 1380988),
                      legend.var2.values.rnd = 0,
                      border="black",
                      lwd=0.2, colNA = NA,
                      legend.var.title.txt = "Nombre de cas",
                      legend.var2.title.txt = "Nombre de cas pour \n 1M d'habitants", 
                      add = TRUE)
plot(UE_Mask, lwd = 0.3,  col=NA, add=TRUE)
text(-851330.7, 9850732, newdate, font=4, cex=1.4)
legend(x=343751.7, y=-5748705, "Pays touchés", fill="grey55", bty = "n", cex=1)



} 
}

unlink("Map_world_covid19_cases.gif")
ani.options(ani.width=800)
saveGIF(graph.ani(), interval = 0.9,  cmd.fun = system,  movie.name = "Map_world_covid19_cases.gif")


# Carte animée des DEATHS
graph.ani_2 <- function() {
  for (i in 12:length(cal)){


    temp <- covid_world[as.Date(covid_world$cal) %in%  cal[i], ]
    temp <- merge(WORLD2, temp, by.x="CODE_ISO3", by.y="cc")
    
    
    cc_dc <- temp[temp$cumsum_dc>0,]

    newdate <- format(cal[i], "%d %B %Y")
    
    par(mar=c(0.5,0.5,1.2,0.5))
    plot(st_geometry(WORLD2), col = "grey85", border = "grey75", lwd=0.1)

    layoutLayer(title = "Nombre de décès officiels cumulés",
                author = "Auteur : Hugues Pecout (CNRS, FR CIST)",
                sources = "Source : European Centre for Disease Prevention and Control",
                theme = "red.pal", scale = 3000,tabtitle=TRUE, bg = "grey90")
    
    plot(st_geometry(WORLD2), lwd=0.1, col= "grey85", border = "grey65", add=TRUE)
    plot(st_geometry(cc_dc), lwd=0.1, col= "grey65", border = "grey55", add=TRUE)
    plot(Zoom_box , lwd = 0.3, col=NA, add=TRUE)
    propSymbolsChoroLayer(temp, 
                          var = "cumsum_dc", 
                          var2 = "nb_dc_M", 
                          fixmax= max_dc,
                          inches = 0.3,
                          symbols = "circle", 
                          breaks = breaks_dc,
                          col = carto.pal(pal1 = "red.pal", n1 = length(breaks_dc)),
                          legend.var.pos = c(-14837116, 1255491),
                          legend.var2.pos =  c(-17741861, 1380988),
                          legend.var2.values.rnd = 0,
                          border="black",
                          lwd=0.2, colNA = NA,
                          legend.var.title.txt = "Nombre de décès",
                          legend.var2.title.txt = "Nombre de décès \n pour 1M d'habitants", 
                          add = TRUE)
    plot(UE_Mask, lwd = 0.3,  col=NA, add=TRUE)
    text(-851330.7, 9850732, newdate, font=4, cex=1.4)
    legend(x=343751.7, y=-5748705, "Pays touchés", fill="grey55", bty = "n", cex=1)
    
    
    
  } 
}

unlink("Map_world_covid19_deaths.gif")
ani.options(ani.width=800)
saveGIF(graph.ani_2(), interval = 0.9,  cmd.fun = system,  movie.name = "Map_world_covid19_deaths.gif")

# set date in english
Sys.setlocale("LC_TIME", "English")

# Carte animée des CASES - EN
graph.ani <- function() {
  for (i in 1:length(cal)){
    
    
    temp <- covid_world[as.Date(covid_world$cal) %in%  cal[i], ]
    
    temp <- merge(WORLD2, temp, by.x="CODE_ISO3", by.y="cc")
    
    
    cc_cas <- temp[temp$cumsum_cas>0,]
    
    newdate <- format(cal[i], "%d %B %Y")
    
    par(mar=c(0.5,0.5,1.2,0.5))
    plot(st_geometry(WORLD2), col = "grey85", border = "grey75", lwd=0.1)
    layoutLayer(title = "Cumulative number of official confirmed cases",
                author = "Author: Hugues Pecout (CNRS, FR CIST)",
                sources = "Source : European Centre for Disease Prevention and Control",
                theme = "sand.pal", scale = 3000,tabtitle=TRUE, bg = "grey90")
    plot(st_geometry(WORLD2), lwd=0.1, col= "grey85", border = "grey65", add=TRUE)
    plot(st_geometry(cc_cas), lwd=0.1, col= "grey65", border = "grey55", add=TRUE)
    plot(Zoom_box , lwd = 0.3, col=NA, add=TRUE)
    propSymbolsChoroLayer(temp, 
                          var = "cumsum_cas", 
                          var2 = "nb_cas_M", 
                          fixmax= max_cas,
                          inches = 0.3,
                          symbols = "circle", 
                          breaks = breaks_cas,
                          col = carto.pal(pal1 = "sand.pal", n1 = length(breaks_cas)),
                          legend.var.pos = c(-14837116, 1255491),
                          legend.var2.pos =  c(-17741861, 1380988),
                          legend.var2.values.rnd = 0,
                          border="black",
                          lwd=0.2, colNA = NA,
                          legend.var.title.txt = "Number of cases",
                          legend.var2.title.txt = "Number of cases \n per 1M inhabitants", 
                          add = TRUE)
    plot(UE_Mask, lwd = 0.3,  col=NA, add=TRUE)
    text(-851330.7, 9850732, newdate, font=4, cex=1.4)
    legend(x=343751.7, y=-5748705, "Affected countries", fill="grey55", bty = "n", cex=1)
    
    
    
  } 
}

unlink("World_map_spread_cases_covid19.gif")
ani.options(ani.width=800)
saveGIF(graph.ani(), interval = 0.9,  cmd.fun = system,  movie.name = "World_map_spread_cases_covid19.gif")


# Carte animée des DEATH - EN
graph.ani_2 <- function() {
  for (i in 12:length(cal)){
    
    
    temp <- covid_world[as.Date(covid_world$cal) %in%  cal[i], ]
    temp <- merge(WORLD2, temp, by.x="CODE_ISO3", by.y="cc")
    
    
    cc_dc <- temp[temp$cumsum_dc>0,]
    
    newdate <- format(cal[i], "%d %B %Y")
    
    par(mar=c(0.5,0.5,1.2,0.5))
    plot(st_geometry(WORLD2), col = "grey85", border = "grey75", lwd=0.1)
    
    layoutLayer(title = "Cumulative number of official deaths",
                author = "Author: Hugues Pecout (CNRS, FR CIST)",
                sources = "Source: European Centre for Disease Prevention and Control",
                theme = "red.pal", scale = 3000,tabtitle=TRUE, bg = "grey90")
    
    plot(st_geometry(WORLD2), lwd=0.1, col= "grey85", border = "grey65", add=TRUE)
    plot(st_geometry(cc_dc), lwd=0.1, col= "grey65", border = "grey55", add=TRUE)
    plot(Zoom_box , lwd = 0.3, col=NA, add=TRUE)
    propSymbolsChoroLayer(temp, 
                          var = "cumsum_dc", 
                          var2 = "nb_dc_M", 
                          fixmax= max_dc,
                          inches = 0.3,
                          symbols = "circle", 
                          breaks = breaks_dc,
                          col = carto.pal(pal1 = "red.pal", n1 = length(breaks_dc)),
                          legend.var.pos = c(-14837116, 1255491),
                          legend.var2.pos =  c(-17741861, 1380988),
                          legend.var2.values.rnd = 0,
                          border="black",
                          lwd=0.2, colNA = NA,
                          legend.var.title.txt = "Number of deaths",
                          legend.var2.title.txt = "Number of deaths \n per 1M inhabitants", 
                          add = TRUE)
    plot(UE_Mask, lwd = 0.3,  col=NA, add=TRUE)
    text(-851330.7, 9850732, newdate, font=4, cex=1.4)
    legend(x=343751.7, y=-5748705, "Affected countries", fill="grey55", bty = "n", cex=1)
    
    
    
  } 
}

unlink("World_map_spread_deaths_covid19.gif")
ani.options(ani.width=800)
saveGIF(graph.ani_2(), interval = 0.9,  cmd.fun = system,  movie.name = "World_map_spread_deaths_covid19.gif")




