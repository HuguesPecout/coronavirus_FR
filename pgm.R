
# Download data from data gouv
download.file("https://www.data.gouv.fr/fr/datasets/r/fa9b8fc8-35d5-4e24-90eb-9abe586b0fa5", 
              destfile = "data/covid19.csv",  quiet = FALSE, mode = "w",
              cacheOK = TRUE)


# Import data
library(readr)
covid_data <- read.csv("data/covid19.csv", stringsAsFactors = FALSE)
covid_data <- as.data.frame(t(covid_data), stringsAsFactors = FALSE)

# Delete column doublon
# covid_data <- covid_data[, -16]

# Transform data frame
covid_data$reg <- rownames(covid_data)
colnames(covid_data) <- as.character(covid_data[1,])
covid_data <- covid_data[-1,]

# Select FR metropolitaine
covid_data <- covid_data[c(1:13),-(length(covid_data))]
# Ajout Code REG
covid_data$ID <- c("84", "27", "53", "24", "94", "44", "32", "11", "28", "75", "76", "52", "93")

# Pop Totale
POPT <- read.csv("data/reg_pop.csv", stringsAsFactors = FALSE, sep=";")

# Transform valuie in numeric
covid_data[, c(1:(length(covid_data)-1))] <- lapply(covid_data[, c(1:(length(covid_data)-1))], as.numeric)
# Max value
maxi <- max(covid_data[, c(1:(length(covid_data)-1))])

# Import basemap
library(sf)
reg <- st_read("fdc/REGION_CARTO.shp")

# Jointure
covid_data <-merge(reg, covid_data, by.x="INSEE_REG", by.y="ID")


# Construction carte animée
library(cartography)

jpeg("example%03d.png")

for (i in 6:(length(covid_data)-1)){


data <- covid_data[,c(1, 5, i)]
jour <- as.Date(colnames(st_drop_geometry(covid_data[i])))
jour <- format(as.Date(jour), "%d %B")
colnames(data) <- c("ID", "REG" ,"nb", "geometry")
data$nb <- as.numeric(data$nb)

# Recupération popT
data <- merge(data, POPT, by="ID" )
data$tx <- data$nb / data$PTOT *10000

par(mar=c(0,0,1.2,0))
plot(st_geometry(data), col= NA, border = NA)
layoutLayer(title="Nombre de cas confirmés du Coronavirus en France métroplitaine", 
            sources = "Sources: Sante publique france", 
            scale = NULL, tabtitle = TRUE, frame = TRUE, bg = "grey60")
plot(st_geometry(data), lwd=0.1, col= "grey85", border = "grey75", add=TRUE)
propSymbolsChoroLayer(x = data,var = "nb", inches = 0.3,fixmax = maxi,
                      var2 = "tx", breaks = c(0,0.1, 0.5,1,2,4,8,16), 
                      col = carto.pal(pal1 = "orange.pal", n1 = 7), border = "white", lwd = 0.1,
                      legend.var.title.txt = "Cas confirmés", 
                      legend.var.pos = c(113951.5, 6947964), 
                      legend.var2.values.rnd = 1,
                      legend.var2.pos = c(113951.5, 6272579), 
                      legend.var2.title.txt = "Tx de contagion \n (10 000 hab)")
text(1011838, 7058524, jour, font=4, cex=1.5)

}
dev.off()

system("magick -delay 110 *.png example_1.gif")


# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))

