#Exercici 2

library(readxl)
library(rgdal)
library(sf)
library(tidyverse)
library(tmap)
library(ggmap)
library(leaflet)
library(raster)
library(gstat)
library(spatstat)
library(sp)    
library(maptools)
library(rgeos)
library(stars)

## Accés a la BBDD de la qualitat de l'aire a Madrid
calidad_aire <- read.csv('https://datos.madrid.es/egob/catalogo/212531-10515086-calidad-aire-tiempo-real.csv', sep = ';')

##Accés a les dades d'estació de control
url <- "https://datos.madrid.es/egob/catalogo/212629-0-estaciones-control-aire.xls"
file <- "X212629_0_estaciones_control_aire.xls"
curl::curl_download(url, file)
estaciones_control_aire <- read_excel(file)

##Dades dels limits geogràfics
# Cream arxius temporals
temp <- tempfile()
temp2 <- tempfile()

# Download the zip file and save to 'temp' 
URL_2 <- "https://datos.madrid.es/egob/catalogo/300497-11325362-distritos-municipales-madrid.zip"
download.file(URL_2, temp)

# Unzip the contents of the temp and save unzipped content in 'temp2'
unzip(zipfile = temp, exdir = temp2)
data_madrid <- readOGR(dsn = temp2, layer = 'Distritos_20210712')

#Cream un dataframe de coordenades amb el codi de cada estació
coords <- data.frame(estaciones_control_aire$CODIGO_CORTO)

#Canviem el nom de la columna
names(coords)[1] <- 'codigo'

# Substitutim els símbols y lletres
estaciones_control_aire$LONGITUD_ETRS89 <- str_replace_all(estaciones_control_aire$LONGITUD_ETRS89, 
                                                           c("°" = " ", "º" = " ", "'" = " ", 
                                                             "\"" = "", "O" = "", "  " = " "))


#Iteram per cada valor i calculam la conversió
for (i in 1:length(estaciones_control_aire$LONGITUD_ETRS89)){
#Separam per espais
  d <- unlist(strsplit(estaciones_control_aire$LONGITUD_ETRS89[i], " ", fixed=T))
  a <- as.numeric(d[[1]][1])
  b <- as.numeric(d[[2]][1])/60
  c <- as.numeric(d[[3]][1])/3600
  coords[i,c('long')] <- (a + b + c)*(-1)
  }
 
#Repetim el procediment per latitud
estaciones_control_aire$LATITUD_ETRS89 <- str_replace_all(estaciones_control_aire$LATITUD_ETRS89, 
                                                          c("°" = " ", "º" = " ", "'" = " ", 
                                                            "\"" = "", "N" = "", "  " = " "))

for (i in 1:length(estaciones_control_aire$LATITUD_ETRS89)){
  #Separam per espais
  d <- unlist(strsplit(estaciones_control_aire$LATITUD_ETRS89[i], " ", fixed=T))
  a <- as.numeric(d[[1]][1])
  b <- as.numeric(d[[2]][1])/60
  c <- as.numeric(d[[3]][1])/3600
  coords[i,c('lat')] <- (a + b + c)
}

#Comprovam les dades
head(coords, 5)

# Cream un subset de dates en el que ens filtri per dades validades i magnitud = 8 que equival a NO2
data_calidad <- subset(calidad_aire, MAGNITUD == 8)

#Miram quina combinació de columnes i línies tenen una lletra N
c <- which(data_calidad == "N", arr.ind=TRUE)

#Ho convertim en un dataframe i li restam 1 a la columna perquè igualarem tots els N a 0
c <- as.data.frame(c)
c$col <- c$col -1
for(i in 1:nrow(c)){
  data_calidad[c[i,1], c[i,2]] = 0
  }

#Cream un vector amb el nom de les columnes de validació que ja no ens interesen
a <- NULL
for(i in 1:24){
  if (i<=9){
    a <- append(a, values = paste("V0", i, sep = ""))
  } else { 
      a <- append(a, values = paste("V", i, sep = ""))}
}


# Eliminam les columnes de validació
data_calidad <- data_calidad[ , !colnames(data_calidad) %in% a]

#Calculam el màxim valor de contaminació per estació
data_calidad$row_maximum = apply(data_calidad[,9:32], 1, max)

#Unim totes les dades de coordenades d'estacions de qualitat d'aire amb els valors de contaminació
NO2_estaciones <- merge(coords,
                        data_calidad,
                        by.x = 'estaciones_control_aire.CODIGO_CORTO' , 
                        by.y = 'ESTACION',
                        all.x = FALSE,
                        all.y = FALSE)


#Seleccionam el subset que ens interesa mostrar
NO2_estaciones <- subset(NO2_estaciones, 
                         select = c(long, 
                                    lat, 
                                    row_maximum))


#Visualitzam el mapa amb els límits del districte

m <- leaflet() %>% setView(lat  = 40.416775, lng = -3.703790, zoom = 10)
m %>% addTiles() %>% 
  #Afegim els poligons que representen els districtes de Madrid
  addPolygons(data = spTransform(data_madrid, "+init=epsg:4326"),
              fillOpacity = 0.2,
              stroke = T,
              weight = 3,
              label = ~data_madrid$NOMBRE) %>%
  #Afegim els punts amb les estacions de qualitat del aire
  addCircles(lng = NO2_estaciones$long,
             lat = NO2_estaciones$lat,
             color = 'black',
             weight = 5,
             fillOpacity = 0.9)
 
#Per calcular el mapa per interpolació IDW, unim els districtes en un mateix poligon
madrid_municipio <- gUnaryUnion(data_madrid)
madrid_municipio <- spTransform(madrid_municipio, CRS("+init=epsg:4326"))

#Després calculam les coordenades com punts espacials
coords.sf <- st_as_sf(NO2_estaciones, coords = c("long", "lat"))
st_crs(coords.sf) <- st_crs(madrid_municipio)

#Finalment cream el raster per a un conjunt de 6 possibles valors pel paràmetre IDP. Per cada un d'aquests,
#es genera un objecte gstat diferent com a input de la funció d'interpolació
r <- raster(madrid_municipio, res=0.0005)
par(mfrow=c(2,3), mai=c(0.5,0.5,0.5,0.5))
for (idp.val in c(0.5,1,2,4,10,100)){
  gstat.parametros <- gstat(formula=row_maximum~1, locations=coords.sf,set = list(idp = idp.val))
  NO2.idw <- interpolate(r, gstat.parametros)
  NO2.idwr <- mask(NO2.idw, madrid_municipio)
  plot(NO2.idwr)
  title(paste("IDP: ",idp.val))}

#Elaboració del variograma
gstat.parametros <- gstat(formula=row_maximum~1, locations=coords.sf)
variograma.NO2 <- variogram(gstat.parametros, width=0.2)
var.teorico <-fit.variogram(variograma.NO2, vgm(c("Exp", "Ste", "Sph","Mat","Gau","Spl")))
plot(variograma.NO2, var.teorico)

#Càlcul del model
NO2.ordkg <- gstat(NULL, "NO2", row_maximum~1, coords.sf, model=var.teorico)
NO2.ordkgr <- interpolate(r, NO2.ordkg)
NO2.kgr <- mask(NO2.ordkgr, madrid_municipio)
plot(NO2.kgr)
