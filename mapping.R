devtools::install_github("rstudio/leaflet")
library("leaflet")

mymap <- leaflet()
mymap <- addTiles(mymap)


mymap <- setView(mymap, -84.3847, 33.7613, zoom = 17)
mymap





mymap <- leaflet() %>% 
  addTiles() %>%
  setView(-84.3847, 33.7613, zoom = 17) %>%
  addPopups(-84.3847, 33.7616, 'Data journalists at work, <b>NICAR 2015</b>')

mymap


starbucks <- read.csv("https://opendata.socrata.com/api/views/ddym-zvjk/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)
str(starbucks)
atlanta <- subset(starbucks, City == "Atlanta" & State == "GA")
leaflet() %>% addTiles() %>% setView(-84.3847, 33.7613, zoom = 16) %>%
  addMarkers(data = atlanta, lat = ~ Latitude, lng = ~ Longitude,popup = atlanta$Name) %>%
  addPopups(-84.3847, 33.7616, 'Data journalists at work, <b>NICAR 2015</b>')












