#install.packages("countrycode")
library(sf)
library(tidyverse)
library(countrycode)
#install.packages("janitor")
#install.packages("maptools")
#install.packages(c("classInt","tmap"))
#install.packages(c("RColorBrewer", "sp", "rgeos", 
#                   "tmaptools", "sf", "downloader", "rgdal", 
#                   "geojsonio"))
library(sp)
library(maptools)
library(RColorBrewer)
library(classInt)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(janitor)
library(dplyr)
#loading data
world <- st_read(here::here("data",
                            "World_Countries_(Generalized)",
                            "World_Countries__Generalized_.shp"))
gender <- read.csv("data//HDR21-22_Composite_indices_complete_time_series.csv",
                   head = TRUE, sep = ",", encoding = "latin1", na = "NA")

#calculate the difference between 2019 and 2010

gender_diff <- gender %>%
  clean_names() %>%
  dplyr::select("iso3","country","gii_2010","gii_2019") %>%
  slice(1:195)%>%
  na.omit(gender)%>%
  mutate(diff = gii_2019 - gii_2010)

#filter the data 
#gender_1019 <- gender_diff %>%
#  slice(1:195)

#??pivot_longer
#library(tidyverse)

#check the data type
#class(gender_1019)
#Datatypelist <-gender_1019 %>%  
#  summarise_all(class) %>% 
#  pivot_longer(everything(), 
#               names_to="All_variables", 
#               values_to="Variable_class")
#Datatypelist

?countrycode


#covert the countrycode
library(countrycode)

diffmap_1019 <- gender_diff %>%
  mutate(code = countrycode(iso3, origin = "iso3c", destination = "iso2c"))

#join shp and csv
?left_join

diff_join <- diffmap_1019 %>%
  left_join(., world, by = c("code" = "ISO"))

#map
#tmap_mode("plot")
#qtm(diff_join,fill="diff")


#static map
tmap_mode("plot")
diff_join <- st_sf(diff_join)

breaks = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1) 

tm1 <- tm_shape(diff_join) + 
  tm_polygons("diff", 
              breaks=breaks,
              palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)

legend <- tm_shape(diff_join) +
  tm_polygons("diff",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)+
  tm_credits("(b) Difference In Gender Inequality", position=c(0.0,0.0))

t=tmap_arrange(tm1, legend, ncol=2)

t


#Basic interactive map
tmap_mode("view")

tm_shape(diff_join) + 
  tm_polygons("diff", breaks=breaks)


#Advanced interactive map
# library for pop up boxes
library(leafpop)
library(leaflet)

#remove the geometry for our pop up boxes to avoid
popupgii_2010 <-diff_join %>%
  st_drop_geometry()%>%
  dplyr::select(`gii_2010`, country)%>%
  popupTable()

popupgii_2019 <-diff_join %>%
  st_drop_geometry()%>%
  dplyr::select(`gii_2019`, country)%>%
  popupTable()

popupdiff <-diff_join %>%
  st_drop_geometry()%>%
  dplyr::select(`diff`, country)%>%
  popupTable()

tmap_mode("view")

# set the colour palettes using our previously defined breaks
pal1 <- diff_join %>%
  colorBin(palette = "YlOrRd", domain=.$`gii_2010`, bins=breaks)

pal2 <-colorBin(palette = "YlOrRd", domain=diff_join$`gii_2019`, bins=breaks)

pal3 <- diff_join %>%
  colorBin(palette = "YlOrRd", domain=.$`diff`, bins=breaks)


map<- leaflet(diff_join) %>%
  # add basemap options
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB")%>%
  
  #add our polygons, linking to the tables we just made
  addPolygons(color="white", 
              weight = 2,
              opacity = 1,
              dashArray = "3",
              popup = popupgii_2010,
              fillOpacity = 0.7,
              fillColor = ~pal2(`gii_2010`),
              group = "gii_2010")%>%
  
  addPolygons(fillColor = ~pal2(`gii_2019`), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              popup = popupgii_2019,
              fillOpacity = 0.7,group = "gii_2019")%>%
  
  addPolygons(fillColor = ~pal2(`diff`), 
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              popup = popupdiff,
              fillOpacity = 0.7,group = "diff")%>%
  
  
  # add a legend
  addLegend(pal = pal3, values = ~`diff`, group = c("gii_2010","gii_2019","diff"), 
            position ="bottomleft", title = "Difference In Gender Inequality") %>%
  # specify layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite", "CartoDB"),
    overlayGroups = c("gii_2010","gii_2019","diff"),
    options = layersControlOptions(collapsed = FALSE)
  )

# plot the map
map