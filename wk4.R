install.packages("countrycode")
library(sf)
library(tidyverse)
library(countrycode)
install.packages("janitor")
install.packages("maptools")
install.packages(c("classInt","tmap"))
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))
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
world <- st_read("D:/casa05/work1/World_Countries_(Generalized)/World_Countries__Generalized_.shp")
gender <- read.csv("D:/casa05/work1/HDR21-22_Composite_indices_complete_time_series.csv",head = TRUE, sep = ",", encoding = "latin1", na = "NULL")

#calculate the difference between 2019 and 2010

gender_diff <- gender %>%
  clean_names() %>%
  dplyr::select("iso3","country","gii_2010","gii_2019") %>%
  mutate(diff = gii_2019 - gii_2010)

#filter the data 
gender_1019 <- gender_diff %>%
  slice(1:195)

??pivot_longer
library(tidyverse)

#check the data type
class(gender_1019)
Datatypelist <-gender_1019 %>%  
  summarise_all(class) %>% 
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

?countrycode


#covert the countrycode
library(countrycode)

diffmap_1019 <- gender_1019 %>%
  mutate(code = countrycode(iso3, origin = "iso3c", destination = "iso2c"))

#join shp and csv
?left_join

diff_join <- world %>%
  left_join(., diffmap_1019, by = c("ISO" = "code"))

#map
tmap_mode("plot")
qtm(diff_join,fill="diff")
