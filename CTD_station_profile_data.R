#### MSM80 Cruise #####
### Dec 2018- Jan 2019
### Leila KIttu
### CTD Stations data from validated CTD profiles provided by Physical Oceanography


##1. create a list of all cnv-CTD files that you require according to the station list (Named the folder profiles)####
#Read  CTD original files as cnv files from the folder directory
#60 files - All CTD profiles from each station. In 24 hour stations, only 1 profile file has been selected

library(oce) ###for reading and processing CTD data (Kelley and Richards 2018)
library(ocedata) # Also has the loop  function

##Read all CTD Files from the folder. Stations with more than 1 file , only one profile was taken
allfiles = dir("~/Documents/Profiles", full.names = TRUE, pattern = ".cnv")

##Because the Station ID were sometimes not indicated in the CTD file. I added them manually
library(readxl)
MSM80_stations <- read_excel("~/Documents//MSM80.xlsx", 
                         sheet = "Stations")
View(MSM80_stations)

##Loop through  the files
#profile measurement aligned to a standard pressure of 20 centimeters.
all.ctd = list()

for (i in 1:length(allfiles)){
  
  all.ctd[[i]] = read.ctd(allfiles[i])%>%
    ctdDecimate(p = 0.2) # Align to same standard pressure
  
  all.ctd[[i]][["station"]] = MSM80_stations$Station[i] ##Add station IDs
  
}

#####2. make a data frame of CTD data from CTD list####
#https://semba-blog.netlify.app/02/12/2019/isosurface-of-temperature-salinity-oxygen-and-fluorescence-in-mafia-channel-from-ctd-data/
#The ctd is the list file with 59 CTD casts from 59 stations. 
#We need to convert the profile value of each cast to data frame and then combine them to form a large data frame with all the cast embeded. 
#That is tedious to do it manually. However, we can tell R to do it for us using a for() loop function

require(tidyverse) #chaining the process and tidying of data 
require(lubridate) #for manipulating date (Grolemund and Wickham 2011)
require(sf) #sf for mapping (Pebesma 2018)
library(dplyr)

##Making a tibble
ctd.tb = list()

for (j in 1:length(all.ctd)){
  
  ctd.tb[[j]] = all.ctd[[j]]@data %>% 
    as_tibble() %>% 
    mutate(lon =all.ctd[[j]]@metadata$longitude,
           lat =all.ctd[[j]]@metadata$latitude,
           station=all.ctd[[j]]@data$station,
           #time = all.ctd[[i]]@data$time%>% dmy(),
           profile = j)%>% 
    select(station,lon,lat, profile, pressure, depth,conductivity, 
           temperature, salinity=salinity2, oxygen=oxygen2, par, fluorescence, turbidity)
  
}

##Concert tibble to  data frame
ctd.df = ctd.tb %>% bind_rows()

library(writexl)
write_xlsx(ctd.df,"~/Documents/CTD Profiles.xlsx")
# load the logsheet

logsheet <- read_excel("GeneralLogSheet_MSM80_Geomar_25012019.xlsx", skip = 2)
# create unique names in form of lat_lon so the ctd data and the rest can be combined without the need of copy/paste

logsheet = logsheet %>% unite(lat_lon, "Lat S (decimal)" , "Lon W (decimal)" , sep = "_", remove = FALSE )
# do the same for the ctd data
ctd.df = ctd.df %>% unite(lat_lon, "lat" , "lon" , sep = "_", remove = FALSE )


# combine the two data frames by the single uniting column lat_lon
ctd_logsheet <- list(ctd.df, logsheet) %>% reduce(full_join, by = "lat_lon")
write.csv(c, "logctd.csv")
## 4. Distance from shore ######
##Calculate distance from shore for every station in the CTD file

library(sf)
library(geosphere)
library(osmdata)
library(tidyverse)

# create peru polygon from the world map
map_peru <- map_data("world", c("peru"))

##Select the longitudes and latitudes of the boundary
map_peru_lon_lat <- select(map_peru, long, lat)

#Convert the long lat data and change into sf object using the WGS84 (EPSG: 4326) CRS
## This CRS is commonly used by organizations that provide GIS data for the entire globe or many countries. CRS used by Google Earth
##Make the points into a polygon/shape file of peru
##This is your reference for the coastline

map_peru_sf <- map_peru_lon_lat %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# convert your station dataframes (ctd.df) to sf object
#Each coordinate is converted to an sf object according to the reference system
##Then all the sf objects are put in one file
Stations_ctd_sf <- ctd.df %>% st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(4326) # coordinate reference system of the world

## using "dist2Line" we can calculate the distance between points and lines or the border
# of a polygon (eg. country shape file)
##In this case we use the country shape file of Peru
distctd <- geosphere::dist2Line(p = st_coordinates(Stations_ctd_sf),
                                line =
                                  st_coordinates(map_peru_sf$geometry)[,1:2])

#combine initial data from your ctd file with with distance to coastline that you calculated
#Convert the distances to Kilometer by dividing by 1000
Stations_ctd_dist <- cbind( ctd.df %>% rename(y=lat,x=lon),distctd) %>%
  mutate(Km=distance/1000)




