#### MSM80 Cruise #####
### Dec 2018- Jan 2019
### Leila KIttu
### CTD Stations data from validated CTD profiles provided by Physical Oceanography


library(oce) ###for reading and processing CTD data (Kelley and Richards 2018)
library(ocedata) # Also has the loop  function
library(readxl)
##1. create a list of all cnv-CTD files####
#Read  CTD original files as cnv files from the folder directory
##There were 137 CTD profiles in total from 106 stations
#59 stations - All transect stations where a ctd was taken except yoyo stations and 24 hour stations,
#and stations outside the transects
##Stations outside the transect are excluded from the list

allfiles = dir("CTD_Files", full.names = TRUE, pattern = ".cnv")

##Adding station number manually



##2. Loop through  the files
##To include all ctd profiles, change the number 59 to indicate all the profiles
#profile measurement aligned to a standard pressure of 20 centimeters.
ctd = list()

for (i in 1:length(allfiles)){
  
  ctd[[i]] = read.ctd(allfiles[i])%>%
    ctdTrim(method = "downcast")%>% 
    ctdDecimate(p = 0.2)

}
section = ctd%>%as.section()

ctd.tb = ctd.tb%>%
  select(cruise, station, date, lon,lat, 
         pressure, depth, temperature)

ctd.tb = as_tibble(ctd.tb, .name_repair = "minimal")%>%
  select(cruise, station, date, lon,lat, 
         pressure, depth, temperature)
par(mfrow = c(1,2))
section%>%plot(which = "map", showStations = TRUE, showStart = TRUE)
section = ctd.tb%>%as.section()
summary(ctd.tb)
structure(ctd)

#####3. make a data frame of CTD data from CTD list####
#The ctd is the list file with 59 CTD casts from 59 stations. 
#We need to convert the profile value of each cast to a data frame and then combine them to form a large data frame with all the cast embeded. 
#That is tedious to do it manually. However, we can tell R to do it for us using a for() loop function

require(tidyverse) #chaining the process and tidying of data 
require(lubridate) #for manipulating date (Grolemund and Wickham 2011)

##Making a tibble
ctd.tb = list()


for (i in 1:length(ctd)){
  
  ctd.tb[[i]] = ctd[[i]]@data %>% 
    as_tibble() %>% 
    mutate(lon =ctd[[i]]@metadata$longitude,
           lat =ctd[[i]]@metadata$latitude,
           filename = ctd[[i]]@metadata$filename,
           hexfilename = ctd[[i]]@metadata$hexfilename
           #time = ctd[[i]]@metadata$timeS %>% dmy(),
           )%>% 
    select(lon,lat,filename,hexfilename, pressure, depth, 
           temperature, salinity, oxygen, par)
  
}
##Concert tibble to  data frame
##You can use the .df file to plot data from stations/transects
ctd.df = ctd.tb %>% bind_rows()

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




