# Plotting the spatial variability in the MLD 
# Load master data file 
all_data <- read_delim("AllData_MSM80_16112020.csv",";")
#change names of lat and long
library("data.table")
library(tidyr)
setnames(all_data, "Lon [?E/?W]", "lon")
setnames(all_data, "Lat [?N/?S]" , "lat")
# load ccoastline
coastline <- read_delim("peru coastline.csv", ";")

#remove missing values in the MLDepth
all_data=all_data %>% drop_na(MLDepth)

library("ggplot2")
ggplot() +
  # force mapping coordinates
  coord_map() +
  # plot stations with color proportional to MLD
  geom_point(aes(x=lon, y=lat, colour=MLDepth), data=all_data, size=5,na.rm = TRUE) +
  # add the coast
  geom_polygon(aes(x=lon, y=lat), data=coastline) +
  # fix axis labels
  scale_x_continuous(breaks = c(-74,-76,-78,-80,-82),labels = c("74°W", "76°W", "78°W","80°W", "82°W"))+
  scale_y_continuous(breaks= c(-10, -12.5,-15,-17.5), labels = c("10°S","12.5°S","15°S","17.5°S"))+
  scale_colour_viridis_c(name = "MLD [m]")

# simple numerical interpolation
library("akima")
library("akima")
# perform the interpolation with grid size 45 x 30 [depending on desired resolution nx and ny might be changes]
interp_MLD <- interp(x=all_data$lon, y=all_data$lat, z=all_data$MLDepth, linear=TRUE, nx=60, ny=45, duplicate = "strip",
                      extrap = FALSE) %>%  interp2xyz() %>% as.data.frame() 
names(interp_MLD) <- (c("lon", "lat", "mldepth"))

ggplot() +
  coord_quickmap() +
  geom_tile(aes(x=lon, y=lat, fill=mldepth), data=interp_MLD) +
  geom_polygon(aes(x=lon, y=lat), data=coastline) +
  # add a (continuous) viridis scale
  scale_fill_viridis_c(na.value=NA)
