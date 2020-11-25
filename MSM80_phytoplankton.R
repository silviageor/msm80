## Plotting phytoplankton groups as determined by the HPLC at the chl max ## 

## load phytoplankton groups
phyto <- read_delim("HPLC_Prochl_20_11.csv",";")
# load all data file so chl a concentration can be extracted [this files includes calculated dist from shore]
all_data <- read_delim("MSM80_Dist.csv",",")%>% setnames("Depth [m]", "Depth")
# combine both datasets

all_phyto <-full_join(all_data, phyto, by= c("Station", "Depth")) %>%  
  select(c(Station,Depth,,x,y,Type.y,MLDepth,chl_a,Km,Diatoms:Prochlorophytes ))
# remove rows where chl a concentration is missing
all_phyto = all_phyto %>% drop_na(chl_a) 
#  keep only the rows where chl a is max when data is grouped by station 

phyto_chlmax <- all_phyto %>%
  group_by(Station) %>%
  slice(which.max(chl_a))
# create colour palette with 10 colours  
library("rcartocolor")
myColors <- carto_pal(10, "Safe")

# melt phytoplankton data 
meltedall <- phyto_chlmax %>% group_by(Type.y, Station) %>% melt(id.vars = c("Km", "Type.y", "Station", "Depth", "x", "y"), 
                                                                        measure.vars = c("Diatoms","Dinoflagellates","Cryptophytes","Pelagophytes",
                                                                                         "T4haptophytes","T3haptophytes","Prasinophytes",
                                                                                         "Chlorophytes","Synechococcus","Prochlorophytes"), 
                                                                        value.name = "concentration",
                                                                        variable.name = "type")
# select only transects 
transectsonly <- filter(meltedall, Type.y %in% c("transect1","Transect2", "Transect3(IMARPE)", "Transect4", "Transect5", "Transect6"))

# plot data 
ggplot(transectsonly, aes(x = as.numeric(Km), y = concentration,fill = type))+
  geom_bar(position =position_fill(vjust = 1),stat = "identity",size=2 ,width = 15)+
  facet_wrap(. ~ Type.y, strip.position = "top")+
  scale_y_continuous("Class contribution")+
  scale_fill_manual("Class", values = myColors)+
  ylab("Class contribution")+
  scale_x_reverse(name = 'Distance from shore (km)')+
  theme(panel.spacing = unit(1, "lines"),plot.title = element_text(size = 10, hjust = 0,face = "italic"),
        axis.title = element_text(size = 10),strip.text = element_text(face = "italic"))

########## Pie chart on top of spatial data ####

# load coast data
coast <- read_delim("peru coastline.csv", ";")

ggplot()+
  #force mapping coordinates
  coord_quickmap()+
  #the land mass
  geom_polygon(data = coast, aes(x=lon, y=lat), fill = "grey70", colour = "black", size = 0.5, show.legend = FALSE)+
  # improve axis labels
  scale_x_continuous(breaks = c(-74,-76,-78,-80),
                     labels = c( "74°W", "76°W", "78°W", "80°W")) +
  scale_y_continuous(labels = c("20S", "15°S", "10°S", "5°S", "0°S"))+
  labs(x = NULL, y = NULL) +
  # piechart 
  geom_scatterpie(aes(x=x, y=y, group = type, r=0.2), 
                  data = phyto_chlmax, cols = colnames(phyto_chlmax[,c(9:18)]))+
  scale_fill_manual("Class", values = myColors)+
  theme(legend.position = "right")
# Improve on the x and y axis labels
