## extract the Ekman transport for selected coordinates and days
# The csv files are wht Sadegh provided in .mat format, I just converted them to .csv so it is readable by R. The first column is the days:
#1-61 meaning 1st of December 2018 until 30th of January 2019
library("readr")
library("tidyverse")
library("reshape2")
library("janitor")
library("dplyr")
library("data.table")
## load files and select only the lon where transects were sampled
#T1

T1up <- read_delim("Upwelling/UE_MSM_T1.csv", ",")
T1up <- T1up %>% filter(lon %in% c( "-80.5","-80.25","-80","-79.75", "-79.5","-79.25")) %>% 
  mutate(transect = "Transect 1", 
         station = c("1", "4", "7", "10", "13", "14")) 
T1 <- T1up %>% melt(id.vars = c("lon", "transect", "station"))

# T2
T2up <- read_delim("Upwelling/UE_MSM_T2.csv", ",") 
T2up <- T2up %>% filter(lon %in% c("-80","-79.75", "-79.5", "-79","-78.5", "-78.25")) %>% 
  mutate(transect = "Transect 2",
         station = c(18, 20, 22, 25, 28, 30))

T2 <- T2up %>% melt(id.vars = c("lon", "transect", "station"))
# T3 
T3up <- read_delim("Upwelling/UE_MSM_T3.csv", ",")
T3up <- T3up %>% filter (lon %in% c("-77", "-77.25", "-77.5", "-77.75", "-78")) %>% 
  mutate(transect = "Transect 3",
         station = c("46_22", "45","43", "40", "38"))
  
T3 <- T3up %>% melt(id.vars = c("lon", "transect", "station"))
# T4 

T4up <- read_delim("Upwelling/UE_MSM_T4.csv", ",")
T4up <- T4up %>% filter(lon %in% c("-77.5", "-77", "-76.75", "-76.5", "-76.25", "-76")) %>% 
  mutate(transect = "Transect 4", 
         station = (c("53", "56_3", "58", "66", "63", "65")))

T4 <- T4up %>% melt(id.vars = c("lon", "transect", "station"))
# T5

T5up <- read_delim("Upwelling/UE_MSM_T5.csv", ",")  
T5up <- T5up %>% filter(lon %in% c("-75.25", "-75.5", "-75.75", "-76", "-76.5")) %>% 
  mutate(transect = "Transect 5",
         station = c("78", "74", "80", "82", "67"))
T5 <- T5up %>% melt(id.vars = c("lon", "transect", "station"))

# T6

T6up <- read_delim("Upwelling/UE_MSM_T6.csv", ",")
T6up <- T6up %>% filter(lon %in% c("-74", "-75", "-75.25", "-76", "-74.5" )) %>% 
  mutate(transect = "Transect 6",
         station = c("96", "99", "94_6", "104_1", "94"))
T6 <- T6up %>% melt(id.vars = c("lon", "transect", "station"))


Tallup <- rbind(T1up,T2up,T3up,T4up,T5up,T6up) %>% melt(id.vars = c("lon", "transect", "station"))

all_data <- read_delim("AllData_MSM80_17022021.csv", ",") %>% clean_names()

upwell <- left_join(Tallup, all_data, by = c("station","transect")) %>% select (-depth_m)
peru_coastline <- read_delim("peru coastline.csv", ";")
#check if the stations number were assigned correctly

ggplot() +coord_fixed()+
 geom_polygon(aes(x=lon, y=lat), colour = "black", fill ="grey", data=peru_coastline)+
  geom_text(mapping=aes(x=lon_e_w, y=lat_n_s, label=station), size=3, data=all_data)
# plot temporatl variability of Ekman transpott


Ts= upwell %>% select(c(transect, station, variable, value)) %>% unique()

  ggplot(Ts)+
  geom_line(aes(x=variable, y = value, colour = station, group = station),size = 1)+
  facet_wrap(. ~ transect, strip.position = "top")
 
############################################################################################
############################# EKMAN RECORDED DURING SAMPLING OF EACH TRANSECT ################
  #######################################################################################
t1during <- T1 %>% filter(variable %in%c(27,28,29)) ## the average upwelling 3 days when sampling. 27,28,29 DEC

t2during <- T2 %>% filter(variable %in% c(32,33,34)) ## the average upwelling of 3 days when sampling. 1,2,3 JAN 2019

t3during <- T3 %>% filter(variable %in% c(37,38,39)) # the average upwelling of 3 dats when sampling. 6,7,8 JAN 2019

t4during <- T4 %>% filter(variable %in% c(42,43,44))# the average upwelling of 3 days when samplng. 11,12,13 JAN 2019

t5during <- T5 %>% filter(variable %in% c(45,46,47,48))# the average upwelling of 4 days when sampling. 14-17 JAN 2019

t6during  <-T6 %>% filter(variable %in% c(53,54,55,56))# the average upwelling of 4 days when sampling 22-25 JAN 2019

## here I put all the transect in one data frame 

Tallduring <- rbind(t1during, t2during, t3during, t4during,t5during,t6during )

# here I join my other env data, such as nutrients and physical data together with the upwelling data
upwell_during <- left_join(Tallduring, all_data, by = c("station","transect")) %>% 
  select(c(transect, station, variable, value,distance_to_shore_km,ml_depth)) %>% unique()


summary= Tallduring %>% group_by(station) %>% mutate(avUpduring = mean(value), 
                                                        sdduring = sd(value))

legend <-expression ("Ekman Transport"~
                       m^2*s^-1)

bw_update <- theme_bw() +
  theme(plot.background = element_blank(),
        panel.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(colour = "black",
                                    fill = NA, size = 0.8),
        panel.grid.minor = element_line(colour = NA),
        panel.grid.major = element_line(colour = "black",
                                        size = 0.2,
                                        linetype = "dotted"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(2, "mm"),
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text( size = 10),
        legend.key = element_blank(),
        legend.background = element_blank(),
        plot.title = element_text(size = 10, hjust = 0,face = "italic"),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = 8))

ggplot(summary)+
  geom_line(aes(x= distance_to_shore_km, y=avUp, group = transect, colour = transect), size = 2)+
  geom_point(aes(x= distance_to_shore_km, y=avUp, group = transect, colour = transect), size = 4)+
  geom_errorbar(aes(x = distance_to_shore_km, ymin = avUp-sd, ymax = avUp+sd,colour = transect))+
  #scale_color_brewer(palette = "Dark2")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous(name = legend, limits = c(-2.5, 0))+
  bw_update

############################################################################################
############################# EKMAN RECORDED 3 DAYS BEFORE SAMPLING OF EACH TRANSECT ################
#######################################################################################
t1before3 <- T1 %>% filter(variable %in%c(24,25,26)) ## the average upwelling of 3 days 3 before sampling. 24,25,26 DEC

t2before3 <- T2 %>% filter(variable %in% c(29,30,31)) ## the average upwelling of 3 days when sampling. 29,30,31 DEC 2019

t3before3 <- T3 %>% filter(variable %in% c(34,35,36)) # the average upwelling of 3 days when sampling. 3,4,5 JAN 2019

t4before3 <- T4 %>% filter(variable %in% c(39,40,41))# the average upwelling of 3 days when samplng. 8,9,10 JAN 2019

t5before3 <- T5 %>% filter(variable %in% c(42,43,44,45))# the average upwelling of 4 days when sampling. 11-14 JAN 2019

t6before3 <-T6 %>% filter(variable %in% c(50,51,52,53))# the average upwelling of 4 days when sampling 19-22 JAN 2019

## here I put all the transect in one data frame 

Tallbefore3 <- rbind(t1before3, t2before3, t3before3, t4before3,t5before3,t6before3 )

# here I join my other env data, such as nutrients and physical data together with the upwelling data
upwell_before3 <- left_join(Tallbefore3, all_data, by = c("station","transect")) %>% 
  select(c(transect, station, variable, value,distance_to_shore_km,ml_depth)) %>% unique()


summary3before= Tallbefore3 %>% group_by(station) %>% mutate(avUp3before = mean(value), 
                                                        sd3before = sd(value))
ggplot(summary)+
  geom_line(aes(x= distance_to_shore_km, y=avUp, group = transect, colour = transect), size = 2)+
  geom_point(aes(x= distance_to_shore_km, y=avUp, group = transect, colour = transect), size = 4)+
  geom_errorbar(aes(x = distance_to_shore_km, ymin = avUp-sd, ymax = avUp+sd,colour = transect))+
  #scale_color_brewer(palette = "Dark2")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  #scale_y_continuous(name = legend, limits = c(-2.5, 0))+
  bw_update

############################################################################################
############################# EKMAN RECORDED 7 DAYS BEFORE SAMPLING OF EACH TRANSECT ################
#######################################################################################
t1before7 <- T1 %>% filter(variable %in%c(20,21,22)) ## the average upwelling of 3 days 7 before sampling. 24,25,26 DEC

t2before7 <- T2 %>% filter(variable %in% c(25,26,27)) ## the average upwelling of 3 days 7 days before sampling. 29,30,31 DEC 2019

t3before7 <- T3 %>% filter(variable %in% c(30,31,32)) # the average upwelling of 3 days 7 days before sampling. 3,4,5 JAN 2019

t4before7 <- T4 %>% filter(variable %in% c(35,36,37))# the average upwelling of 3 days 7 days before samplng. 8,9,10 JAN 2019

t5before7 <- T5 %>% filter(variable %in% c(38,39,40,41))# the average upwelling of 4 days 7 days before sampling. 11-14 JAN 2019

t6before7 <-T6 %>% filter(variable %in% c(46,47,48,49))# the average upwelling of 4 days 7 days before sampling 19-22 JAN 2019

## here I put all the transect in one data frame 

Tallbefore7 <- rbind(t1before7, t2before7, t3before7, t4before7,t5before7,t6before7)

# here I join my other env data, such as nutrients and physical data together with the upwelling data
upwell_before7 <- left_join(Tallbefore7, all_data, by = c("station","transect")) %>% 
  select(c(transect, station, variable, value,distance_to_shore_km,ml_depth)) %>% unique()


summary7before = Tallbefore7 %>% group_by(station) %>% mutate(avUp7before = mean(value), 
                                                         sd7before = sd(value))
ggplot(summary)+
  geom_line(aes(x= distance_to_shore_km, y=avUp, group = transect, colour = transect), size = 2)+
  geom_point(aes(x= distance_to_shore_km, y=avUp, group = transect, colour = transect), size = 4)+
  geom_errorbar(aes(x = distance_to_shore_km, ymin = avUp-sd, ymax = avUp+sd,colour = transect))+
  #scale_color_brewer(palette = "Dark2")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  #scale_y_continuous(name = legend, limits = c(-2.5, 0))+
  bw_update


#### combine during, before3 and before7 

ekman <- cbind(summary, summary3before, summary7before) %>% 
  select(c("transect...2", "station...3", "avUpduring", "sd", "avUp3before","sd3before", "avUp7before", "sd7before"))
setnames(ekman, "transect...2", "transect")
setnames(ekman, "station...3", "station")

ekmanandall <- left_join(ekman, all_data, by = c("station","transect")) %>%  
  select(c(transect, station,distance_to_shore_km,avUpduring,avUp3before,avUp7before, sd, sd3before, sd7before)) %>% unique()

ggplot(ekamnandall)+
  geom_line(aes(x= distance_to_shore_km, y=avUp, group = transect, colour = transect), size = 2)+
  geom_point(aes(x= distance_to_shore_km, y=avUp, group = transect, colour = transect), size = 4)+
  geom_errorbar(aes(x = distance_to_shore_km, ymin = avUp-sd, ymax = avUp+sd,colour = transect))+
  #scale_color_brewer(palette = "Dark2")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  #scale_y_continuous(name = legend, limits = c(-2.5, 0))+
  bw_update

ekmand <- ekmanandall %>% select(-c(sd3before,sd,sd7before))

meltedekman <- ekmand %>% melt(id.vars = c("distance_to_shore_km","station", "transect"))

ggplot(meltedekman)+
  geom_line(aes(x=distance_to_shore_km, y = value, colour = variable),size = 1)+
  geom_point(aes(x=distance_to_shore_km, y = value, colour = variable),size = 1)+
  facet_wrap(. ~ transect, strip.position = "top")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous(name = legend)
  
