######################################################################
#AUTHOR: LEILA KITTU
#FILE LAST UPDATED: 16.06.2021
##SCRIPT USED TO PLOT POINT NUTRIENT AND POM DATA
#####################################################################

###set theme ######
theme_point <-  theme_bw() + 
  theme(panel.grid = element_blank(),
        plot.background = element_blank(),
        panel.background = element_rect(colour = NA, fill = "grey100"),#Changed fill from NA to "grey50"
        panel.border = element_rect(colour = "black",
                                    fill = NA, size = 1),
        panel.grid.major = element_line(colour = "black",
                                        size = 0.2,
                                        linetype = "dashed"),
        axis.title.x = element_text(size = 17,family="Times",hjust = 0.5),
        axis.title.y = element_text(size = 17,family="Times",vjust = -0.5),
        axis.text.x = element_text(size = 16, family="Times",hjust = 0,colour = "black"),##was initially 12 for individual plots
        axis.text.y = element_text(size = 16, family="Times",hjust = 0,colour = "black"),##was initially 12 for individual plots
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(1.5, "mm"),
        legend.position = "right",
        legend.background = element_rect(fill = "transparent"),##background of text
        legend.key = element_rect(fill = "transparent"), #background of scale
        legend.text = element_text(size = 16, family="Times",hjust = 0,colour = "black"),
        legend.title = element_text(size = 16, family="Times",hjust = 0,colour = "black"),
        plot.title = element_text(size = 17, family="Times",hjust = 0),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = 17, family="Times",hjust = 0))

##select only stations that fall into transects
ALL_DATA2 <-ALL_DATA[ALL_DATA$transect %in% c("Transect 1", "Transect 2","Transect 3","Transect 4","Transect 5","Transect 6"), ]  

###1. NUTRIENT PLOTS######
####Select Nutrient data
NUT_AL<- select(ALL_DATA2,"station","transect", "distancetoshore","depth", "nitrate","phosphate" ,"silicate"  )%>% drop_na()

##Removed stations - because they are repeated- 24 hour stations or drifter stations###
#46_10-46_22 =st 46_5
#St 80, (80_9,80_16)=80_5, 82
#St 102_8 =St 102_1

##Use row numbers to remove
NUT_AL<- NUT_AL[-c(158:208,328:333,351:367,386:410,531:540), ]

legendNITRAT <- expression(atop("NO"["3"]^-{}, 
                                "\n[μmol L"^-1*"]"))

max(NUT_AL$nitrate) ##49.226
min(NUT_AL$nitrate) ##0

NITRATall<- ggplot(NUT_AL, aes(x = distancetoshore, y = depth, fill = nitrate))+
  geom_point(aes(fill = nitrate),size=4, shape=21, stroke=0.5,position = position_dodge2(width = 0.3)) +
  scale_fill_gradientn(legendNITRAT, colours = cmocean("matter")(20), na.value="grey",limits=c(0,48),guide= guide_colorbar(barwidth = 2, barheight = 20, nbin = 50)) +
  facet_wrap(. ~ transect, strip.position = "top", ncol = 2, scales = "free_x")+
  scale_y_reverse(name = expression(paste("Depth [m]"), limits =c(300,0), breaks = seq(0,300,50))) +
  coord_cartesian(ylim = c(300,0))+ ##Used to zoom in the plot to 300 without changing the data
  scale_x_reverse(name= "Distance from coast (km)")+
  theme_grey +
  theme(panel.spacing = unit(1, "lines"),plot.title = element_text(size = 17, hjust = 0,family = "Times"),
        axis.title = element_text(size = 17),strip.text = element_text(family = "Times"))

plot(NITRATall)

#Save as png
ggsave(PON,filename = "MSM80NITRATE-point.png",width = 11, height = 10, dpi = 150, units = "in", device='png')


###2. POM PLOTS####

##Select POM data
##No 24 hour stations or drifter repeated stations
POM<- select(ALL_DATA2,"station","transect", "distancetoshore","depth","pon_umol","pon_ug","poc_umol","poc_ug" ,"poc_pon","b_si","pop", "chl_a" )%>% drop_na()


legendPON <- expression(atop("PON",
                             paste("[μg L"^-1*"]")))

max(POM$pon_ug) ##  91.27957umol,1278 ug
min(POM$pon_ug) ##0 2 ug

PON<- ggplot(POM, aes(x = distancetoshore, y = depth, fill = pon_ug))+
  geom_point(aes(fill = pon_ug),size=4, shape=21, stroke=0.5) +
  scale_fill_gradientn(legendPON, colours = cmocean("matter")(20), na.value="red",limits=c(0,465),breaks=seq(0,465,50),guide= guide_colorbar(barwidth = 2, barheight = 20, nbin = 50)) +
  facet_wrap(. ~ transect, strip.position = "top", ncol = 2, scales = "free_x")+
  scale_x_reverse(name= "Distance from coast (km)")+
  scale_y_reverse(name = expression(paste("Depth [m]"), limits =c(100,0), breaks = seq(0,100,50))) +
  coord_cartesian(ylim = c(100,0))+
  theme_grey +
  theme(panel.spacing = unit(1, "lines"),plot.title = element_text(size = 17, hjust = 0,family = "Times"),
        axis.title = element_text(size = 17),strip.text = element_text(family = "Times"))

plot(PON)

#Save as png
ggsave(PON,filename = "MSM80PONug-point.png",width = 11, height = 10, dpi = 150, units = "in", device='png')


