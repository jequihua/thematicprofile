# packages
library("rgdal")
library("sp")
library("raster")

############################################## LOAD DATA
cropped_dem<-raster("E:/work/20150806_thematic_profile/dem_chamela_90_clean.tif")
cham_pol_rast<-raster("E:/work/20150806_thematic_profile/cham_pol_rast90.tif")
rio <- readOGR("E:/work/20150806_thematic_profile/zoom_rio.shp","zoom_rio")
spoints <- raster("E:/work/20150806_thematic_profile/closest_points_raster_20151102.tif")

############################################## PROFILE PLOTTING

# extract dem data
ext_dem <- matrix(extract(cropped_dem,rio)[[1]])

# any missing values?
sum(is.na(ext_dem))
ext_dem[is.na(ext_dem)]<-1

# smooth with moving average
ext_dem_smooth <- array(ma(ext_dem,n=9))
plot(ext_dem_smooth,type="l",col="red")

# still any missing values?
sum(is.na(ext_dem_smooth))

ext_dem_smooth[1:4]<-6.2222
ext_dem_smooth[26:29]<-1.222222

ext_dem_smooth

# extract land cover data
ext_pol <- matrix(extract(cham_pol_rast,rio)[[1]])

ext_pol

# extract sampling points data
ext_cp <- matrix(extract(spoints,rio)[[1]])
ext_cp[3] <- 11
ext_cp

# lengths equal?
length(ext_dem_smooth)
length(ext_pol)
length(ext_cp)

# vegetation profile

####################################################
###################################### thematic legend
color_matrix <- matrix(0,9,3)
color_matrix[1,1]<-"Agricultural activities"
color_matrix[2,1]<-"Human settlements"
color_matrix[3,1]<-"Pine, Pine-Oak and Mountain mesophyll forests"
color_matrix[4,1]<-"Oak forest"
color_matrix[5,1]<-"Water body"
color_matrix[6,1]<-"Gallery vegetation"
color_matrix[7,1]<-"Induced and cultivated grasslands"
color_matrix[8,1]<-"Seasonally dry tropical forest"
color_matrix[9,1]<-"Aquatic vegetation"
color_matrix[,2]<-1:9
color_matrix[1,3]<-colors()[77]
color_matrix[2,3]<-colors()[84]
color_matrix[3,3]<-colors()[258]
color_matrix[4,3]<-colors()[657]
color_matrix[5,3]<-colors()[400]
color_matrix[6,3]<-colors()[26]
color_matrix[7,3]<-colors()[652]
color_matrix[8,3]<-colors()[655]
color_matrix[9,3]<-colors()[474]
###################################################
###################################################



# Visualize 
plot(cham_pol_rast)
plot(rio,add=TRUE)

arrays <- from_to(cham_pol_rast,rio)
fromarray <- arrays$fromarray
toarray <- arrays$toarray

# read sample points data
samp_names <- read.table("E:/work/20150806_thematic_profile/puntos_finales_20151102_z.csv",sep=",",header=TRUE,stringsAsFactors=FALSE)
head(samp_names)

samp_names

ext_cp
  
  tematic<-tematic_profile(tematic_vector=ext_pol,
                           dem_vector=ext_dem_smooth,
                           samp_points_vector=ext_cp,
                           from_array=fromarray,
                           to_array=toarray,
                           color_matrix=color_matrix,
                           sp_matrix=samp_names,
                           fillarea=FALSE,
                           width=14,height=9,outputname="E:/work/20150806_thematic_profile/perfil_20151102_zoom.pdf",
                           lwd=3.5)

tematic

write.table(tematic,"puntos_finales_20151102_coords.csv",sep=",",row.names=FALSE)
