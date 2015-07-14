
# packages
library("rgdal")
library("sp")
library("raster")

######################## DATA PREPARATION

# working directory
setwd("D:/Julian/66_mama_dem")

# shapefile
chamela <- readOGR("/home/julian/Documents/demprofile/1/Chamela_3.shp","Chamela_3")

# dem
dem <- raster("D:/Julian/66_mama_dem/DEMsMx/resol30m/mdt50kscw1.tif")

# reproject shape
chamela_reproj <- spTransform(chamela,CRS(projection(dem)))

# cut dem based on shape
cropped_dem <- crop(dem,chamela_reproj)

# visualize
plot(cropped_dem)
plot(chamela_reproj, col=rgb(0,0,0,alpha=0.1), add=TRUE)

# write raster to disk
rf <- writeRaster(cropped_dem, filename="dem_chamela_90.tif", format="GTiff", overwrite=TRUE)

# rasterize polygon 
cham_pol_rast <- rasterize(chamela_reproj,cropped_dem,field="VEGE_gris")

# write raster to disk
rf <- writeRaster(cham_pol_rast, filename="cham_pol_rast90.tif", format="GTiff", overwrite=TRUE)

# clean dem raster
cropped_dem[is.na(cham_pol_rast)]<-NA

# write raster to disk
rf <- writeRaster(cropped_dem, filename="dem_chamela_90_clean.tif", format="GTiff", overwrite=TRUE)

# read in transects shape
trans <- readOGR(".","transecto")

# visualize
plot(cham_pol_rast)
plot(trans[1,],col="red",add=TRUE)

############################################## LOAD DATA
cropped_dem<-raster("/home/julian/Documents/demprofile/chamela_90m/dem_chamela_90_clean.tif")
cham_pol_rast<-raster("/home/julian/Documents/demprofile/chamela_90m/cham_pol_rast90.tif")
rio <- readOGR("/home/julian/Documents/demprofile/transects/perfil_tres.shp","perfil_tres")

############################################## PROFILE PLOTTING

# extract data

ext_dem <- matrix(extract(cropped_dem,rio)[[1]])
ext_dem[is.na(ext_dem)]<-1
ext_dem_smooth <- ma(ext_dem,n=21)

ext_pol <- matrix(extract(cham_pol_rast,rio)[[1]])

ext_pol

plot(ext_dem_smooth,type="l")

class(ext_dem)


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
tematic<-tematic_profile(ext_pol,
                         ext_dem_smooth,fromarray,toarray,color_matrix,fillarea=TRUE,
                         width=14,height=9,outputname="/home/julian/Documents/demprofile/outputs/perfil_90_smoothed.pdf",
                         lwd=1)

