# packages
library("rgdal")
library("sp")
library("raster")

############################################## LOAD DATA
cropped_dem<-raster("D:/Julian/66_mama_dem/chamela_90m/dem_chamela_90_clean.tif")
cham_pol_rast<-raster("D:/Julian/66_mama_dem/chamela_90m/cham_pol_rast90.tif")
rio <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres")

############################################## PROFILE PLOTTING