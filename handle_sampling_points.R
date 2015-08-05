# packages
library("rgdal")
library("sp")
library("raster")

# working directory
setwd("D:/Julian/66_mama_dem/")

# shapefile
chamela <- readOGR("Chamela_3.shp","Chamela_3")

# read sampling points table
samp_points <- read.table("puntos_muestreo.csv",sep=",",header=TRUE)

# table structure
head(samp_points)

# turn sampling points table into a spacial opject
coordinates(samp_points)<-~UTM_X+UTM_Y

# visualize
plot(chamela)
points(samp_points,col="red",bg="red",pch=21)

# assign projection
projection(samp_points)<-projection(chamela)

# write to shapefile
writeOGR(samp_points, "D:/Julian/66_mama_dem/sampling_points/samp_points.shp", "samp_points", driver="ESRI Shapefile")

# Load dem which has the projection we want for this project
cropped_dem<-raster("D:/Julian/66_mama_dem/chamela_90m/dem_chamela_90_clean.tif")

# transform sampling points shape to match projection
samp_points_reproj <- spTransform(samp_points,CRS(projection(cropped_dem)))

# write to shapefile
writeOGR(samp_points_reproj, "D:/Julian/66_mama_dem/sampling_points/samp_points.shp", "samp_points", driver="ESRI Shapefile")

### find closest point from each sampling point to the river

# load river shapefile
rio <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres")

# get every coordinate as a simple matrix 
river_coords <- do.call("rbind", lapply(rio@lines, function(x1) do.call("rbind", lapply(x1@Lines, function(x2) x2@coords[-nrow(x2@coords), ]))))

# visualize
plot(cropped_dem)
points(samp_points_reproj,col="red",bg="red",pch=21)
points(river_coords,col="blue",bg="blue",pch=21,cex=0.2)

# distance from every sampling point to every river point
closest_points <- list()
for (i in 1:nrow(samp_points_reproj))
{
  dists <- spDistsN1(river_coords,coordinates(samp_points_reproj)[i,], longlat = FALSE)
  closest_point <- river_coords[which.min(dists),]
  closest_points[[i]]<-closest_point
}

# unlist closest points to a data.frame
closest_points_df <- data.frame(matrix(unlist(closest_points), nrow=nrow(samp_points_reproj), byrow=T))
closest_points_df$id <- seq(1,nrow(samp_points_reproj))
colnames(closest_points_df)<-c("x","y","id")

# make a spatial object
coordinates(closest_points_df)=~x+y

# write to disk
projection(closest_points_df)<-projection(cropped_dem)
writeOGR(closest_points_df, "D:/Julian/66_mama_dem/sampling_points/closest_river_points.shp", "closest_river_points", driver="ESRI Shapefile")

# visualize
plot(cropped_dem)
points(samp_points_reproj,col="red",bg="red",pch=21)
points(river_coords,col="blue",bg="blue",pch=21,cex=0.2)
points(closest_points_df,col="green",bg="green",pch=21)

# rasterize
spot_

