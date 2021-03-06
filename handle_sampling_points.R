# packages
library("rgdal")
library("sp")
library("raster")

# working directory
setwd("E:/work/20150806_thematic_profile/")

# shapefile
chamela <- readOGR("Chamela_3.shp","Chamela_3")

# read sampling points table
samp_points <- read.table("puntos_finales_20151102.csv",sep=",",header=TRUE)

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
writeOGR(samp_points, "samp_points_20151102.shp", "samp_points", driver="ESRI Shapefile")

# Load dem which has the projection we want for this project
cropped_dem<-raster("E:/work/20150806_thematic_profile/dem_chamela_90_clean.tif")

# transform sampling points shape to match projection
samp_points_reproj <- spTransform(samp_points,CRS(projection(cropped_dem)))

dim(samp_points_reproj)

# write to shapefile
writeOGR(samp_points_reproj, "E:/work/20150806_thematic_profile/samp_points_20151102.shp", "samp_points_20151102", driver="ESRI Shapefile")

### find closest point from each sampling point to the river

# load river shapefile
rio <- readOGR("E:/work/20150806_thematic_profile/perfil_tres.shp","perfil_tres")

# make a raster from dem raster
dem_table <- as.data.frame(rasterToPoints(cropped_dem))
dem_table$cx<-dem_table$x
dem_table$cy<-dem_table$y
coordinates(dem_table)=~x+y
gridded(dem_table)=TRUE
dem_table <- brick(dem_table)
projection(dem_table)<-projection(cropped_dem)

# extract coordinates using river shape
river_coords <- extract(dem_table,rio)
river_coords <- matrix(unlist(river_coords), nrow=1323, byrow=FALSE)
river_coords <- river_coords[,2:3]

# visualize
plot(cropped_dem)
points(samp_points_reproj,col="red",bg="red",pch=21)
points(river_coords,col="blue",bg="blue",pch=21,cex=0.2)

# remove missings
river_coords_clean <- river_coords[complete.cases(river_coords),]

# distance from every sampling point to every river point
closest_points <- list()
for (i in 1:nrow(samp_points_reproj))
{
  dists <- spDistsN1(river_coords_clean,coordinates(samp_points_reproj)[i,], longlat = FALSE)
  closest_point <- river_coords_clean[which.min(dists),]
  closest_points[[i]]<-closest_point
}

length(closest_points)

# unlist closest points to a data.frame
closest_points_df <- data.frame(matrix(unlist(closest_points), nrow=nrow(samp_points_reproj), byrow=T))
closest_points_df$id <- seq(1,nrow(samp_points_reproj))
colnames(closest_points_df)<-c("x","y","id")

# make a spatial object
coordinates(closest_points_df)=~x+y

# write to disk
projection(closest_points_df)<-projection(cropped_dem)
writeOGR(closest_points_df, "E:/work/20150806_thematic_profile/closest_river_points.shp", "closest_river_points", driver="ESRI Shapefile")

# visualize
plot(cropped_dem)
points(samp_points_reproj,col="red",bg="red",pch=21)
points(river_coords,col="blue",bg="blue",pch=21,cex=0.2)
points(closest_points_df,col="green",bg="green",pch=21)

# rasterize
closest_points_df
closest_points_raster <- rasterize(closest_points_df,cropped_dem,field="id")

closest_points_raster[!is.na(closest_points_raster)]
length(closest_points_raster[!is.na(closest_points_raster)])

# write raster to disk
writeRaster(closest_points_raster, filename="E:/work/20150806_thematic_profile/closest_points_raster_20151102.tif", format="GTiff", overwrite=TRUE)

