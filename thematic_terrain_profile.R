
# packages
library("rgdal")
library("sp")
library("raster")

######################## DATA PREPARATION

# working directory
setwd("D:/Julian/66_mama_dem")

# shapefile
chamela <- readOGR(".","Chamela_3")

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
cropped_dem<-raster("dem_chamela_90_clean.tif")
cham_pol_rast<-raster("cham_pol_rast.tif")
trans <- readOGR(".","transecto")

############################################## PROFILE PLOTTING


# functions to find classes start and end of vegetation classes in tematic raster
# (transects)

extrans <- function(raster,transect)
{
  raster_extraction <- extract(raster,transect)
  raster_extraction <- matrix(unlist(raster_extraction),
                                      ncol = length(raster_extraction), byrow = FALSE)
  return(raster_extraction)
}

from_to <- function(tematic_raster,transect)
{
  # extract raster values lying on given transect and transform to array
  tematic_raster_extraction<-extrans(tematic_raster,transect)
                                      
  # find change points in classes
  counter <- 0
  from_list <- list()
  to_list <- list()
  from_list[[1]]<-0
  for (i in 1:(length(tematic_raster_extraction)-1))
  {
    if (tematic_raster_extraction[i]!=tematic_raster_extraction[i+1])
    {
      counter = counter+1     
      to_list[[counter]]<-i
      from_list[[counter+1]]<-i      
    }
  }
  to_list[[counter+1]]<-length(tematic_raster_extraction)
  
  # from/to lists to arrays
  to_list<-matrix(unlist(to_list), ncol = length(to_list), byrow = FALSE)
  from_list<-matrix(unlist(from_list), ncol = length(from_list), byrow = FALSE)
  returnlist <- list("fromarray"=from_list,"toarray"=to_list)
  return(returnlist)
}



# vegetation profile

####################################################
###################################### thematic legend
color_matrix <- matrix(0,9,3)
color_matrix[1,1]<-"Agricultura"
color_matrix[2,1]<-"Asentamiento humano"
color_matrix[3,1]<-"Bosque de coniferas"
color_matrix[4,1]<-"Bosque de latifoliadas"
color_matrix[5,1]<-"Cuerpo de agua"
color_matrix[6,1]<-"Galeria"
color_matrix[7,1]<-"Pastizal"
color_matrix[8,1]<-"Selva"
color_matrix[9,1]<-"Vegetación hidrofila"
color_matrix[,2]<-1:9
color_matrix[1,3]<-colors()[77]
color_matrix[2,3]<-colors()[84]
color_matrix[3,3]<-colors()[258]
color_matrix[4,3]<-colors()[657]
color_matrix[5,3]<-colors()[461]
color_matrix[6,3]<-colors()[26]
color_matrix[7,3]<-colors()[652]
color_matrix[8,3]<-colors()[655]
color_matrix[9,3]<-colors()[400]
###################################################
###################################################


tematic_profile <- function(tematic_vector,dem_vector,from_array,to_array,color_matrix,fillarea=TRUE,
                            xlab="Distancia sobre transecto (m)",ylab="Altitud",lwd=2,outputname="perfil.pdf",
                            width=9,height=6)
{
  n <- length(to_array)
  meters <- c(0,(1:(length(dem_vector)-1))*30)
  pdf(outputname,onefile=TRUE, paper='A4r',width=width,height=height)
  plot(meters,dem_vector,type="l",col="white",xlab=xlab,ylab=ylab,bty="n")
  flag<-FALSE
  if (fillarea)
  {
    for (i in 1:n)
    {
      if(i==1)
      {
        interval <- from_array[i]:to_array[i]
        color <- color_matrix[tematic_vector[1],3]
        polygon(c(0,meters[interval],meters[interval[length(interval)]]),c(0,dem_vector[interval],0),
                col=color,border=color)
      }
      else
      {
        interval <- from_array[i]:to_array[i]
        color <- color_matrix[tematic_vector[to_array[i]],3]
        polygon(c(meters[interval[1]],meters[interval],meters[interval[length(interval)]]),
                c(0,dem_vector[interval],0),col=color,border=color) 
      }
    }
    dev.off()
    flag=TRUE
  }
  else
  {
    for (i in 1:n)
    {
      interval <- from_array[i]:to_array[i]
      color <- color_matrix[tematic_vector[to_array[i]],3]
      lines(meters[interval],dem_vector[interval],lwd=lwd,col=color)
    }
    dev.off()
    flag=TRUE
  }
  return(flag)
}

# ejemplo rio
rio2_shp <- readOGR(".","rio2") 
plot(cham_pol_rast)
plot(rio2_shp,add=TRUE)

arrays <- from_to(cham_pol_rast,rio2_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast,rio2_shp),
                         extrans(cropped_dem,rio2_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="perfil2_90_test_outline.pdf",lwd=1)

color=colors()[31]
plot(1,1,pch=21,bg=color,col=color,cex=2)
