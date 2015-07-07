

# load base dem raster
demoriginal <- raster("D:/Julian/66_mama_dem/chamela_90m/dem_chamela_90_clean.tif")

# visualize
plot(demoriginal)

# load shapefile
chamela <- readOGR("D:/Julian/66_mama_dem/Chamela_3.shp","Chamela_3")

# reproject shape
chamela <- spTransform(chamela,CRS(projection(demoriginal)))


##################################################################################################
# dem aggregated by factor of 2
dem_2 <- aggregate(demoriginal,fact=2,fun=mean)

# rasterize polygon 
cham_pol_rast_2 <- rasterize(chamela,dem_2,field="VEGE_gris")

# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_2)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_2,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_2,rio_shp),
                         extrans(dem_2,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_180m.pdf",lwd=2)

##################################################################################################
# dem aggregated by factor of 3
dem_3 <- aggregate(demoriginal,fact=3,fun=mean)

# rasterize polygon 
cham_pol_rast_3 <- rasterize(chamela,dem_3,field="VEGE_gris")

# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_3)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_3,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_3,rio_shp),
                         extrans(dem_3,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_270m.pdf",lwd=2)

##################################################################################################
# dem aggregated by factor of 4
dem_4 <- aggregate(demoriginal,fact=4,fun=mean)

# rasterize polygon 
cham_pol_rast_4 <- rasterize(chamela,dem_4,field="VEGE_gris")
cham_pol_rast_4
# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_4)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_4,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_4,rio_shp),
                         extrans(dem_4,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_360m.pdf",lwd=2)

##################################################################################################
# dem aggregated by factor of 5
dem_5 <- aggregate(demoriginal,fact=5,fun=mean)

# rasterize polygon 
cham_pol_rast_5 <- rasterize(chamela,dem_5,field="VEGE_gris")
cham_pol_rast_5
# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_5)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_5,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_5,rio_shp),
                         extrans(dem_5,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_450m.pdf",lwd=2)

##################################################################################################
# dem aggregated by factor of 6
dem_6 <- aggregate(demoriginal,fact=6,fun=mean)

# rasterize polygon 
cham_pol_rast_6 <- rasterize(chamela,dem_6,field="VEGE_gris")
cham_pol_rast_6
# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_6)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_6,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_6,rio_shp),
                         extrans(dem_6,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_540m.pdf",lwd=2)

##################################################################################################
# dem aggregated by factor of 7
dem_7 <- aggregate(demoriginal,fact=7,fun=mean)

# rasterize polygon 
cham_pol_rast_7 <- rasterize(chamela,dem_7,field="VEGE_gris")
cham_pol_rast_7
# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_7)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_7,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_7,rio_shp),
                         extrans(dem_7,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_630m.pdf",lwd=2)

##################################################################################################
# dem aggregated by factor of 8
dem_8 <- aggregate(demoriginal,fact=8,fun=mean)

# rasterize polygon 
cham_pol_rast_8 <- rasterize(chamela,dem_8,field="VEGE_gris")
cham_pol_rast_8
# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_8)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_8,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_8,rio_shp),
                         extrans(dem_8,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_720m.pdf",lwd=2)

##################################################################################################
# dem aggregated by factor of 9
dem_9 <- aggregate(demoriginal,fact=9,fun=mean)

# rasterize polygon 
cham_pol_rast_9 <- rasterize(chamela,dem_9,field="VEGE_gris")
cham_pol_rast_9
# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_9)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_9,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_9,rio_shp),
                         extrans(dem_9,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_810m.pdf",lwd=2)

##################################################################################################
# dem aggregated by factor of 11
dem_11 <- aggregate(demoriginal,fact=11,fun=mean)

# rasterize polygon 
cham_pol_rast_11 <- rasterize(chamela,dem_11,field="VEGE_gris")
cham_pol_rast_11
# river shapefile
rio_shp <- readOGR("D:/Julian/66_mama_dem/rivershape/perfil_tres.shp","perfil_tres") 

# visualize if desired
plot(cham_pol_rast_11)
plot(rio_shp,add=TRUE)

arrays <- from_to(cham_pol_rast_11,rio_shp)
fromarray <- arrays$fromarray
toarray <- arrays$toarray
tematic<-tematic_profile(extrans(cham_pol_rast_11,rio_shp),
                         extrans(dem_11,rio_shp),fromarray,toarray,color_matrix,fillarea=FALSE,
                         width=14,height=9,outputname="D:/Julian/66_mama_dem/perfiles/perfil_990m.pdf",lwd=2)