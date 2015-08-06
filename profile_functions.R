
# moving average function
ma <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}


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

tematic_profile <- function(tematic_vector,
                            dem_vector,
                            samp_points_vector,
                            from_array,to_array,
                            color_matrix,
                            sp_matrix,
                            fillarea=TRUE,
                            xlab="Distance from origin",
                            ylab="Height",
                            lwd=1,
                            outputname="perfil.pdf",
                            width=9,
                            height=6,
                            pixelsize=90)
{
  n <- length(to_array)
  meters <- c(0,(1:(length(dem_vector)-1))*pixelsize)
  pdf(outputname,onefile=TRUE, paper='A4r',width=width,height=height)
  plot(meters,dem_vector,type="l",col="black",lwd=1,xlab=xlab,ylab=ylab,bty="n",bty="l")
  
  legend(70000,1000, # where legend is placed (coodinates)
         legend=color_matrix[,1],
         fill=color_matrix[,3],
         title="Vegetation and Land Use")
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
    flag=TRUE
  }
  
  counter=0
  for (j in 1:length(sp_matrix[,1]))
  {
    
    sp_matrix$distanceplot
    sp_matrix$heightplot
    
    if (!is.na(sp_matrix[j,1]))
    {
      counter=counter+1
      points(meters[j],dem_vector[j],pch=21,bg="red",col="black")

      sp_matrix$distanceplot[sp_matrix[j,1]]<-meters[j]
      sp_matrix$heightplot[sp_matrix[j,1]]<-dem_vector[j]

      text(meters[j]+100,dem_vector[j]+40,labels=sp_matrix[j,1],cex=0.6)
    }
  }
  dev.off()
  return(sp_matrix)
}
