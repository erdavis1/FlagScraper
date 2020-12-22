library(png)
library(reshape2)
library(abind)
library(magick)
library(dplyr)

setwd("C:/Users/Erin/Documents/Desktop/")
options(stringsAsFactors = FALSE)

#this code assumes there's a central motif on a solid bg color that needs to be cropped out. 
#I'm sure there's a more efficient way to do this, but this is the plan:
#calculate how different each color on the flag is from the background color (assumed be the color at position (10,10))
#next, find all the pixels that are further away from the bg color than they are close to it (ie, those that are a different color)
#those different colored pixels are assumed to be the central motif, and it is cropped out and made into a square


#it's assumed all your downloaded flags are in a folder called flags in your working directory
files <- list.files("./flags/", pattern = ".gif")


#convert all flags to PNG
for (i in 1:length(files)) {
  file <- paste0("./flags/", files[i])
  img <- image_read(file)
  file <- gsub(".gif|.png|./", "", file)
  image_write(img, path = paste0("./flags/", file, ".png"), format = "png")
}



#given a vector of rgb colors, this function finds the Euclidian distance of those colors from the color at (10,10) on the flag
mydist <- function(x) {
  dist(rbind(flag[10,10,], x))
}


files <- list.files("./flags/", pattern = ".png")

for (i in 1:length(files)) {

  flag <- readPNG(paste0("./flags/", files[i])) 
  
  #skip black and white images
  if (length(dim(flag)) !=3) {
    next()
  }

  #find the distance of each pixel from the background color
  x  <- apply(flag, c(1,2), mydist)
  names(x) <- seq.int(1, ncol(x))
  y <- melt(x)
  names(y) <- c("row", "col", "val")
  
  
  #select just those pixels that are more than midway between the bg color and the furthest one from it
  #in theory, this should just be the central motif
  y <- subset(y, val >= max(y$val)/2)
  
  #get the coordinates of the central motif bounding box
  maxrow <- max(y$row)
  minrow <- min(y$row)
  maxcol <- max(y$col)
  mincol <- min(y$col)
  
  
  #figure out how to make the cropped logo a square by padding the top+bottom or sides with the bg color
  #I realize now this can be done *much* more efficiently with imagemagick but... effort...
  topsize <- max((maxcol - mincol), (maxrow - minrow))*1.05
  topsize <- round(topsize/2,0)*2
  
  centerpt <- c(round((maxrow + minrow)/2,0),  round((maxcol + mincol)/2,0))
  
  maxrow <- centerpt[1] + topsize/2
  minrow <- centerpt[1] - topsize/2
  maxcol <- centerpt[2] + topsize/2
  mincol <- centerpt[2] - topsize/2
  
  if (maxrow > dim(flag)[1]) { maxrow <- dim(flag)[1]}
  if (minrow < 0 ) { minrow <- 0}
  if (maxcol > dim(flag)[2]) { maxcol <- dim(flag)[2]}
  if (mincol < 0 ) { mincol <- 0}
  
  flag_crop <- flag[minrow:maxrow, mincol:maxcol, ]
  
  if (dim(flag_crop)[1] < dim(flag_crop)[2]) {
    numrows <- dim(flag_crop)[2] - dim(flag_crop)[1]
    numrows <- numrows/2
    
    for (k in 1:numrows) {
      flag_crop <- abind(flag_crop[1,], flag_crop, along = 1) 
    }
    
    for (k in 1:numrows) {
      flag_crop <- abind(flag_crop, flag_crop[1,], along = 1) 
    }
  } else if (dim(flag_crop)[1] > dim(flag_crop)[2]) {  
    numrows <- dim(flag_crop)[1] - dim(flag_crop)[2]
    numrows <- numrows/2
    
    for (j in 1:numrows) {
      flag_crop <- abind(flag_crop[,1], flag_crop, along = 2) 
    }
    
    for (j in 1:numrows) {
      flag_crop <- abind(flag_crop, flag_crop[,1], along = 2) 
    }
    
  }
  
  #save the newly cropped motif
  writePNG(flag_crop, paste0("./flags/cropped_", files[i]))
  
  if (i%%100 == 0) {
    print(i)
  }
}




#resize and rotate everybody
#save them in a folder called final 
files <- list.files("./flags/", pattern = "cropped")

for (j in 1:length(files)) {
  img <- image_read(paste0("./flags/", files[j]))
  img <- image_scale(img, "150")
  
  for (i in c(0, 90, 180, 270)) {
    img %>% image_rotate(i) %>% image_write(path = paste0("./final/", files[j], "_", i, ".png"), format = "png")
  }
}



