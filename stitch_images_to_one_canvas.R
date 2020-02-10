##==================================================================================================##
##  Project        : Image Processing                                                               ##
##                                                                                                  ##
##  Description    : Scan all the images in a directory and put them in a single image              ##
##                                                                                                  ##
##  Date           : 10-Feb-2019                                                                    ##
##                                                                                                  ##
##  Author         : Deepankar Kotnala                                                              ##
##                                                                                                  ##
##  Important Note : Input  File Name: report.pdf (If we are extracting images from pdf file)       ##
##                   If we are reading data directly from the directory, then make sure that        ##
##                   the filenames follow the naming convention:                                    ##
##                                                                                                  ##
##                   report_ 'number' .png                                                          ##
##                   Example: report_1.png                                                          ##
##                   Output File Name: all_pages.png                                                ##    
##                                                                                                  ##
##                   The size of output image may vary based on the number of images                ##
##                   in the directory. Also, the loading time of the final output image             ##
##                   will increase with the increase in number of input images. This is             ##
##                   a trade-off between the readability of the content of each image in            ##
##                   the final image and the file size. Zoom in to the final image to see           ##
##                   the contents of each smaller image.                                            ##
##==================================================================================================##

# Put the directory path in the below code and uncomment it
# setwd("C:/Users/dkotn/Downloads/images")

##==================================================================================================##
# Installing the required package

install.packages(c("rstudioapi", "Rtools","qpdf","pdftools", "png"))

library(pdftools)
library(png)

##==================================================================================================##

# Setting the source file directory as the current working directory.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



# Converting each page of the pdf to image in the directory [Checkpoint 1]

pdf_convert("report.pdf")


##==================================================================================================##

# Count the number of files in the directory
f <- list.files()

image_count <- length(grep("[0-9].png", f, value = T))
print(paste0("Number of images in the directory: ", image_count))


# Dimensions of 1 page.
# Changing the Dimensions dynamically based on the number of images present in the directory.
# Reason for doing so is that if we are putting all the images in one image,
# then the content of all the images will be lost if we have huge number of images 
# and the resolution of the final image is fixed.


if (image_count > 100)
{
imgwidth <- round(sqrt(image_count)) * round(sqrt(image_count)) * 8
imgheight <- round(sqrt(image_count)) * round(sqrt(image_count)) * 8
}else if(image_count < 49)
  {
  imgwidth <- 2560
  imgheight <- 2560
}

print(imgwidth)
print(imgheight)
print(sqrt(image_count))

# Grid dimensions.
if (image_count > 100)
{
gridwidth <- round(sqrt(image_count))
gridheight <- round(sqrt(image_count))
}else if(image_count <= 49 & image_count > 36){
  gridwidth <- 10
  gridheight <- 10
}else if(image_count <= 36 & image_count > 16){
  gridwidth <- 6
  gridheight <- 6
}else if(image_count <= 16 & image_count > 9){
    gridwidth <- 4
    gridheight <- 4
}else if(image_count <= 9 & image_count > 4){
  gridwidth <- 3
  gridheight <- 3
}else{
  gridwidth <- 2
  gridheight <- 2
}

gridwidth
gridheight


# Total plot width and height.
spacing <- 1
totalwidth <- (imgwidth+spacing) * gridwidth
totalheight <- (imgheight+spacing) * gridheight

##==================================================================================================##

# Plot all the pages and save as PNG.
png("all_pages.png", round((imgwidth+spacing)*gridwidth/round(sqrt(image_count))), round((imgheight+spacing)*gridheight/round(sqrt(image_count))))
par(mar=c(0,0,0,0))
plot(0, 0, type='n', xlim=c(0, totalwidth), ylim=c(0, totalheight), asp=1, bty="n", axes=FALSE)

for (i in 1:image_count) {
  fname <- paste("report_", i, ".png", sep="")
  img <- readPNG(fname)
  
  x <- (i %% gridwidth) * (imgwidth+spacing)
  y <- totalheight - (floor(i / gridwidth)) * (imgheight+spacing)
  
  rasterImage(img, xleft=x, ybottom = y-imgheight, xright = x+imgwidth, ytop=y)
}
cat("\nAdding all images to the output image file ...")
dev.off()

{ 
  cat("\n>>> Output File Generated.")
  cat("\n>>> File Name: all_pages.png")
  cat("\n>>> File Location: " ,getwd(),"\n")
  cat("\n>>> Script Execution Completed\n")
}

##==================================================================================================##

