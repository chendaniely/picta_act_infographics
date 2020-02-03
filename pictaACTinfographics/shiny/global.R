library(here)
library(RCurl)
library(ggplot2)
library(ggtext)
library(shiny)

dummy <- data.frame(x=c(0,100), y=c(0, 100)) # need this to actually show plot

arrow_x_poor <- c( 8.7, 11.8, 14.9, 18.0, 21.1, # 5
                   24.2, 27.3, 30.4, 33.5, 36.8, # 10
                   40.5, 44.2, 47.9, 51.6, 55.3  # 15
)

arrow_x_16 <- 59.5
arrow_x_notwell <- c(63.5, 67.2, 70.9)
arrow_x_20 <- 75
arrow_x_well <- c(79, 82.7, 86.4, 90.1)
arrow_x_25 <- 94.2

arrow_x_all <- c(arrow_x_poor, arrow_x_16, arrow_x_notwell, arrow_x_20, arrow_x_well, arrow_x_25)

png_url_english <- "./www/graphical_elements_act/ACT number line ENG - cropped.png"
#png_img_english <- tempfile(fileext = ".png")
#png_img_english
#download.file(url = png_url_english,destfile =  png_img_english)

image <- png::readPNG(png_url_english)

base_image_g <- grid::rasterGrob(image, interpolate=TRUE)
