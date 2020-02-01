library(here)

library(ggplot2)


arrow_x_16 <- 59.5
arrow_x_notwell <- c(63.5, 67.2, 70.9)
arrow_x_20 <- 75
arrow_x_well <- c(79, 82.7, 86.4, 90.1)
arrow_x_25 <- 94.2

arrow_x_all <- c(arrow_x_poor, arrow_x_16, arrow_x_notwell, arrow_x_20, arrow_x_well, arrow_x_25)

image <- png::readPNG(here::here("./graphical_elements_act/ACT number line ENG - cropped.png"))
base_image_g <- grid::rasterGrob(image, interpolate=TRUE)
