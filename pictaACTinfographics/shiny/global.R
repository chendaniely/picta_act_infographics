library(here)
library(RCurl)
library(ggplot2)
#library(ggtext)
library(shiny)
library(readr)
library(DT)

dummy <- data.frame(x=c(0,100), y=c(0, 100)) # need this to actually show plot

score_arrow_y1 = 38
score_arrow_y2 = 41.5
score_arrow_size = 1.0
score_arrow_length_unit = 0.25
score_today_numb_label_y = 35
score_today_text_label_y = 31.5

previous_score_arrow_y1 = 25
previous_score_arrow_y2 = 41.5
previous_score_arrow_size = 1.0
previous_score_arrow_length_unit = 0.25
previous_score_today_numb_label_y = 22
previous_score_today_text_label_y = 16.5

diff_arrow_buffer_x = 1
diff_arrow_buffer_y = .5

png_url_english <- "./www/graphical_elements_act/ACT number line ENG - cropped.png"
png_url_spanish <- "./www/graphical_elements_act/ACT number line SPA - cropped.png"
