library(here)
library(RCurl)
library(ggplot2)
library(shiny)
library(readr)
library(DT)

theme_nothing_text <- function(base_size = 12, base_family = "Arial")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      rect              = element_blank(),
      line              = element_blank(),
      axis.ticks        = element_blank()
    )
}

geom_today_score_value <- function(x, y, label, size = 10, fontface = "bold") {
  layer(
    geom = 'text',
    stat = "identity",
    position = "identity",
    params = list(
      x = x,
      y = y,
      label = label,
      size = size,
      fontface = fontface
    )
  )
}

geom_today_score_today <- function(x, y, language, size = 4.5, fontface = "bold") {
  layer(
    geom = 'text',
    stat = "identity",
    position = "identity",
    params = list(
      x = x,
      y = y,
      label = ifelse(language == "spanish", "Hoy", "Today"),
      size = size,
      fontface = fontface
    )
  )
}

translate_eng_mo_to_sp <- function(eng_mo) {
  eng_mo <- stringr::str_to_lower(eng_mo)
  switch(eng_mo,
         'january' = 'enero',
         'february' = 'febrero',
         'march' = 'marzo',
         'april' = 'abril',
         'may' = 'mayo',
         'june' = 'junio',
         'july' = 'julio',
         'august' = 'agosto',
         'september' = 'septiembre',
         'october' = 'octubre',
         'november' = 'noviembre',
         'december' = 'diciembre',
         stop(glue::glue("Unknown english month to translate into spanish: {eng_mo}"))
  )
}

gen_x_coords <- function(language) {
  if (language == "spanish") {
    arrow_x_poor <- c( 8.7, 11.8, 14.9, 18.0, 20.7, # 5
                       23.8, 26.9, 30.0, 33.1, 36.5, # 10
                       40.4, 44.1, 47.8, 51.5, 55.2  # 15
    )
    
    arrow_x_16 <- 59.5
    arrow_x_notwell <- c(63.6, 67.4, 71.1)
    arrow_x_20 <- 75
    arrow_x_well <- c(79.4, 83.2, 86.9, 90.6)
    arrow_x_25 <- 95.0
  } else {
    arrow_x_poor <- c( 8.7, 11.8, 14.9, 18.0, 21.1, # 5
                       24.2, 27.3, 30.4, 33.5, 36.8, # 10
                       40.5, 44.2, 47.9, 51.6, 55.3  # 15
    )
    
    arrow_x_16 <- 59.5
    arrow_x_notwell <- c(63.5, 67.2, 70.9)
    arrow_x_20 <- 75
    arrow_x_well <- c(79, 82.7, 86.4, 90.1)
    arrow_x_25 <- 94.2
  }
  arrow_x_all <- c(arrow_x_poor, arrow_x_16, arrow_x_notwell, arrow_x_20, arrow_x_well, arrow_x_25)
  return(arrow_x_all)
}

gen_asthma_date_blob <- function(date, language) {
  if (language == "spanish") {
    translated_mo <- translate_eng_mo_to_sp(strftime(date, "%B"))
    format_eng <- strftime(date,
                           glue::glue("Fecha:  %d de {translated_mo}, %Y"))
  } else {
    return(strftime(date, "Date:  %B %d, %Y"))
  }
}

gen_asthma_interpretive_statement_blob <- function(today_act_score, language) {
  if (today_act_score %in% 5:15) {
    if (language == "spanish") {
      return("Su asma está muy mal controlada")
    } else {
      return("Your asthma is very poorly controlled")
    }
  } else if (today_act_score %in% 16:19) {
    if (language == "spanish") {
      return("Su asma está mal controlada")
    } else {
      return("Your asthma is not well controlled")
    }
  } else if (today_act_score %in% 20:25) {
    if (language == "spanish") {
      return("Su asma está bien controlada")
    } else {
      return("Your asthma is well controlled")
    }
  } else {
    stop(sprintf("Invalid act score given. Expected 5-25, got %s", today_act_score))
  }
}

gen_asthma_progress_statment <- function(today_act_score, previous_act_score, language) {
  if (previous_act_score - today_act_score >= 3) {
    if (language == "spanish") {
      return("Ha empeorado desde su última visita.")
    } else {
      return("It has gotten worse since your last visit.")
    }
  } else if (today_act_score <= 19 && abs(previous_act_score - today_act_score) <= 2) {
    if (language == "spanish") {
      return("Sigue igual que desde su última visita.")
    } else {
      return("It is about the same as at your last visit.")
    }
  } else if (today_act_score >= 20 &&
             (today_act_score - previous_act_score >= 0 || previous_act_score - today_act_score >= 2)) {
    return("")
  } else if (today_act_score <= 19 && today_act_score - previous_act_score >= 3) {
    if (language == "spanish") {
      return("Ha habido mejora!")
    } else {
      return("You made good progress!")
    }
  } else if (today_act_score >= 20 && previous_act_score <= 19) {
    if (language == "spanish") {
      return("Muy bien!")
    } else {
      return("Great job!")
    }
  } else {
    stop("Unknown progress statement condition")
  }
}

dummy <- data.frame(x = c(0,100), y = c(0, 100)) # need this to actually show plot

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

empty_batch_df <- data.frame(id_file = NA,
                             display_name = NA,
                             language = NA,
                             today_date = NA,
                             today_act_score = NA,
                             previous_date = NA,
                             previous_act_score = NA)
