gen_exterior_png_pth <- function(language) {
  eng_or_spa <- stringr::str_to_upper(ifelse(language == "english", "eng", "spa"))
  return(glue::glue(
    "./www/graphical_elements_act/ACT exterior {eng_or_spa}.png"
  ))
}

theme_nothing_text <- function(base_size = 12, base_family = "Arial") {
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

geom_previous_score_value <- function(x, y, previous_act_score, color = "#939598", size = 10, fontface = "bold") {
  layer(
    geom = 'text',
    stat = "identity",
    position = "identity",
    params = list(
      x = x,
      y = y,
      label = previous_act_score,
      color = color,
      size = size,
      fontface = fontface
    )
  )
}

geom_previous_score_date <- function(x, y, language, previous_date, size = 4.5, color = "#939598") {
  layer(
    geom = 'text',
    stat = "identity",
    position = "identity",
    params = list(
      x = x,
      y = y,
      label = glue::glue("{ifelse(language == 'spanish', 'Última Visita','Last visit')}\n{previous_date}"),
      color = color,
      size = size
    )
  )
}

geom_today_score_arrow <- function(mapping = NULL, arrow_length = score_arrow_length_unit, arrow_size = score_arrow_size) {
  layer(data = NULL,
        mapping = mapping,
        stat = "identity",
        geom = GeomSegment, 
        position = "identity",
        params = list(arrow = arrow(length = unit(arrow_length, "cm")),
                      size = arrow_size,
                      arrow.fill = NULL, 
                      lineend = "butt",
                      linejoin = "round"))
}

geom_previous_score_arrow <- function(mapping = NULL, arrow_length = score_arrow_length_unit, arrow_size = previous_score_arrow_size, arrow_color = "#939598") {
  layer(data = NULL,
        mapping = mapping,
        stat = "identity",
        geom = GeomSegment, 
        position = "identity",
        params = list(arrow = arrow(length = unit(arrow_length, "cm")),
                      size = arrow_size,
                      color = arrow_color,
                      arrow.fill = NULL, 
                      lineend = "butt",
                      linejoin = "round"))
}

geom_diff_arrow_pos_right <- function(mapping = NULL, arrow_length = score_arrow_length_unit, arrow_size = previous_score_arrow_size, arrow_color = "#939598") {
  layer(data = NULL,
        mapping = mapping,
        stat = "identity",
        geom = GeomSegment, 
        position = "identity",
        params = list(arrow = arrow(length = unit(arrow_length, "cm")),
                      size = arrow_size,
                      color = arrow_color,
                      arrow.fill = NULL, 
                      lineend = "butt",
                      linejoin = "round"))
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