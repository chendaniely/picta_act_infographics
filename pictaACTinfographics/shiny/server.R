
server <- function(input, output, session) {
  
  theme_nothing_text <- function(base_size = 12, base_family = "Arial")
  {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
        rect              = element_blank(),
        line              = element_blank(),
        axis.ticks        = element_blank()
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
  
  today_score_arrow <- function() {
    geom_segment(aes(x = arrow_x_all()[PT_INFO()$today_act],
                     y = score_arrow_y1,
                     xend = arrow_x_all()[PT_INFO()$today_act],
                     yend = score_arrow_y2),
                 size = score_arrow_size,
                 arrow = arrow(length = unit(score_arrow_length_unit, "cm")))
  }
  
  today_score_value <- function() {
    ggplot2::annotate("text",
                      x = arrow_x_all()[PT_INFO()$today_act],
                      y = score_today_numb_label_y,
                      label = glue::glue("{PT_INFO()$today_act}"),
                      size = 10,
                      fontface = "bold")
  }

  today_score_today <- function(language) {
    ggplot2::annotate("text",
                      x = arrow_x_all()[PT_INFO()$today_act],
                      y = score_today_text_label_y,
                      label = ifelse(language == "spanish", "Hoy", "Today"),
                      size = 4.5,
                      fontface = "bold")
  }
  

  previous_score_arrow <- function() {
    geom_segment(aes(x = arrow_x_all()[PT_INFO()$previous_act],
                     y = previous_score_arrow_y1,
                     xend = arrow_x_all()[PT_INFO()$previous_act],
                     yend = previous_score_arrow_y2),
                 size = previous_score_arrow_size,
                 color = "#939598",
                 arrow = arrow(length = unit(previous_score_arrow_length_unit, "cm")))
  }
  
  previous_score_value <- function() {
    ggplot2::annotate("text",
                      x = arrow_x_all()[PT_INFO()$previous_act],
                      y = previous_score_today_numb_label_y,
                      label = glue::glue("{PT_INFO()$previous_act}"),
                      color = "#939598",
                      size = 10,
                      fontface = "bold")
  }
  
  previous_score_date <- function(language) {
    annotate("text",
             x = arrow_x_all()[PT_INFO()$previous_act],
             y = previous_score_today_text_label_y,
             size = 4.5,
             color = "#939598",
             label = glue::glue("{ifelse(language == 'spanish', 'Última Visita','Last visit')}\n{PT_INFO()$previous_date_text}"))
  }
  
  diff_arrow_pos_right <- function() {
    geom_segment(aes(x = arrow_x_all()[PT_INFO()$previous_act] + diff_arrow_buffer_x,
                     y = previous_score_arrow_y2 - diff_arrow_buffer_y,
                     xend = arrow_x_all()[PT_INFO()$today_act] - diff_arrow_buffer_x,
                     yend = previous_score_arrow_y2 - diff_arrow_buffer_y),
                 size = previous_score_arrow_size,
                 color = "#939598",
                 arrow = arrow(length = unit(previous_score_arrow_length_unit, "cm")))
  }
  
  diff_arrow_neg_left <- function() {
    # only difference here is how the diff arrow buffer is subtracted/added on the x axis
    geom_segment(aes(x = arrow_x_all()[PT_INFO()$previous_act] - diff_arrow_buffer_x,
                     y = previous_score_arrow_y2 - diff_arrow_buffer_y,
                     xend = arrow_x_all()[PT_INFO()$today_act] + diff_arrow_buffer_x,
                     yend = previous_score_arrow_y2 - diff_arrow_buffer_y),
                 size = previous_score_arrow_size,
                 color = "#939598",
                 arrow = arrow(length = unit(previous_score_arrow_length_unit, "cm")))
  }
  
  PT_INFO <- reactive({
    PT_VALUES_ASTHMA <- list(
      language = input$language,
      name = input$name,
      display_name = input$name, # name and display name are coming from the same input
      today_act = input$today_act,
      today_date = input$today_date,
      today_date_text = NA,
      previous_act = input$previous_act,
      previous_date = input$previous_date,
      previous_date_text = NA,
      asthma_interpretive_statement = NA,
      asthma_score_statement = NA,
      asthma_progress_statment = NA
    )
    
    if (is.na(PT_VALUES_ASTHMA$previous_act)) {
      PT_VALUES_ASTHMA$previous_date_text <- ""
      PT_VALUES_ASTHMA$asthma_progress_statment <- ""
    } else {
      previous_date_date <- as.Date(PT_VALUES_ASTHMA$previous_date)
      PT_VALUES_ASTHMA$previous_date_text <- strftime(previous_date_date, "%m/%d/%y")
      PT_VALUES_ASTHMA$asthma_progress_statment <- gen_asthma_progress_statment(PT_VALUES_ASTHMA$today_act,
                                                                                PT_VALUES_ASTHMA$previous_act,
                                                                                PT_VALUES_ASTHMA$language)
    }
    
    today_date_date <- as.Date(PT_VALUES_ASTHMA$today_date)
    PT_VALUES_ASTHMA$today_date_text <- gen_asthma_date_blob(today_date_date, PT_VALUES_ASTHMA$language)
    
    PT_VALUES_ASTHMA$asthma_interpretive_statement <- gen_asthma_interpretive_statement_blob(PT_VALUES_ASTHMA$today_act,
                                                                                             PT_VALUES_ASTHMA$language)

    if (PT_VALUES_ASTHMA$language == "spanish") {
      PT_VALUES_ASTHMA$asthma_score_statement <- sprintf("Su puntaje es %s", PT_VALUES_ASTHMA$today_act)
      PT_VALUES_ASTHMA$png_url <- png_url_spanish
      PT_VALUES_ASTHMA$act_rnw_f <- "act-pamphlet_interrior-spanish.Rnw"
    } else {
      PT_VALUES_ASTHMA$asthma_score_statement <- sprintf("Your score is %s", PT_VALUES_ASTHMA$today_act)
      PT_VALUES_ASTHMA$png_url <- png_url_english
      PT_VALUES_ASTHMA$act_rnw_f <- "act-pamphlet_interrior-english.Rnw"
    }
    PT_VALUES_ASTHMA
  })
  
  arrow_x_all <- reactive({
    gen_x_coords(PT_INFO()$language)
  })

  image <- reactive({png::readPNG(PT_INFO()$png_url)})
  base_image_g <- reactive({grid::rasterGrob(image(), interpolate=TRUE)})
  
  output$asthma_statements <- renderPrint({
    print(glue::glue("Date text: {PT_INFO()$today_date_text}"))
    print(glue::glue("Score: {PT_INFO()$asthma_score_statement}"))
    print(glue::glue("Interpretive: {PT_INFO()$asthma_interpretive_statement}"))
    print(glue::glue("Progress: {PT_INFO()$asthma_progress_statment}"))
  })
  
  output$pt_list <- renderPrint({PT_INFO()})
  
  output$cwd <- renderPrint({getwd()})
  
  plot_pth_norm <- reactive({fs::path_norm(tempfile(fileext = '.png'))})
  plot_pth_unix <- reactive({gsub("\\\\", "/", plot_pth_norm())})
  fig_pth_act_exterior <- reactive({
    eng_or_spa <- stringr::str_to_upper(ifelse(input$language == "english", "eng", "spa"))
    return(glue::glue(
      "./www/graphical_elements_act/ACT exterior {eng_or_spa}.png"
    ))
  })
  
  output$plot_pth_debug <- renderPrint({
    print(plot_pth_norm());
    print(plot_pth_unix());
    print(fig_pth_act_exterior())
  })
  
  output$plot <- renderImage({
    outfile <- plot_pth_norm()
    
    base_g <- ggplot(data = dummy, aes(x = x, y = y)) +
      geom_point(alpha = 1/1000000000) +
      annotation_custom(base_image_g(), xmin = -Inf, xmax = Inf, ymin = 20, ymax = Inf) +
      theme_nothing_text() +
      scale_x_continuous(breaks = seq(1, 100, by = 5), 1) +
      theme(legend.position = "none") +
      theme(axis.title = element_blank(), axis.text = element_blank()) +
      theme(plot.title = element_text(size = 30, colour = "black")) +
      theme(text = element_text(family = "sans"))
    
    arrow_g <- base_g +
      today_score_arrow() +
      today_score_value() +
      today_score_today(PT_INFO()$language)
    
    if (!is.na(PT_INFO()$previous_act)) {
      # if there is a previous act value
      last_g <- arrow_g +
        previous_score_arrow() +
        previous_score_value() +
        previous_score_date(PT_INFO()$language)
      
      if (PT_INFO()$today_act - PT_INFO()$previous_act > 0) {
        last_g <- last_g + diff_arrow_pos_right()
        
      } else if (PT_INFO()$today_act - PT_INFO()$previous_act < 0) {
        last_g <- last_g + diff_arrow_neg_left()
      } else {
        # when previous act is the same as today's act
        last_g <- arrow_g
      }
      
      ggplot2::ggsave(filename =  outfile,
                     plot = last_g,
                     width = 11, height = 8.5)
      #last_g
    } else {
      # no previous value provided
      ggplot2::ggsave(filename =  outfile,
                     plot = arrow_g,
                     width = 11, height = 8.5)
      #arrow_g
    }
    list(src = outfile,
         contentType = 'image/png',
         width = "100%",
         #height = "100%",
         alt = "Alternative text")
  }, deleteFile = FALSE
  )

  pdf_single_filename <- reactive({
    name <- gsub(" ", "_", input$name)
    print(name)
    return(glue::glue("act-{PT_INFO()$language}-{name}-{PT_INFO()$today_date}.pdf"))
  })
  
  output$download_single <- downloadHandler(
    filename = function() {pdf_single_filename()},
    
    content = function(file) {
      out = knitr::knit2pdf(input = PT_INFO()$act_rnw_f,
                            #output = glue::glue("{input$name}-{input$today_date}.tex"),
                            clean = TRUE,
                            #quiet = TRUE,
                            compiler = "xelatex")
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = 'application/pdf'
  )
  
  output$tinytex_info <- renderPrint({
    print(tinytex:::is_tinytex())
    print(tinytex::tinytex_root())
  })

  gen_pdf_filename <- function(language, name, date) {
    return(glue::glue("act-{language}-{name}-{date}.pdf"))
  }

  output$pdf_single_fn <- renderPrint({print(pdf_single_filename())})
  
  # batch file ----
  input_file <- reactive({input$file})
  output$batch_file_pth <- renderPrint({str(input_file())})
 
  input_file_df <- reactive({
    if (!is.null(input_file())) {
      batch_df <- readr::read_csv(input_file()$datapath)
      print(batch_df)
      return(batch_df)
    } else {
      empty_df <- empty_batch_df
      print(empty_df)
      print("Need to upload data.")
      showNotification("Need to upload data.", duration = NULL, type = "error")
      return(empty_df)
    }
  })

  output$table <- DT::renderDT(input_file_df())

  new_pt_info_batch <- reactive({})
  new_pt_info_batch_i <- reactive({})

  gen_pdf_from_df <- function(dat) {
    print("in gen_pdf_from_df")
    print(dat)
    if (identical(dat, empty_batch_df)) {
      print("Need to upload data.")
    } else {
      for (row_i in 1:nrow(dat)) {
        row_dat <- dat[row_i, , drop = FALSE]
        print(row_dat)
        print(row_dat$display_name)
        new_pt_info <- list(
          language = row_dat$language,
          display_name = row_dat$display_name,
          today_act = row_dat$today_act_score,
          today_date = row_dat$today_date,
          previous_act = row_dat$previous_act_score,
          previous_date = row_dat$previous_date
        )
        print(new_pt_info)
      }
    }
  }

  output$download_batch <- downloadHandler(
    filename = function() {glue::glue("act-batch.zip")},
    content = function(file) {
        gen_pdf_from_df(input_file_df())
    },
    contentType = 'application/zip'
  )
}
