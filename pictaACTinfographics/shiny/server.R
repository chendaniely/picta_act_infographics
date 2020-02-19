
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
    geom_segment(aes(x = arrow_x_all[PT_INFO()$today_act],
                     y = PT_INFO()$score_arrow_y1,
                     xend = arrow_x_all[PT_INFO()$today_act],
                     yend = PT_INFO()$score_arrow_y2),
                 size = PT_INFO()$score_arrow_size,
                 arrow = arrow(length = unit(PT_INFO()$score_arrow_length_unit, "cm")))
  }
  
  today_score_value <- function() {
    ggtext::geom_richtext(
      aes(
        x = arrow_x_all[PT_INFO()$today_act],
        y = PT_INFO()$score_today_numb_label_y,
        label = glue::glue("**{PT_INFO()$today_act}**")
      ),
      size = 10,
      fill = NA, label.color = NA, # remove background and outline
      label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    )
  }
  
  today_score_today <- function(language) {
    ggtext::geom_richtext(aes(
      x = arrow_x_all[PT_INFO()$today_act],
      y = PT_INFO()$score_today_text_label_y,
      label = ifelse(language == "spanish", "**Hoy**", "**Today**")
    ),
    size = 4.5,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    )
  }
  

  previous_score_arrow <- function() {
    geom_segment(aes(x = arrow_x_all[PT_INFO()$previous_act],
                     y = PT_INFO()$previous_score_arrow_y1,
                     xend = arrow_x_all[PT_INFO()$previous_act],
                     yend = PT_INFO()$previous_score_arrow_y2),
                 size = PT_INFO()$previous_score_arrow_size,
                 color = "#939598",
                 arrow = arrow(length = unit(PT_INFO()$previous_score_arrow_length_unit, "cm")))
  }
  
  previous_score_value <- function() {
    ggtext::geom_richtext(aes(
      x = arrow_x_all[PT_INFO()$previous_act],
      y = PT_INFO()$previous_score_today_numb_label_y,
      #color = "#939598",
      label = glue::glue("<b style='color:#939598'>{PT_INFO()$previous_act}</b>")
    ),
    size = 10,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
    )
  }
  
  previous_score_date <- function(language) {
    annotate("text",
             x = arrow_x_all[PT_INFO()$previous_act],
             y = PT_INFO()$previous_score_today_text_label_y,
             size = 4.5,
             color = "#939598",
             label = glue::glue("{ifelse(language == 'spanish', 'Última Visita','Last visit')}\n{PT_INFO()$previous_date_text}"))
  }
  
  diff_arrow_pos_right <- function() {
    geom_segment(aes(x = arrow_x_all[PT_INFO()$previous_act] + PT_INFO()$diff_arrow_buffer_x,
                     y = PT_INFO()$previous_score_arrow_y2 - PT_INFO()$diff_arrow_buffer_y,
                     xend = arrow_x_all[PT_INFO()$today_act] - PT_INFO()$diff_arrow_buffer_x,
                     yend = PT_INFO()$previous_score_arrow_y2 - PT_INFO()$diff_arrow_buffer_y),
                 size = PT_INFO()$previous_score_arrow_size,
                 color = "#939598",
                 arrow = arrow(length = unit(PT_INFO()$previous_score_arrow_length_unit, "cm")))
  }
  
  diff_arrow_neg_left <- function() {
    # only difference here is how the diff arrow buffer is subtracted/added on the x axis
    geom_segment(aes(x = arrow_x_all[PT_INFO()$previous_act] - PT_INFO()$diff_arrow_buffer_x,
                     y = PT_INFO()$previous_score_arrow_y2 - PT_INFO()$diff_arrow_buffer_y,
                     xend = arrow_x_all[PT_INFO()$today_act] + PT_INFO()$diff_arrow_buffer_x,
                     yend = PT_INFO()$previous_score_arrow_y2 - PT_INFO()$diff_arrow_buffer_y),
                 size = PT_INFO()$previous_score_arrow_size,
                 color = "#939598",
                 arrow = arrow(length = unit(PT_INFO()$previous_score_arrow_length_unit, "cm")))
  }
  
  PT_INFO <- reactive({
    PT_VALUES_ASTHMA <- list(
      language = input$language,
      name = input$name,
      display_name = input$name,
      today_act = input$today_act,
      today_date = input$today_date,
      today_date_text = NA,
      previous_act = input$previous_act,
      previous_date = input$previous_date,
      previous_date_text = NA,
      asthma_interpretive_statement = NA,
      asthma_score_statement = NA,
      asthma_progress_statment = NA,
      
      score_arrow_y1 = 38,
      score_arrow_y2 = 41.5,
      score_arrow_size = 1.0,
      score_arrow_length_unit = 0.25,
      score_today_numb_label_y = 35,
      score_today_text_label_y = 31.5,
      
      previous_score_arrow_y1 = 25,
      previous_score_arrow_y2 = 41.5,
      previous_score_arrow_size = 1.0,
      previous_score_arrow_length_unit = 0.25,
      previous_score_today_numb_label_y = 22,
      previous_score_today_text_label_y = 16.5,
      
      diff_arrow_buffer_x = 1,
      diff_arrow_buffer_y = .5
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
    PT_VALUES_ASTHMA$today_date_text <- strftime(today_date_date, "Date:  %B %d, %Y")

    PT_VALUES_ASTHMA$asthma_score_statement <- sprintf("Your score is %s", PT_VALUES_ASTHMA$today_act)
    
    PT_VALUES_ASTHMA$asthma_interpretive_statement <- gen_asthma_interpretive_statement_blob(PT_VALUES_ASTHMA$today_act,
                                                                                             PT_VALUES_ASTHMA$language)

    PT_VALUES_ASTHMA
  })
  
  output$asthma_statements <- renderPrint({
    print(glue::glue("Interpretive: {PT_INFO()$asthma_interpretive_statement}"))
    print(glue::glue("Progress: {PT_INFO()$asthma_progress_statment}"))
    print(glue::glue("Score: {PT_INFO()$asthma_score_statment}"))
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
      annotation_custom(base_image_g, xmin = -Inf, xmax = Inf, ymin = 20, ymax = Inf) +
      theme_nothing_text() +
      scale_x_continuous(breaks = seq(1, 100, by = 5), 1) +
      theme(legend.position = "none") +
      theme(axis.title = element_blank(), axis.text = element_blank()) +
      theme(plot.title = element_text(size = 30, colour = "black")) +
      theme(text = element_text(family = "sans"))
    
    arrow_g <- base_g +
      today_score_arrow() #+
      #today_score_value() +
      #today_score_today(PT_INFO()$language)
    
    if (!is.na(PT_INFO()$previous_act)) {
      # if there is a previous act value
      last_g <- arrow_g +
        previous_score_arrow() +
        #previous_score_value() +
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
  
  output$download_single <- downloadHandler(
    filename = glue::glue("act.pdf"),
    
    content = function(file) {
      out = knitr::knit2pdf(input = "act-pamphlet_interrior-english.Rnw",
                             #output = glue::glue("{input$name}-{input$today_date}.tex"),
                             clean = TRUE,
                             compiler = "xelatex")
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = 'application/pdf'
  )
  
  output$tinytex_info <- renderPrint({
    #print(tinytex:::is_tinytex())
    #print(tinytex::tinytex_root())
  })
}
