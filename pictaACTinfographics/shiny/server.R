
server <- function(input, output, session) {

  # reactive functions -----
  previous_score_arrow <- function() {
    geom_segment(aes(x = arrow_x_all()[PT_INFO()$previous_act],
                     y = previous_score_arrow_y1,
                     xend = arrow_x_all()[PT_INFO()$previous_act],
                     yend = previous_score_arrow_y2),
                 size = previous_score_arrow_size,
                 color = "#939598",
                 arrow = arrow(length = unit(previous_score_arrow_length_unit, "cm")))
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
  
  default_inputs <- reactive({
    list(
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
  })
  
  # PT_INFO -----
  PT_INFO <- reactive({
    PT_VALUES_ASTHMA <- default_inputs()
    
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
  
  # x coord for arrows -----
  arrow_x_all <- reactive({
    gen_x_coords(PT_INFO()$language)
  })

  # base image -----
  image <- reactive({png::readPNG(PT_INFO()$png_url)})
  base_image_g <- reactive({grid::rasterGrob(image(), interpolate=TRUE)})
  
  
  
  plot_pth_norm <- reactive({fs::path_norm(tempfile(fileext = '.png'))})
  plot_pth_unix <- reactive({gsub("\\\\", "/", plot_pth_norm())})
  fig_pth_act_exterior <- reactive({gen_exterior_png_pth(PT_INFO()$language)})
  

  # image -----
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
      geom_today_score_arrow(aes(x = arrow_x_all()[PT_INFO()$today_act], y = score_arrow_y1, xend = arrow_x_all()[PT_INFO()$today_act], yend = score_arrow_y2)) +
      geom_today_score_value(x = arrow_x_all()[PT_INFO()$today_act], y = score_today_numb_label_y, label = glue::glue("{PT_INFO()$today_act}")) +
      geom_today_score_today(x = arrow_x_all()[PT_INFO()$today_act], y = score_today_text_label_y, language = PT_INFO()$language)
    
    if (!is.na(PT_INFO()$previous_act)) {
      # if there is a previous act value
      last_g <- arrow_g +
        previous_score_arrow() +
        geom_previous_score_value(x = arrow_x_all()[PT_INFO()$previous_act], y = previous_score_today_numb_label_y, previous_act_score = PT_INFO()$previous_act) +
        geom_previous_score_date(x = arrow_x_all()[PT_INFO()$previous_act], y = previous_score_today_text_label_y, language = PT_INFO()$language, previous_date = PT_INFO()$previous_date)
      
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

  # pdf file name -----
  pdf_single_filename <- reactive({
    name <- gsub(" ", "_", input$name)
    print(name)
    return(glue::glue("act-{PT_INFO()$language}-{name}-{PT_INFO()$today_date}.pdf"))
  })
  
  # single download button -----
  output$download_single <- downloadHandler(
    filename = function() {
      pdf_single_filename()
    },
    content = function(file) {
      out = knitr::knit2pdf(input = PT_INFO()$act_rnw_f,
                            #output = glue::glue("{input$name}-{input$today_date}.tex"),
                            clean = TRUE,
                            #quiet = TRUE,
                            compiler = "xelatex")
      # try({
      #   fs::file_delete("act-pamphlet_interrior-english.log")
      #   fs::file_delete("act-pamphlet_interrior-english.tex")
      #   fs::file_delete("act-pamphlet_interrior-spanish.log")
      #   fs::file_delete("act-pamphlet_interrior-spanish.tex")
      # })
      file.rename(out, file) # move pdf to file for downloading
    },
    contentType = 'application/pdf'
  )


  # batch file ----
  input_file <- reactive({input$file})
 
  input_file_df <- reactive({
    if (!is.null(input_file())) {
      batch_df <- readr::read_csv(input_file()$datapath)
      problem_rows <- readr::problems(batch_df)$row
      
      batch_df$is_valid <- apply(batch_df, MARGIN = 1, FUN = is_row_valid)
      batch_df$is_valid[problem_rows] <- FALSE

      print(batch_df)
      return(batch_df)
    } else {
      empty_df <- empty_batch_df
      print(empty_df)
      print("Need to upload data.")
      showNotification("Need to upload data.", type = "error")
      return(empty_df)
    }
  })

  output$table <- DT::renderDT(input_file_df())
  
  output$table_errors <- DT::renderDT({
    if (is.null(input_file())) {
      return(empty_batch_df)
    } else {
      return(input_file_df() %>% dplyr::filter(is_valid == FALSE))
    }
  })

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
  
  # debug print output -----
  output$plot_pth_debug <- renderPrint({
    print(plot_pth_norm());
    print(plot_pth_unix());
    print(fig_pth_act_exterior())
  })
  
  output$asthma_statements <- renderPrint({
    print(glue::glue("Name: {PT_INFO()$display_name}"))
    print(glue::glue("Date text: {PT_INFO()$today_date_text}"))
    print(glue::glue("Score: {PT_INFO()$asthma_score_statement}"))
    print(glue::glue("Interpretive: {PT_INFO()$asthma_interpretive_statement}"))
    print(glue::glue("Progress: {PT_INFO()$asthma_progress_statment}"))
  })
  
  output$pt_list <- renderPrint({PT_INFO()})
  
  output$cwd <- renderPrint({getwd()})
  
  output$tinytex_info <- renderPrint({
    print(tinytex:::is_tinytex())
    print(tinytex::tinytex_root())
  })
  
  output$pdf_single_fn <- renderPrint({print(pdf_single_filename())})
  
  output$batch_file_pth <- renderPrint({str(input_file())})
}
