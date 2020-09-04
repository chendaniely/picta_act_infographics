options(tinytex.verbose = TRUE)

server <- function(input, output, session) {

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
    return(
      gen_pt_info(
        display_name = PT_VALUES_ASTHMA$display_name,
        language = PT_VALUES_ASTHMA$language,
        today_date = PT_VALUES_ASTHMA$today_date,
        today_act_score = PT_VALUES_ASTHMA$today_act,
        previous_date = PT_VALUES_ASTHMA$previous_date,
        previous_act_score = PT_VALUES_ASTHMA$previous_act 
      )
    )
  })
  
  # x coord for arrows -----
  arrow_x_all <- reactive({
    gen_x_coords(PT_INFO()$language)
  })

  # base image -----
  image <- reactive({png::readPNG(PT_INFO()$png_url)})
  base_image_g <- reactive({grid::rasterGrob(image(), interpolate = TRUE)})
  
  
  
  plot_pth_norm <- reactive({fs::path_norm(tempfile(fileext = '.png'))})
  plot_pth_unix <- reactive({gsub("\\\\", "/", plot_pth_norm())})
  fig_pth_act_exterior <- reactive({gen_exterior_png_pth(PT_INFO()$language)})
  
  arrow_g <- reactive({
    base_g <- geom_base_image(base_image_g())
    
    arrow_g <- geom_score_arrows(base_g = base_g,
                                 today_act = PT_INFO()$today_act,
                                 previous_act = PT_INFO()$previous_act,
                                 previous_date = PT_INFO()$previous_date,
                                 language = PT_INFO()$language,
                                 x_breaks = arrow_x_all())
    return(arrow_g)
  })

  # image -----
  output$plot <- renderImage({
    outfile <- plot_pth_norm()

    ggplot2::ggsave(filename =  outfile,
                    plot = arrow_g(),
                    width = 11, height = 8.5)

    list(src = outfile,
         contentType = 'image/png',
         width = "100%",
         #height = "100%",
         alt = "Alternative text")
  }, deleteFile = FALSE)

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
      withProgress(message = "Generating PDF", {
        out = knitr::knit2pdf(input = PT_INFO()$act_rnw_f,
                              #output = glue::glue("{input$name}-{input$today_date}.tex"),
                              clean = TRUE,
                              #quiet = TRUE,
                              compiler = "xelatex")
        #if (fs::file_exists("act-pamphlet_interrior-english.log")) {fs::file_delete("act-pamphlet_interrior-english.log")}
        #if (fs::file_exists("act-pamphlet_interrior-english.tex")) {fs::file_delete("act-pamphlet_interrior-english.tex")}
        #if (fs::file_exists("act-pamphlet_interrior-spanish.log")) {fs::file_delete("act-pamphlet_interrior-spanish.log")}
        #if (fs::file_exists("act-pamphlet_interrior-spanish.tex")) {fs::file_delete("act-pamphlet_interrior-spanish.tex")}
      })
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
  
  output$table_good <- DT::renderDT({
    if (is.null(input_file())) {
      return(empty_batch_df)
    } else {
      return(input_file_df() %>% dplyr::filter(is_valid == TRUE))
    }
  })

  new_pt_info_batch <- reactive({})
  new_pt_info_batch_i <- reactive({})
  
  gen_single_params <- function(row_dat) {
    print(row_dat)
    print(row_dat$is_valid)
    current_pt_info <- list(
      language = row_dat$language,
      display_name = row_dat$display_name,
      today_act = row_dat$today_act_score,
      today_date = row_dat$today_date,
      previous_act = row_dat$previous_act_score,
      previous_date = row_dat$previous_date
    )
    #print(current_pt_info)
    
    pt_info <- gen_pt_info(
      display_name = current_pt_info$display_name,
      language = current_pt_info$language,
      today_date = current_pt_info$today_date,
      today_act_score = current_pt_info$today_act,
      previous_date = current_pt_info$previous_date,
      previous_act_score = current_pt_info$previous_act
    )
    
    pt_info$pdf_name <- glue::glue("act-{pt_info$language}-{pt_info$display_name}-{row_dat$id_file}-{pt_info$today_date}.pdf")
    pt_info$pdf_name <- stringr::str_replace_all(pt_info$pdf_name, " ", "_")
    
    #print(class(pt_info))
    print(pt_info)
    return(pt_info)
  }
  
  gen_single_env <- function(single_params) {
    e <- new.env()
    
    e$PT_INFO <- function(){
      return(
        list(
          display_name = single_params$display_name,
          asthma_interpretive_statement = single_params$asthma_interpretive_statement,
          asthma_score_statement = single_params$asthma_score_statement,
          asthma_progress_statment = single_params$asthma_progress_statment,
          today_date_text = single_params$today_date_text
        )
      )
    }
    
    e$plot_pth_norm_call <- function() {
      plot_pth_norm_batch <- fs::path_norm(tempfile(fileext = '.png'))
      if (fs::file_exists(plot_pth_norm_batch)) {fs::file_delete(plot_pth_norm_batch)}
      return(plot_pth_norm_batch)
    }
    
    e$plot_pth_norm <- e$plot_pth_norm_call()
    
    e$plot_pth_unix <- function() {
      plot_pth_unix_batch <- gsub("\\\\", "/", e$plot_pth_norm)
      return(plot_pth_unix_batch)
    }
    
    e$fig_pth_act_exterior <- function() {
      fig_pth_act_exterior_batch <- gen_exterior_png_pth(single_params$language)
    }
    
    print("generated environment")
    print(e$PT_INFO())
    print(e$plot_pth_norm)
    print(e$plot_pth_unix())
    print(e$fig_pth_act_exterior())
    
    return(e)
  }
  
  gen_figure <- function(language, png_url, today_act, previous_act, previous_date) {
    
    arrow_x_all <- gen_x_coords(language)
    
    image <- png::readPNG(png_url)
    base_image_g <-grid::rasterGrob(image, interpolate = TRUE)
    
    base_g <- geom_base_image(base_image_g)
    
    arrow_g <- geom_score_arrows(base_g = base_g,
                                 today_act = today_act,
                                 previous_act = previous_act,
                                 previous_date = previous_date,
                                 language = language,
                                 x_breaks = arrow_x_all)
    return(arrow_g)
    
  }
  
  # gen_pdf_filename <- function(name, language, today_date) {
  #   name_underscore <- gsub(" ", "_", input$name)
  #   return(glue::glue("act-{language}-{name_underscore}-{today_date}.pdf"))
  # }
  
  gen_pdf_from_single_row <- function(row_dat, pdf_dir) {
   row_pt_info <- gen_single_params(row_dat)
   knit_env <- gen_single_env(row_pt_info)
   arrow_g <- gen_figure(language = row_pt_info$language,
                         png_url = row_pt_info$png_url,
                         today_act = row_pt_info$today_act,
                         previous_act = row_pt_info$previous_act,
                         previous_date = row_pt_info$previous_date)
   print(glue::glue("Saving figure to: {knit_env$plot_pth_norm}"))
   ggplot2::ggsave(filename = knit_env$plot_pth_norm,
                   plot = arrow_g,
                   width = 11,
                   height = 8.5)
   
   pdf_path <- fs::path_join(c(pdf_dir, row_pt_info$pdf_name))
   tex_path <- stringr::str_replace(pdf_path, "\\.pdf$", "\\.tex")

   print(glue::glue("tex saved to: {tex_path}"))
   print(glue::glue("PDF saved to: {pdf_path}"))
   
   #print(row_pt_info)
   
   knitr::knit2pdf(input = row_pt_info$act_rnw_f,
                   output = tex_path,
                   clean = TRUE,
                   #quiet = TRUE,
                   envir = knit_env,
                   compiler = "xelatex")

  }

  gen_pdf_from_df <- function(dat) {
    print("in gen_pdf_from_df")
    print(dat)
    if (identical(dat, empty_batch_df)) {
      print("Need to upload data.")
    } else {
      pdf_dir <- fs::path_temp('batch_pdfs')
      if (fs::dir_exists(pdf_dir)) {
        print(glue::glue("Removing: {pdf_dir}"))
        fs::dir_delete(pdf_dir)
      }
      fs::dir_create(pdf_dir, recurse = TRUE)
      print(glue::glue("Created: {pdf_dir}"))
      fs::dir_copy("www",
                   fs::path_join(c(pdf_dir, "www")),
                   overwrite = TRUE)
      
      for (row_i in 1:nrow(dat)) {
        row_dat <- dat[row_i, , drop = FALSE]
        if (row_dat$is_valid == TRUE) {
          print("************************* BATCH *************************")
          gen_pdf_from_single_row(row_dat, pdf_dir)
        } else {
          next
        }
        #break
      }
      print("BATCH PDF CREATION DONE.")
    }
    return(pdf_dir)
  }

  output$download_batch <- downloadHandler(
    filename = function() {glue::glue("act-batch.zip")},
    content = function(file) {
        saved_pdfs <- gen_pdf_from_df(input_file_df())
        
        print(glue::glue("Compressing for download: {saved_pdfs}"))
        print(glue::glue("Creating for download: {file}"))
        zip_files = fs::dir_ls(path = saved_pdfs, recurse = TRUE, glob = "*.pdf")
        print(glue::glue("Zip contents:"))
        print(zip_files)
        zip::zip(zipfile = file,
                 files = zip_files,
                 mode = "cherry-pick")
        
        print("All done. You should've gotten a save request.")
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
