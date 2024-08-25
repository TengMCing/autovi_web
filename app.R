library(shiny)
library(tidyverse)
library(visage)
library(autovi)
library(shinydashboard)


# Setup -------------------------------------------------------------------

# This is suggested by `htmltools`, so let `renv` capture this package
markdown::mark

reticulate::py_install(c("numpy", "Pillow"),
                       envname = "r-web_interface",
                       method = c("virtualenv"),
                       ignore_installed = FALSE)
reticulate::use_virtualenv("r-web_interface")


# Globals -----------------------------------------------------------------

# Resize the image and return a string representation of the image RGB array
get_resized_plot_string <- function(im_path, target_size = c(32L, 32L)) {
  
  # Use temporary file for Python to store the string representation of 
  # the numpy list. This ensure the array will not be affected the difference
  # between the column-major and the row-major format.
  temp_string_path <- tempfile(fileext = ".txt")
  reticulate::py_run_string(glue::glue("from PIL import Image 
                                       import numpy as np
                                       p = Image.open('{im_path}').resize([{target_size[1]}, {target_size[2]}])
                                       result = np.asarray(p).tolist()
                                       p.close()
                                       with open('{temp_string_path}', 'w') as f:
                                           f.write(str(result))
                                       "))
  plot_string <- read_file(temp_string_path)
  unlink(temp_string_path)
  return(plot_string)
}

# Compute the auxiliary input and return a string representation of the array
get_auxiliary_data <- function(data) {
  paste0("[",
         paste(unname(AUTO_VI$auxiliary(data)), collapse = ", "),
         "]")
}


# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Auto Visual Inference",
                  tags$li(class = "dropdown", tags$a(href = "https://github.com/TengMCing/autovi_web", target="_blank", icon("github")))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Residual Plot Diagnostics", tabName = "single_tab", icon = icon("chart-line")),
      menuItem("Info", tabName = "get_started", icon = icon("circle-info"))
      # menuItem("Lineup Evaluation", tabName = "lineup_tab", icon = icon("layer-group"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$script(src = "js/tf.min.js"),
      tags$script(src = "js/predict.js")
    ),
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = "get_started",
              box(htmltools::includeMarkdown("www/markdown/get_started.md"), width = 12)),
      tabItem(tabName = "single_tab",
              fluidRow(
                box(htmltools::includeMarkdown("www/markdown/upload.md"),
                    textOutput("data_status"),
                    br(),
                    fileInput("upload", NULL, accept = ".csv", buttonLabel = "Upload CSV", multiple = FALSE),
                    htmltools::includeMarkdown("www/markdown/use_example.md"),
                    actionButton("example_1", "Use Lineup Example"),
                    actionButton("example_2", "Use Single Plot Example"),
                    htmltools::includeMarkdown("www/markdown/csv_type.md"),
                    radioButtons("csv_type", "CSV type", c("A single residual plot" = "single", 
                                                           "A lineup consists of multiple residual plots" = "lineup"),
                                 inline = FALSE),
                    width = 4,
                    height = "100%",
                    status = "primary"),
                box(
                  conditionalPanel(
                      condition = "input.csv_type == 'single'",
                      htmltools::includeMarkdown("www/markdown/variable_select.md"),
                      fluidRow(
                        column(4, selectInput("var_fitted", "Fitted values", c(".fitted"))),
                        column(4, selectInput("var_resid", "Residuals", c(".resid")))
                      ),
                      htmltools::includeMarkdown("www/markdown/boot_sample.md"),
                      fluidRow(
                        column(8, sliderInput("num_boot", "Bootstrapped samples", 20, 100, 20, 1, round = TRUE, width = "100%"))
                      ),
                      fluidRow(
                        column(4, numericInput("seed", "Seed", 2024, min = 1, max = 9999999, step = 1, width = "100%"))
                      )
                  ),
                    
                  conditionalPanel(
                      condition = "input.csv_type == 'lineup'",
                      htmltools::includeMarkdown("www/markdown/lineup_variable_select.md"),
                      fluidRow(
                        column(4, selectInput("lineup_var_fitted", "Fitted values", c(".fitted"))),
                        column(4, selectInput("lineup_var_resid", "Residuals", c(".resid"))),
                        column(4, selectInput("lineup_var_sample", "Labels", c("-----")))
                      ),
                      htmltools::includeMarkdown("www/markdown/lineup_boot_sample.md"),
                      fluidRow(
                        column(8, sliderInput("lineup_num_boot", "Bootstrapped samples (Optional)", 20, 100, 20, 1, round = TRUE, width = "100%"))
                      ),
                      fluidRow(
                        column(4, numericInput("lineup_seed", "Seed", 2024, min = 1, max = 9999999, step = 1, width = "100%")),
                        column(8, selectInput("true_sample", "True residual plot (Optional)", c("-----" = "0")))
                      )
                  ),
                  width = 5, height = "100%", status = "primary"
                ),
                box(
                  htmltools::includeMarkdown("www/markdown/run.md"),
                  p("🟡 TensorFlow.js Status: Please wait, TensorFlow.js is initializing", id = "tf_status"),
                  br(),
                  shinyjs::disabled(actionButton("run", icon("play"), width = "100%", style = "height: 200px; font-size: 200%")),
                  width = 3,
                  height = "100%",
                  status = "primary"),
                id = "equal-height-row", style = "display: flex; padding-bottom: 20px;"
              ),
              conditionalPanel(
                condition = "input.csv_type == 'single' && input.has_run > 0",
                fluidRow(
                  box(h3("Visual signal strength"),
                      shinycssloaders::withSpinner(DT::DTOutput("vss_table", height = 500)),
                      shinycssloaders::withSpinner(htmlOutput("vss_summary"), type = 0),
                      width = 6,
                      height = 700,
                      status = "success"),
                  box(h3("Lineup consists of one true residual plot and 19 simulated null plots"), 
                      shinycssloaders::withSpinner(plotOutput("residual_plots", width = "100%", height = "600px")),
                      width = 6,
                      height = 700,
                      status = "success")
                ),
                fluidRow(
                  box(h3("Bootstrapped visual signal strength"),
                      shinycssloaders::withSpinner(plotOutput("vss_boot_plot", width = "100%", height = "500px")),
                      shinycssloaders::withSpinner(htmlOutput("vss_boot_summary"), type = 0),
                      width = 6,
                      height = 700,
                      status = "success"),
                  box(h3("Gradient of model output with respect to the greyscale input of the true residual plot"),
                      shinyjs::hidden(actionButton("show_attention", "No significant violations (p-value > 5%), click this button to show/hide the attention map.")),
                      div(shinycssloaders::withSpinner(plotOutput("attention", width = "100%", height = "600px")), id = "attention_div"),
                      width = 6,
                      height = 700,
                      status = "success")
                )
              ),
              conditionalPanel(
                condition = "input.csv_type == 'lineup' && input.has_run > 0",
                fluidRow(
                  box(h3("Visual signal strength"),
                      shinycssloaders::withSpinner(DT::DTOutput("lineup_vss_table", height = 500)),
                      shinycssloaders::withSpinner(htmlOutput("lineup_vss_summary"), type = 0),
                      width = 6,
                      height = 700,
                      status = "success"),
                  box(h3("Lineup"), 
                      shinycssloaders::withSpinner(plotOutput("lineup_residual_plots", width = "100%", height = "600px")),
                      width = 6,
                      height = 700,
                      status = "success")),
                conditionalPanel(
                  condition = "input.csv_type == 'lineup' && input.has_run > 0 > 0 && input.true_sample != '0'",
                  fluidRow(
                    box(h3("Bootstrapped visual signal strength"),
                        shinycssloaders::withSpinner(plotOutput("lineup_vss_boot_plot", width = "100%", height = "500px")),
                        shinycssloaders::withSpinner(htmlOutput("lineup_vss_boot_summary"), type = 0),
                        width = 6,
                        height = 700,
                        status = "success"),
                    box(h3("Gradient of model output with respect to the greyscale input of the true residual plot"),
                        shinyjs::hidden(actionButton("show_lineup_attention", "No significant violations (p-value > 5%), click this button to show/hide the attention map.")),
                        div(shinycssloaders::withSpinner(plotOutput("lineup_attention", width = "100%", height = "600px")), id = "lineup_attention_div"),
                        width = 6,
                        height = 700,
                        status = "success")
                  )
                )
                
              )
              
        )
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  current_data <- reactiveVal(NULL)
  lineup_current_data <- reactiveVal(NULL)
  data_info <- reactiveVal("🟡 Data Status: no dataset available.")
  
  completed_data <- reactiveVal(NULL)
  tf_ready <- reactiveVal(FALSE)
  current_csv_type <- reactiveVal(NULL)
  
  previous_seed <- reactiveVal(NULL)
  previous_lineup_seed <- reactiveVal(NULL)
  
  updateNumericInput(session = session, "seed", value = sample(1:999999, 1))
  updateNumericInput(session = session, "lineup_seed", value = sample(1:999999, 1))
  

# Get example data --------------------------------------------------------

  observeEvent(input$example_1, {
    this_data <- readr::read_csv("s_lineup.csv")
    info_string <- glue::glue("🟢 Data Status: using lineup example. ({nrow(this_data)} rows, {ncol(this_data)} cols)")
    data_info(info_string)
    current_data(this_data)
    lineup_current_data(this_data)
    if (tf_ready()) shinyjs::enable("run")
    updateRadioButtons(session = session, "csv_type", selected = "lineup")
  })
  
  observeEvent(input$example_2, {
    this_data <- readr::read_csv("dino.csv")
    info_string <- glue::glue("🟢 Data Status: using single plot example. ({nrow(this_data)} rows, {ncol(this_data)} cols)")
    data_info(info_string)
    current_data(this_data)
    lineup_current_data(this_data)
    if (tf_ready()) shinyjs::enable("run")
    updateRadioButtons(session = session, "csv_type", selected = "single")
  })
  
# Get upload data ---------------------------------------------------------

  observeEvent(input$upload, {
    # Read the data from the uploaded file
    this_data <- readr::read_csv(input$upload$datapath)
    
    info_string <- glue::glue("🟢 Data Status: using uploaded data. ({nrow(this_data)} rows, {ncol(this_data)} cols)")
    data_info(info_string)
    current_data(this_data)
    lineup_current_data(this_data)
    if (tf_ready()) shinyjs::enable("run")
  })  

# Data status -------------------------------------------------------------
  
  output$data_status <- renderText({
    data_info()
  })

# reset -------------------------------------------------------------------

  observeEvent(input$run, {
    shinyjs::runjs('Shiny.setInputValue("has_run", 1);')
    current_csv_type(input$csv_type)
  })  
  
  observeEvent(input$csv_type, {
    if (is.null(current_csv_type()) || current_csv_type() != input$csv_type) {
      shinyjs::runjs('Shiny.setInputValue("has_run", 0);')
    }
    
    if (!is.null(current_csv_type()) && current_csv_type() == input$csv_type) {
      shinyjs::runjs('Shiny.setInputValue("has_run", 1);')
    }
  })

# Update dropdown selection -----------------------------------------------
    
  observeEvent(current_data(), {
    this_data <- current_data()
    updateSelectInput(session, 
                      "var_fitted", 
                      choices = names(this_data),
                      selected = ".fitted")
    updateSelectInput(session, 
                      "var_resid", 
                      choices = names(this_data),
                      selected = ".resid")
  })
  
  observeEvent(lineup_current_data(), {
    this_data <- lineup_current_data()
    updateSelectInput(session, 
                      "lineup_var_fitted", 
                      choices = names(this_data),
                      selected = ".fitted")
    updateSelectInput(session, 
                      "lineup_var_resid", 
                      choices = names(this_data),
                      selected = ".resid")
    
    num_levels <- unlist(lapply(this_data, function(x) length(unique(x))))
    choice_display <- paste0(names(this_data), " (", num_levels, " levels)")
    
    choices <- names(this_data)
    names(choices) <- choice_display
    
    if (".sample" %in% names(this_data)) {
      .sample_num <- length(unique(this_data$.sample))
      
      updateSelectInput(session, 
                        "lineup_var_sample", 
                        choices = c("-----" = "-----", choices),
                        selected = glue::glue(".sample"))
    } else {
      updateSelectInput(session, 
                        "lineup_var_sample", 
                        choices = c("-----" = "-----", choices),
                        selected = "-----")
    }
  })
  
  observeEvent(input$lineup_var_sample, {
    
    this_data <- lineup_current_data()
    
    if (input$lineup_var_sample %in% names(this_data)) {
      val_display <- sort(unique(this_data[[input$lineup_var_sample]]))
      val <- as.character(1:length(val_display))
      names(val) <- val_display
      
      updateSelectInput(session,
                        "true_sample",
                        choices = c("-----" = "0", val),
                        selected = "0")
    } else {
      updateSelectInput(session,
                        "true_sample",
                        choices = c("-----" = "0"),
                        selected = "0")
    }
  })
  

# Show residual plot ------------------------------------------------------
  
  output$residual_plots <- renderPlot({
    
    # Not run until prediction is made
    req(!is.null(completed_data()))
    
    # Get residuals
    new_data <- completed_data()

    # Determine if the incoming data has more than one plot
    p <- AUTO_VI$plot_resid(new_data,
                            size = 1,
                            theme = theme_light(base_size = 15)) +
      facet_wrap(~.sample, labeller = label_both)
    
    return(p)
  }, height = function() {
    h <- session$clientData$output_residual_plots_width
    ifelse(h > 600, 600, h)
  })
  
  output$lineup_residual_plots <- renderPlot({
    
    # Not run until button is clicked
    req(input$run)
    req(isolate(input$csv_type == "lineup"))
    set.seed(isolate(input$lineup_seed))
    
    # Get residuals
    this_data <- isolate(lineup_current_data())
    
    fitted_name <- isolate(input$lineup_var_fitted)
    resid_name <- isolate(input$lineup_var_resid)
    sample_name <- isolate(input$lineup_var_sample)
    
    true_sample <- isolate(as.integer(input$true_sample))
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    this_data$.sample <- this_data[[sample_name]]
    
    if (is.null(this_data$.sample)) this_data$.sample <- 1
    
    this_title <- ""
    if (length(unique(this_data$.sample)) > 20) {
      if (true_sample != 0) {
        sample_used <- c(sample(sort(unique(this_data$.sample))[-true_sample], 19), 
                         sort(unique(this_data$.sample))[true_sample])
        this_data <- this_data %>%
          filter(.sample %in% sample_used)
      } else {
        this_data <- this_data %>%
          filter(.sample %in% sample(unique(this_data$.sample), 20))
      }
      
      this_title <- "Too many plot labels. Only shown 20 random plots."
    }
    

    p <- VI_MODEL$plot(this_data,
                       size = 1,
                       theme = theme_light(base_size = 15),
                       remove_axis = TRUE,
                       remove_grid_line = TRUE) +
      facet_wrap(~.sample, labeller = label_both) +
      ggtitle(this_title)
    
    return(p)
  }, height = function() {
    h <- session$clientData$output_lineup_residual_plots_width
    ifelse(h > 600, 600, h)
  })
  

# Compute vss -------------------------------------------------------------
  
  observeEvent(input$run, {
    
    req(isolate(input$csv_type == "single"))
    set.seed(isolate(input$seed))
    
    this_data <- isolate(current_data())
    
    fitted_name <- isolate(input$var_fitted)
    resid_name <- isolate(input$var_resid)
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    
    new_pos <- sample(1:20, 1)
    new_data <- map_df((1:20)[-new_pos], function(.x) {
      tibble(.fitted = this_data$.fitted, 
             .resid = rnorm(nrow(this_data), mean = 0, sd = sd(this_data$.resid)),
             .sample = .x,
             null = TRUE)
    }) %>%
      bind_rows(this_data %>%
                  select(.fitted, .resid) %>%
                  mutate(.sample = new_pos, null = FALSE))
    completed_data(new_data)
    
    input_array <- "array_input_data=["
    auxiliary_array <- "array_auxiliary_data=["
    
    for (this_sample in 1:20) {
      
      current_data <- new_data %>%
        filter(.sample == this_sample) %>%
        select(.fitted, .resid)
      current_auxiliary <- AUTO_VI$auxiliary(current_data)
      current_p <- AUTO_VI$plot_resid(current_data)
      current_path <- save_plot(current_p)
      
      input_array <- paste(input_array, get_resized_plot_string(current_path))
      input_array <- paste(input_array, ",")
      
      auxiliary_array <- paste(auxiliary_array, get_auxiliary_data(current_data))
      auxiliary_array <- paste(auxiliary_array, ",")
      
      remove_plot(current_path)
    }
    
    input_array <- paste(input_array, "];")
    auxiliary_array <- paste(auxiliary_array, "];")
    
    shinyjs::runjs(input_array)
    shinyjs::runjs(auxiliary_array)
    shinyjs::runjs("predict();")
  })
  
  observeEvent(input$run, {
    
    req(isolate(input$csv_type == "lineup"))
    set.seed(isolate(input$lineup_seed))
    
    this_data <- isolate(lineup_current_data())
    
    fitted_name <- isolate(input$lineup_var_fitted)
    resid_name <- isolate(input$lineup_var_resid)
    sample_name <- isolate(input$lineup_var_sample)
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    this_data$.sample <- this_data[[sample_name]]
    
    # Manually makes a lineup
    if (!".sample" %in% names(this_data)) {
      this_data$.sample <- 1
    }
    
    sorted_sample <- sort(unique(this_data$.sample))
    input_array <- "array_lineup_input_data=["
    auxiliary_array <- "array_lineup_auxiliary_data=["
    
    for (this_sample in sorted_sample) {
      
      current_data <- this_data %>%
        filter(.sample == this_sample) %>%
        select(.fitted, .resid)
      current_auxiliary <- AUTO_VI$auxiliary(current_data)
      current_p <- AUTO_VI$plot_resid(current_data)
      current_path <- save_plot(current_p)
      
      input_array <- paste(input_array, get_resized_plot_string(current_path))
      input_array <- paste(input_array, ",")
      
      auxiliary_array <- paste(auxiliary_array, get_auxiliary_data(current_data))
      auxiliary_array <- paste(auxiliary_array, ",")
      
      remove_plot(current_path)
    }
    
    input_array <- paste(input_array, "];")
    auxiliary_array <- paste(auxiliary_array, "];")
    
    shinyjs::runjs(input_array)
    shinyjs::runjs(auxiliary_array)
    shinyjs::runjs("predict_lineup();")
  })

# Show vss ----------------------------------------------------------------
  
  output$vss_summary <- renderText({
    req(input$run)
    req(length(input$prediction) > 0)
    
    new_data <- isolate(completed_data())
    
    sorted_sample <- sort(unique(new_data$.sample))
    vss <- unlist(input$prediction[-1])
    true_pos <- new_data$.sample[new_data$null == FALSE][1]
    p_value <- sum(vss >= vss[true_pos])/length(vss)
    num_gt <- sum(vss > vss[true_pos])
    
    if (p_value > 0.05) {
      shinyjs::show("show_attention")
      shinyjs::hide("attention_div")
    } else {
      shinyjs::hide("show_attention")
      shinyjs::show("attention_div")
    }
    
    glue::glue('<h4>The true residual plot has vss = {format(vss[true_pos], digits = 3)} and p-value = {format(p_value, digits = 3)}. There are {num_gt} null plots that have vss greater than the true residual plot.</h4>')
  })
  
  output$vss_table <- DT::renderDT(server = FALSE, {
    req(input$run)
    req(length(input$prediction) > 0)
    
    new_data <- isolate(completed_data())
    
    sorted_sample <- sort(unique(new_data$.sample))
    vss <- unlist(input$prediction[-1])

    data.frame(.sample = sorted_sample,
               vss = vss,
               rank = rank(-vss, ties.method = "first"),
               null = new_data %>%
                 group_by(.sample) %>%
                 summarise(null = first(null)) %>% 
                 pull(null)) %>%
      mutate(.sample = factor(.sample)) %>%
      mutate(vss = format(vss, digits = 3)) %>% 
      arrange(desc(vss)) %>%
      DT::datatable(rownames = FALSE, extensions = "Buttons", 
                    options = list(paging = TRUE,
                                   scrollX = TRUE, 
                                   searching = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf'),
                                   pageLength = 10, 
                                   lengthMenu = c(3, 5, 10)))
  })
  
  output$lineup_vss_summary <- renderText({
    req(input$run)
    req(length(input$lineup_prediction) > 0)
    
    this_data <- isolate(lineup_current_data())
    
    fitted_name <- isolate(input$lineup_var_fitted)
    resid_name <- isolate(input$lineup_var_resid)
    sample_name <- isolate(input$lineup_var_sample)
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    this_data$.sample <- this_data[[sample_name]]
    
    # Manually makes a lineup
    if (!".sample" %in% names(this_data)) {
      this_data$.sample <- 1
    }
    
    sorted_sample <- sort(unique(this_data$.sample))
    vss <- unlist(input$lineup_prediction[-1])
    p_value <- unlist(lapply(vss, function(x) mean(vss >= x)))
    true_sample <- isolate(as.integer(input$true_sample))
    
    if (true_sample != 0) {
      true_vss <- vss[true_sample]
      true_p_value <- p_value[true_sample]
      num_gt <- sum(vss > true_vss)
      
      if (p_value[true_sample] > 0.05) {
        shinyjs::show("show_lineup_attention")
        shinyjs::hide("lineup_attention_div")
      } else {
        shinyjs::hide("show_lineup_attention")
        shinyjs::show("lineup_attention_div")
      }
      
      glue::glue('<h4>The true residual plot has vss = {format(true_vss, digits = 3)} and p-value = {format(true_p_value, digits = 3)}. There are {num_gt} null plots that have vss greater than the true residual plot.</h4>')
    } else {
      "<h4></h4>"
    }
  })
  
  output$lineup_vss_table <- DT::renderDT(server = FALSE, {
    req(input$run)
    req(length(input$lineup_prediction) > 0)
    
    this_data <- isolate(lineup_current_data())
    
    fitted_name <- isolate(input$lineup_var_fitted)
    resid_name <- isolate(input$lineup_var_resid)
    sample_name <- isolate(input$lineup_var_sample)
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    this_data$.sample <- this_data[[sample_name]]
    
    # Manually makes a lineup
    if (!".sample" %in% names(this_data)) {
      this_data$.sample <- 1
    }
    
    sorted_sample <- sort(unique(this_data$.sample))
    vss <- unlist(input$lineup_prediction[-1])
    true_sample <- isolate(as.integer(input$true_sample))
    
    if (true_sample == 0) {
      null <- "NA"
    } else {
      null <- rep(TRUE, length(vss))
      null[true_sample] <- FALSE
    }
    
    data.frame(.sample = sorted_sample,
               vss = vss,
               rank = rank(-vss, ties.method = "first"),
               null = null) %>%
      mutate(.sample = factor(.sample)) %>%
      mutate(vss = format(vss, digits = 3)) %>% 
      mutate(rank = format(rank)) %>%
      arrange(desc(vss)) %>%
      DT::datatable(rownames = FALSE, extensions = "Buttons", 
                    options = list(paging = TRUE,
                                   scrollX = TRUE, 
                                   searching = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf'),
                                   pageLength = 10, 
                                   lengthMenu = c(3, 5, 10)))
  })
  
# Compute boot vss --------------------------------------------------------
  
  observeEvent(input$run, {
    
    req(isolate(input$csv_type == "single"))
    set.seed(isolate(input$seed))
    
    this_data <- isolate(current_data())
    
    fitted_name <- isolate(input$var_fitted)
    resid_name <- isolate(input$var_resid)
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    
    this_data$.boot <- 1
    result_data <- this_data
    
    for (i in 2:(as.integer(input$num_boot) + 1)) {
      current_data <- this_data[sample(1:nrow(this_data), nrow(this_data), replace = TRUE), ]
      current_data$.boot <- i
      result_data <- bind_rows(result_data, current_data)
    }
    
    input_array <- "array_boot_input_data=["
    auxiliary_array <- "array_boot_auxiliary_data=["
    
    for (i in 1:(as.integer(input$num_boot) + 1)) {
      
      current_data <- result_data %>%
        filter(.boot == i) %>%
        select(.fitted, .resid)
      current_auxiliary <- AUTO_VI$auxiliary(current_data)
      current_p <- AUTO_VI$plot_resid(current_data)
      current_path <- save_plot(current_p)
      
      input_array <- paste(input_array, get_resized_plot_string(current_path))
      input_array <- paste(input_array, ",")
      
      auxiliary_array <- paste(auxiliary_array, get_auxiliary_data(current_data))
      auxiliary_array <- paste(auxiliary_array, ",")
      
      remove_plot(current_path)
    }
    
    input_array <- paste(input_array, "];")
    auxiliary_array <- paste(auxiliary_array, "];")
    
    shinyjs::runjs(input_array)
    shinyjs::runjs(auxiliary_array)
    shinyjs::runjs("predict_boot();")
    shinyjs::runjs("get_grad();")
    
  })

  observeEvent(input$run, {
    
    req(isolate(input$csv_type == "lineup"))
    set.seed(isolate(input$lineup_seed))
    
    this_data <- isolate(lineup_current_data())
    
    fitted_name <- isolate(input$lineup_var_fitted)
    resid_name <- isolate(input$lineup_var_resid)
    sample_name <- isolate(input$lineup_var_sample)
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    this_data$.sample <- this_data[[sample_name]]
    
    req(input$true_sample != "0")
    
    this_data <- this_data %>%
      filter(.sample == sort(unique(this_data$.sample))[as.integer(input$true_sample)])
    
    req(nrow(this_data) > 0)
    
    this_data$.boot <- 1
    result_data <- this_data
    
    for (i in 2:(as.integer(input$lineup_num_boot) + 1)) {
      current_data <- this_data[sample(1:nrow(this_data), nrow(this_data), replace = TRUE), ]
      current_data$.boot <- i
      result_data <- bind_rows(result_data, current_data)
    }
    
    input_array <- "array_lineup_boot_input_data=["
    auxiliary_array <- "array_lineup_boot_auxiliary_data=["
    
    for (i in 1:(as.integer(input$lineup_num_boot) + 1)) {
      
      current_data <- result_data %>%
        filter(.boot == i) %>%
        select(.fitted, .resid)
      current_auxiliary <- AUTO_VI$auxiliary(current_data)
      current_p <- AUTO_VI$plot_resid(current_data)
      current_path <- save_plot(current_p)
      
      input_array <- paste(input_array, get_resized_plot_string(current_path))
      input_array <- paste(input_array, ",")
      
      auxiliary_array <- paste(auxiliary_array, get_auxiliary_data(current_data))
      auxiliary_array <- paste(auxiliary_array, ",")
      
      remove_plot(current_path)
    }
    
    input_array <- paste(input_array, "];")
    auxiliary_array <- paste(auxiliary_array, "];")
    
    shinyjs::runjs(input_array)
    shinyjs::runjs(auxiliary_array)
    shinyjs::runjs("predict_lineup_boot();")
    shinyjs::runjs("get_lineup_grad();")
  })  
  

# Show boot vss -----------------------------------------------------------
  
  output$vss_boot_summary <- renderText({
    req(input$run)
    req(length(input$boot_prediction) > 0)
    req(length(input$prediction) > 0)
    
    ori_vss <- unlist(input$prediction[-1])
    vss <- unlist(input$boot_prediction[-1])[-1]
    new_data <- isolate(completed_data())
    true_pos <- new_data$.sample[new_data$null == FALSE][1]
    true_vss <- ori_vss[true_pos]
    
    p_value <- unlist(lapply(vss, function(x) sum(c(ori_vss[-true_pos], x) >= x)/length(ori_vss)))
    
    glue::glue("<h4>{sum(p_value <= 0.05)} out of {length(p_value)} ({scales::percent(mean(p_value <= 0.05))}) of bootstrapped residual plots show significant violations  (p-value <= 0.05).</h4>")
  })
  
  output$vss_boot_plot <- renderPlot({
    req(input$run)
    req(length(input$boot_prediction) > 0)
    req(length(input$prediction) > 0)
    
    ori_vss <- unlist(input$prediction[-1])
    vss <- unlist(input$boot_prediction[-1])[-1]
    new_data <- isolate(completed_data())
    true_pos <- new_data$.sample[new_data$null == FALSE][1]
    true_vss <- ori_vss[true_pos]
    ori_vss <- ori_vss[-true_pos]
    
    ggplot() +
      geom_density(aes(ori_vss, fill = "Null", col = "Null"), alpha = 0.6) +
      geom_rug(aes(ori_vss, col = "Null"), alpha = 0.6) +
      geom_density(aes(vss, fill = "Boot", col = "Boot"), alpha = 0.6) +
      geom_rug(aes(vss, col = "Boot"), alpha = 0.6) +
      geom_segment(aes(x = true_vss, xend = true_vss, y = 0, yend = Inf, linetype = "vss of the true plot")) +
      scale_fill_manual(values = c("#40B0A6", "#E1BE6A")) +
      scale_color_manual(values = c("#40B0A6", "#E1BE6A")) +
      labs(fill = "", col = "", linetype = "") +
      ylab("Density") +
      xlab("Visual signal strength") +
      theme_light(base_size = 15) +
      theme(legend.position = "bottom")
  })
  
  output$lineup_vss_boot_summary <- renderText({
    req(input$run)
    req(length(input$lineup_boot_prediction) > 0)
    req(length(input$lineup_prediction) > 0)
    req(isolate(input$true_sample != "0"))
    
    ori_vss <- unlist(input$lineup_prediction[-1])
    vss <- unlist(input$lineup_boot_prediction[-1])[-1]
    true_sample <- as.integer(isolate(input$true_sample))
    true_vss <- ori_vss[true_sample]
    
    p_value <- unlist(lapply(vss, function(x) sum(c(ori_vss[-true_sample], x) >= x)/length(ori_vss)))
    
    glue::glue("<h4>{sum(p_value <= 0.05)} out of {length(p_value)} ({scales::percent(mean(p_value <= 0.05))}) of bootstrapped residual plots show significant violations (p-value <= 0.05).</h4>")
  })
  
  output$lineup_vss_boot_plot <- renderPlot({
    req(input$run)
    req(length(input$lineup_boot_prediction) > 0)
    req(length(input$lineup_prediction) > 0)
    req(isolate(input$true_sample != "0"))
    
    ori_vss <- unlist(input$lineup_prediction[-1])
    vss <- unlist(input$lineup_boot_prediction[-1])[-1]
    true_sample <- as.integer(isolate(input$true_sample))
    true_vss <- ori_vss[true_sample]
    ori_vss <- ori_vss[-true_sample]
    
    ggplot() +
      geom_density(aes(ori_vss, fill = "Null", col = "Null"), alpha = 0.6) +
      geom_rug(aes(ori_vss, col = "Null"), alpha = 0.6) +
      geom_density(aes(vss, fill = "Boot", col = "Boot"), alpha = 0.6) +
      geom_rug(aes(vss, col = "Boot"), alpha = 0.6) +
      geom_segment(aes(x = true_vss, xend = true_vss, y = 0, yend = Inf, linetype = "vss of the true plot")) +
      scale_fill_manual(values = c("#40B0A6", "#E1BE6A")) +
      scale_color_manual(values = c("#40B0A6", "#E1BE6A")) +
      labs(fill = "", col = "", linetype = "") +
      ylab("Density") +
      xlab("Visual signal strength") +
      theme_light(base_size = 15) +
      theme(legend.position = "bottom")
  })
  

# attention button --------------------------------------------------------

  observeEvent(input$show_attention, {
    shinyjs::toggle("attention_div")
  })  
  
  observeEvent(input$show_lineup_attention, {
    shinyjs::toggle("lineup_attention_div")
  })  

# Show attention map ------------------------------------------------------
  
  output$attention <- renderPlot({
    req(input$run)
    req(length(input$gradient) > 0)
    
    new_data <- isolate(completed_data())
    true_pos <- new_data$.sample[new_data$null == FALSE][1]
    
    scale_zero_one <- function(x) (x - min(x))/(max(x) - min(x))
    
    matrix(input$gradient[((true_pos - 1) * 32 * 32 + 1):(true_pos * 32 * 32)], 
           ncol = 32, 
           nrow = 32, 
           byrow = TRUE) %>%
      as.data.frame() %>%
      mutate(row = rev(1:32)) %>%
      pivot_longer(V1:V32, names_to = "column", values_to = "gradient") %>%
      mutate(column = as.integer(gsub("V", "", column))) %>%
      ggplot() +
      geom_raster(aes(column, row, fill = gradient), alpha = 0.7) +
      scale_fill_gradient(low = "black", high = "white") +
      ggplot2::theme_void(base_size = 15) +
      theme(legend.position = "right")
    
  }, height = function() {
    h <- session$clientData$output_attention_width
    ifelse(h > 600, 600, h)
  })

  output$lineup_attention <- renderPlot({
    req(input$run)
    req(length(input$lineup_gradient) > 0)
    req(isolate(input$true_sample != "0"))
    
    true_sample <- as.integer(isolate(input$true_sample))
    
    scale_zero_one <- function(x) (x - min(x))/(max(x) - min(x))
    
    matrix(input$lineup_gradient[((true_sample - 1) * 32 * 32 + 1):(true_sample * 32 * 32)], 
           ncol = 32, 
           nrow = 32, 
           byrow = TRUE) %>%
      as.data.frame() %>%
      mutate(row = rev(1:32)) %>%
      pivot_longer(V1:V32, names_to = "column", values_to = "gradient") %>%
      mutate(column = as.integer(gsub("V", "", column))) %>%
      ggplot() +
      geom_raster(aes(column, row, fill = gradient), alpha = 0.7) +
      scale_fill_gradient(low = "black", high = "white") +
      ggplot2::theme_void(base_size = 15) +
      theme(legend.position = "right")
  }, height = function() {
    h <- session$clientData$output_lineup_attention_width
    ifelse(h > 600, 600, h)
  }) 
    

# TF status ---------------------------------------------------------------
  
  observeEvent(input$model_loaded, {
    shinyjs::runjs("document.getElementById('tf_status').innerHTML = '🟢 TensorFlow.js Status: Good to go!'")
    shinyjs::runjs("document.getElementById('lineup_tf_status').innerHTML = '🟢 TensorFlow.js Status: Good to go!'")
    if (!is.null(current_data())) shinyjs::enable("run")
    tf_ready(TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
