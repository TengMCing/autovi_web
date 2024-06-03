library(shiny)
library(tidyverse)
library(visage)
library(autovi)
library(shinydashboard)

# put a demo tab, include some example datasets
# instead of upload, allow the button to use example plots
# 
# Tab 1: single plot, Tab 2: lineup
# bootstrapping (takes most of the time)

# write up the goal, or the components of the app first
# 



# Setup -------------------------------------------------------------------

# This is suggested by `htmltools`, so let `renv` capture this package
markdown::mark

reticulate::py_install(c("numpy", "Pillow"),
                       envname = "r-web_interface",
                       method = c("virtualenv"),
                       ignore_installed = FALSE)
reticulate::use_virtualenv("r-web_interface")


# Globals -----------------------------------------------------------------

install_tf_dependencies <- function() {
  if (!reticulate::py_module_available("tensorflow")) {
    reticulate::py_install(c("MarkupSafe==2.1.5", 
                             "absl-py==2.1.0",
                             "astunparse==1.6.3",
                             "cachetools==5.3.3",
                             "certifi==2024.6.2",
                             "charset-normalizer==3.3.2" ),
                           envname = "r-web_interface",
                           method = c("virtualenv"),
                           ignore_installed = FALSE)
  }
}

install_tf_dependencies_2 <- function() {
  if (!reticulate::py_module_available("tensorflow")) {
    reticulate::py_install(c("flatbuffers==24.3.25", 
                             "gast==0.5.4", 
                             "google-auth==2.29.0", 
                             "google-auth-oauthlib==1.2.0", 
                             "google-pasta==0.2.0" ),
                           envname = "r-web_interface",
                           method = c("virtualenv"),
                           ignore_installed = FALSE)
  }
}

install_tf_dependencies_3 <- function() {
  if (!reticulate::py_module_available("tensorflow")) {
    reticulate::py_install(c("grpcio==1.64.0", 
                             "h5py==3.11.0", 
                             "idna==3.7", 
                             "keras==2.15.0", 
                             "libclang==18.1.1"),
                           envname = "r-web_interface",
                           method = c("virtualenv"),
                           ignore_installed = FALSE)
  }
}

install_tf_dependencies_4 <- function() {
  if (!reticulate::py_module_available("tensorflow")) {
    reticulate::py_install(c("markdown==3.6", 
                             "ml-dtypes==0.2.0", 
                             "oauthlib==3.2.2", 
                             "opt-einsum==3.3.0", 
                             "packaging==24.0", 
                             "protobuf==4.25.3", 
                             "pyasn1==0.6.0", 
                             "pyasn1-modules==0.4.0"),
                           envname = "r-web_interface",
                           method = c("virtualenv"),
                           ignore_installed = FALSE)
  }
}

install_tf_dependencies_5 <- function() {
  if (!reticulate::py_module_available("tensorflow")) {
    reticulate::py_install(c("requests==2.32.3", 
                             "requests-oauthlib==2.0.0", 
                             "rsa==4.9", 
                             "six==1.16.0", 
                             "typing-extensions==4.12.1", 
                             "urllib3==2.2.1 ", 
                             "werkzeug==3.0.3", 
                             "wrapt==1.14.1"),
                           envname = "r-web_interface",
                           method = c("virtualenv"),
                           ignore_installed = FALSE)
  }
}

# Delay installation of tensorflow
install_tf <- function() {
  
  if (!reticulate::py_module_available("tensorflow")) {
    reticulate::py_install(c("tensorflow==2.15.0"),
                           envname = "r-web_interface",
                           method = c("virtualenv"),
                           ignore_installed = FALSE)
  }
}


# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Auto Visual Inference"),
  dashboardSidebar(
    
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    fluidRow(
      box(htmltools::includeMarkdown("www/markdown/upload.md"),
          textOutput("data_status"),
          fileInput("upload", NULL, accept = ".csv", buttonLabel = "Upload CSV", multiple = FALSE),
          htmltools::includeMarkdown("www/markdown/use_example.md"),
          actionButton("example_1", "Use Example Dataset 1"),
          actionButton("example_2", "Use Example Dataset 2"),
          width = 5,
          height = 250),
      box(
          htmltools::includeMarkdown("www/markdown/variable_select.md"),
          fluidRow(
            column(4, selectInput("var_fitted", "Fitted values", c(".fitted"))),
            column(4, selectInput("var_resid", "Residuals", c(".resid"))),
            column(4, selectInput("var_sample", "Labels of residual plots", c(".sample")))
            ),
        width = 5,
        height = 250),
      box(
        htmltools::includeMarkdown("www/markdown/run.md"),
        p("🟡 TensorFlow Status: Please wait, TensorFlow is initializing", id = "tf_status"),
        br(),
        shinyjs::disabled(actionButton("run", "Run", icon = icon("play"), width = "100%")),
        textOutput("try_tf"),
        width = 2,
        height = 250)
      ),
    fluidRow(
      box(h2("Visual signal strength"),
          shinycssloaders::withSpinner(DT::DTOutput("vss_table", height = 600)),
          width = 5,
          height = 700),
      box(h2("Residual plot"), 
          shinycssloaders::withSpinner(plotOutput("residual_plots", width = "100%", height = "600px")),
          width = 7,
          height = 700)
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  plot_path <- list()
  
  keras_model <- NULL
  
  current_data <- reactiveVal(NULL)
  data_info <- reactiveVal("🟡 Data Status: no dataset available.")
  
  upload_install_counter <- 0L
  

# Get example data --------------------------------------------------------

  observeEvent(input$example_1, {
    data_info("🟢 Data Status: using example 1.")
    current_data(readr::read_csv("test.csv"))
  })
  
  observeEvent(input$example_2, {
    data_info("🟢 Data Status: using example 2.")
    current_data(readr::read_csv("test2.csv"))
  })
  
# Get upload data ---------------------------------------------------------

  observeEvent(input$upload, {
    # Read the data from the uploaded file
    data_info("🟢 Data Status: using uploaded data.")
    current_data(readr::read_csv(input$upload$datapath))
  })  

# Get current data ---------------------------------------------------------

  residual_data <- reactive({
    
    # Not run until data has been uploaded
    req(!is.null(current_data()))
    
    current_data()
  }) 
  

# Click upload button ------------------------------------------------------

  observeEvent(input$upload, {
    
    upload_install_counter <<- upload_install_counter + 1
    
    if (upload_install_counter == 1L) {
      install_tf_dependencies()
    }
    
    if (upload_install_counter == 2L) {
      install_tf_dependencies_2()
    }
    
    if (upload_install_counter == 3L) {
      install_tf_dependencies_3()
    }
    
    if (upload_install_counter == 4L) {
      install_tf_dependencies_4()
    }
    
    if (upload_install_counter == 5L) {
      install_tf_dependencies_5()
    }
    
    if (upload_install_counter > 5L) {
      install_tf()
    }
  })  
  

# Data status -------------------------------------------------------------
  
  output$data_status <- renderText({
    data_info()
  })
    


# Update dropdown selection -----------------------------------------------
    
  observeEvent(current_data(), {
    this_data <- residual_data()
    updateSelectInput(session, 
                      "var_fitted", 
                      choices = names(this_data),
                      selected = ".fitted")
    updateSelectInput(session, 
                      "var_resid", 
                      choices = names(this_data),
                      selected = ".resid")
    if (".sample" %in% names(this_data)) {
      updateSelectInput(session, 
                        "var_sample", 
                        choices = c("-----", names(this_data)),
                        selected = ".sample")
    } else {
      updateSelectInput(session, 
                        "var_sample", 
                        choices = c("-----", names(this_data)),
                        selected = "-----")
    }
  })
  

# Show residual plot ------------------------------------------------------
  
  output$residual_plots <- renderPlot({
    
    # Not run until data has been uploaded
    req(input$run)
    
    # Get residuals
    this_data <- isolate(residual_data())
    
    fitted_name <- isolate(input$var_fitted)
    resid_name <- isolate(input$var_resid)
    sample_name <- isolate(input$var_sample)
    
    multiple_plots <- FALSE
    
    if (sample_name %in% names(this_data)) {
      if (length(unique(this_data[[sample_name]])) > 1) {
        multiple_plots <- TRUE
      }
    } else {
      if (sample_name != ".sample") {
        print("User select incorrect sample name")
      }
    }
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    this_data$.sample <- this_data[[sample_name]]
    
    this_title <- ""
    if (length(unique(this_data$.sample)) > 20) {
      this_data <- this_data %>%
        filter(.sample %in% unique(this_data$.sample)[1:20])
      this_title <- "Too many plot labels. Only shown the first 20."
    }

    # Determine if the incoming data has more than one plot
    if (multiple_plots) {
      p <- VI_MODEL$plot(this_data,
                         size = 1,
                         theme = theme_light(),
                         remove_axis = TRUE,
                         remove_grid_line = TRUE) +
        facet_wrap(~.sample, labeller = label_both) +
        ggtitle(this_title)
    } else {
      p <- AUTO_VI$plot_resid(this_data,
                              size = 1,
                              theme = theme_light())
    }
    
    return(p)
  })

# Show vss ----------------------------------------------------------------

  output$vss_table <- DT::renderDT(server = FALSE, {
    
    req(input$run)
    req(!is.null(keras_model))
    
    this_data <- isolate(residual_data())
    
    fitted_name <- isolate(input$var_fitted)
    resid_name <- isolate(input$var_resid)
    sample_name <- isolate(input$var_sample)
    
    this_data$.fitted <- this_data[[fitted_name]]
    this_data$.resid <- this_data[[resid_name]]
    this_data$.sample <- this_data[[sample_name]]
    
    # Manually makes a lineup
    if (!".sample" %in% names(this_data)) {
      this_data$.sample <- 1
    }
    
    vss <- c()
    sorted_sample <- sort(unique(this_data$.sample))
    for (this_sample in sorted_sample) {
      current_data <- this_data %>%
        filter(.sample == this_sample) %>%
        select(.fitted, .resid)
      current_auxiliary <- AUTO_VI$auxiliary(current_data)
      current_p <- AUTO_VI$plot_resid(current_data)
      if (is.null(keras_model)) {
        vss <- c(vss, NA)
      } else {
        vss <- c(vss, AUTO_VI$vss(current_p, 
                                  auxiliary = current_auxiliary,
                                  keras_model = keras_model,
                                  node_index = 1L) %>%
                   unlist())
      }
    }
    
    data.frame(.sample = sorted_sample,
               vss = vss) %>%
      mutate(.sample = factor(.sample)) %>%
      mutate(vss = format(vss, digits = 3)) %>% 
      DT::datatable(rownames = FALSE, extensions = "Buttons", 
                    options = list(paging = TRUE,
                                   scrollX = TRUE, 
                                   searching = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf'),
                                   pageLength = 10, 
                                   lengthMenu=c(3, 5, 10)))
  })
  

# TF status ---------------------------------------------------------------
  
  output$try_tf <- renderText({
    tf <- reticulate::import("tensorflow")
    keras <- tf$keras
    keras_model <<- keras$models$load_model("www/model")
    print(keras_model$summary())
    shinyjs::runjs("document.getElementById('tf_status').innerHTML = '🟢 TensorFlow Status: Good to go!'")
    shinyjs::enable("run")
    return("")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
