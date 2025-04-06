# app.R  —  Sigex workflow with explicit package list & labelled spectra
library(shiny)
library(ggplot2)
library(sigex)

required_pkgs <- c("devtools", "Rcpp", "sigex")

ui <- fluidPage(
  titlePanel("Sigex workflow – univariate SARIMA demo"),
  
  ## Step 1 · package availability ----------------------------------
  uiOutput("pkg_status"), tags$hr(),
  
  ## Step 2 · CSV upload & column picker ----------------------------
  fluidRow(
    column(4,
           fileInput("csv", "1 · Upload a CSV file", accept = ".csv"),
           uiOutput("varUI"),
           verbatimTextOutput("preview")
    )
  ), tags$hr(),
  
  ## Step 3 · metadata + Run sigex.load -----------------------------
  fluidRow(
    column(
      3,
      h4("2 · Enter metadata"),
      helpText("We will fit a univariate SARIMA model to the selected series."),
      numericInput("yr",  "Start year",  1949, min = 0),
      numericInput("mon", "Start month", 1,    min = 1, max = 12),
      numericInput("per", "Seasonal period", 12, min = 1),
      actionButton("run", "Run sigex.load (plot series)",
                   class = "btn-primary", width = "100%")
    ),
    column(
      9,
      conditionalPanel(
        "input.run > 0",
        plotOutput("sigexPlot", height = 280)
      )
    )
  ), tags$hr(),
  
  ## Step 4 · transform & spectral diagnostics ----------------------
  fluidRow(
    column(
      3,
      h4("3 · Transform & spectral diagnostics"),
      radioButtons("logTF", "Transformation",
                   c("log", "none"), "log", inline = TRUE),
      numericInput("subseries", "Sub‑series index", 1, min = 1),
      actionButton("prep", "Run sigex.prep + spectra",
                   class = "btn-warning", width = "100%")
    ),
    column(
      9,
      conditionalPanel(
        "input.prep > 0",
        tags$div(
          strong("Panel 1 – transformed series"),
          br(),
          strong("Panel 2 – raw‑data spectrum"),
          br(),
          strong("Panel 3 – growth‑rate spectrum")
        ),
        plotOutput("specPlot", height = 750)
      )
    )
  ),
  tags$hr(),
  verbatimTextOutput("status")
)

server <- function(input, output, session) {
  
  ## ---- Step 1 : package status ------------------------------------
  output$pkg_status <- renderUI({
    ok <- vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
    if (all(ok)) {
      HTML(sprintf(
        '<span style="color:darkgreen;font-weight:bold;">
           &#10004; All required packages are installed:<br>%s
         </span>',
        paste(required_pkgs, collapse = ", ")
      ))
    } else {
      HTML(sprintf(
        '<span style="color:red;font-weight:bold;">
           &#10006; Missing packages: %s</span>',
        paste(required_pkgs[!ok], collapse = ", ")
      ))
    }
  })
  
  ## ---- Step 2 ------------------------------------------------------
  dataFile <- reactive({
    req(input$csv)
    read.csv(input$csv$datapath, stringsAsFactors = FALSE)
  })
  
  output$varUI <- renderUI({
    req(dataFile())
    selectInput("column", "Choose series column",
                choices = names(dataFile()),
                selected = names(dataFile())[1])
  })
  
  output$preview <- renderPrint({
    req(dataFile(), input$column)
    head(dataFile()[[input$column]])
  })
  
  ## ---- Step 3 : sigex.load + calendar plot ------------------------
  dataALL_ts <- reactiveVal(NULL)
  
  output$sigexPlot <- renderPlot({
    req(input$run, dataFile(), input$column)
    
    xvec       <- dataFile()[[input$column]]
    xmat       <- as.matrix(xvec)
    start.time <- c(input$yr, input$mon)
    period     <- input$per
    
    ts_out <- sigex::sigex.load(
      xmat, start.time, period, c("Series"), FALSE
    )
    dataALL_ts(ts_out)
    
    n      <- length(xvec)
    origin <- as.Date(sprintf("%d-%02d-01", input$yr, input$mon))
    step   <- sprintf("%d months", 12 / period)
    dates  <- seq(origin, by = step, length.out = n)
    
    df <- data.frame(date = dates, y = xvec)
    ggplot(df, aes(date, y)) +
      geom_line(color = "#0072B2", linewidth = 0.6) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
      labs(x = "Year", y = input$column, title = "Raw series") +
      theme_minimal()
  })
  
  ## ---- Step 4 : prep + three stacked plots ------------------------
  output$specPlot <- renderPlot({
    req(input$prep, dataALL_ts())
    
    par(mfrow = c(3, 1))   # 3 stacked panels
    
    # Panel 1 – transformed series
    data_ts <- sigex::sigex.prep(
      dataALL_ts(),
      input$logTF,
      FALSE,
      input$subseries,
      NULL,
      TRUE
    )
    
    # Panel 2 – raw‑data spectrum
    sigex::sigex.specar(data_ts, FALSE, 1, input$per)
    
    # Panel 3 – growth‑rate spectrum
    sigex::sigex.specar(data_ts, TRUE, 1, input$per)
  })
  
  ## ---- status ------------------------------------------------------
  output$status <- renderText({
    if (input$prep == 0) {
      "Ready for Step 3 – choose transform & click the orange button."
    } else {
      paste0("sigex.prep done (transform = ", input$logTF,
             ", subseries = ", input$subseries, "). Spectra shown.")
    }
  })
}

shinyApp(ui, server)
