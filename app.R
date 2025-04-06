# app.R  —  same logic, improved layout
library(shiny)

required_pkgs <- c("devtools", "Rcpp", "sigex")

ui <- fluidPage(
  titlePanel("Sigex workflow (Steps 1‑4)"),
  
  ## ── Step 1 · package availability ───────────────────────────────
  fluidRow(
    column(12, uiOutput("pkg_status"))
  ),
  tags$hr(),
  
  ## ── Step 2 · CSV + column picker ────────────────────────────────
  fluidRow(
    column(4,
           fileInput("csv", "Upload CSV file", accept = ".csv"),
           uiOutput("varUI"),
           verbatimTextOutput("preview")
    )
  ),
  tags$hr(),
  
  ## ── Step 3 · sigex.load  ----------------------------------------
  fluidRow(
    column(3,
           numericInput("yr",  "Start year",  1949, min = 0),
           numericInput("mon", "Start month", 1,    min = 1, max = 12),
           numericInput("per", "Period",      12,   min = 1),
           actionButton("run", "Run sigex.load",
                        class = "btn-primary", width = "100%")
    ),
    column(9,
           conditionalPanel("input.run > 0",
                            plotOutput("sigexPlot", height = 250))
    )
  ),
  tags$hr(),
  
  ## ── Step 4 · transform + spectrum -------------------------------
  fluidRow(
    column(3,
           radioButtons("logTF", "Transformation:",
                        c("log", "none"), "log", inline = TRUE),
           numericInput("subseries", "Sub‑series index", 1, min = 1),
           actionButton("prep", "Run sigex.prep + spectrum",
                        class = "btn-warning", width = "100%")
    ),
    column(9,
           conditionalPanel("input.prep > 0",
                            plotOutput("specPlot", height = 250))
    )
  ),
  tags$hr(),
  
  verbatimTextOutput("status")
)

## ── SERVER (unchanged from previous version) -----------------------
server <- function(input, output, session) {
  
  output$pkg_status <- renderUI({
    ok <- vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
    if (all(ok)) {
      HTML(sprintf(
        '<span style="color:darkgreen; font-weight:bold;">
           &#10004; All required packages are installed:<br>%s
         </span>',
        paste(required_pkgs, collapse = ", ")
      ))
    } else {
      HTML(sprintf(
        '<span style="color:red; font-weight:bold;">
           &#10006; Missing packages:<br>%s<br>
           Please install them and restart the app.
         </span>',
        paste(required_pkgs[!ok], collapse = ", ")
      ))
    }
  })
  
  dataFile <- reactive({
    req(input$csv)
    read.csv(input$csv$datapath, stringsAsFactors = FALSE)
  })
  
  output$varUI <- renderUI({
    req(dataFile())
    selectInput("column", "Choose column to analyse",
                choices = names(dataFile()),
                selected = names(dataFile())[1])
  })
  
  output$preview <- renderPrint({
    req(dataFile(), input$column)
    head(dataFile()[[input$column]])
  })
  
  ## Step 3
  dataALL_ts <- reactiveVal(NULL)
  output$sigexPlot <- renderPlot({
    req(input$run, dataFile(), input$column)
    airline    <- dataFile()[[input$column]]
    xmat       <- as.matrix(airline)
    start.time <- c(input$yr, input$mon)
    period     <- input$per
    
    ts_out <- sigex::sigex.load(
      xmat, start.time, period, c("Airline"), TRUE
    )
    dataALL_ts(ts_out)
  })
  
  ## Step 4
  output$specPlot <- renderPlot({
    req(input$prep, dataALL_ts())
    data_ts <- sigex::sigex.prep(
      dataALL_ts(),
      input$logTF,
      FALSE,                # aggregate
      input$subseries,
      NULL,                 # range
      TRUE
    )
    par(mfrow = c(1, 1))
    sigex::sigex.specar(data_ts, FALSE, 1, input$per)
  })
  
  output$status <- renderText({
    if (input$prep == 0) {
      "Steps 1‑3 complete. Choose transform & sub‑series, then click the orange button."
    } else {
      paste0("sigex.prep (transform = ", input$logTF,
             ", subseries = ", input$subseries,
             ") and spectrum plot completed.")
    }
  })
}

shinyApp(ui, server)
