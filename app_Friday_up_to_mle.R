# Load required packages
required_pkgs <- c("shiny", "devtools", "Rcpp", "sigex")
pkg_status <- sapply(required_pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (pkg == "sigex") {
      devtools::install_github("tuckermcelroy/sigex")
    } else {
      install.packages(pkg)
    }
  }
  library(pkg, character.only = TRUE)
  return(TRUE)
})

# STEP 1: Load and Transform Data
dataInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Step 1: Load and Transform Data"),
    verbatimTextOutput(ns("pkg_status")),
    radioButtons(ns("use_builtin"), "Data Source:",
                 choices = c("Use built-in airline data", "Upload CSV")),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'Upload CSV'", ns("use_builtin")),
      fileInput(ns("csv_file"), "Upload CSV File", accept = ".csv"),
      uiOutput(ns("col_selector"))
    ),
    numericInput(ns("start_year"), "Start Year", value = 1949),
    numericInput(ns("start_month"), "Start Month", value = 1, min = 1, max = 12),
    selectInput(ns("transform"), "Transformation", choices = c("log", "none")),
    actionButton(ns("load_btn"), "Load and Transform"),
    tags$hr(),
    plotOutput(ns("ts_plot"))
  )
}

dataInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data.ts <- reactiveVal(NULL)
    dataALL.ts <- reactiveVal(NULL)
    uploaded_data <- reactiveVal(NULL)
    start.time <- reactiveVal(NULL)
    
    output$pkg_status <- renderText({
      paste("All required packages loaded successfully:",
            paste(required_pkgs, collapse = ", "))
    })
    
    observeEvent(input$csv_file, {
      df <- read.csv(input$csv_file$datapath)
      uploaded_data(df)
      updateSelectInput(session, "selected_col", choices = names(df))
    })
    
    output$col_selector <- renderUI({
      req(uploaded_data())
      selectInput(ns("selected_col"), "Choose column for analysis:", choices = names(uploaded_data()))
    })
    
    observeEvent(input$load_btn, {
      req(input$start_year, input$start_month, input$transform)
      
      series <- if (input$use_builtin == "Use built-in airline data") {
        matrix(AirPassengers, ncol = 1)
      } else {
        df <- uploaded_data()
        matrix(df[[input$selected_col]], ncol = 1)
      }
      
      start <- c(input$start_year, input$start_month)
      start.time(start)
      
      dataALL.ts.val <- sigex.load(series, start, 12, "Series")
      data.ts.val <- sigex.prep(dataALL.ts.val, input$transform, FALSE, 1, NULL)
      
      dataALL.ts(dataALL.ts.val)
      data.ts(data.ts.val)
    })
    
    output$ts_plot <- renderPlot({
      req(data.ts())
      ts.plot(data.ts(), main = "Transformed Time Series", ylab = "Value")
    })
    
    return(list(
      data.ts = data.ts,
      dataALL.ts = dataALL.ts,
      start.time = start.time,
      period = reactive(12)
    ))
  })
}

# STEP 2: Spectral
spectralUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Step 2: Spectral Analysis"),
    radioButtons(ns("diff_option"), "Data Type:",
                 choices = c("Raw" = "FALSE", "Growth rate" = "TRUE")),
    plotOutput(ns("spec_plot"))
  )
}

spectralServer <- function(id, data.ts, period) {
  moduleServer(id, function(input, output, session) {
    output$spec_plot <- renderPlot({
      req(data.ts())
      sigex.specar(data.ts(), as.logical(input$diff_option), 1, period())
    })
  })
}

# STEP 3: Calendar
calendarUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Step 3: TD + Easter Regressors"),
    actionButton(ns("compute"), "Generate TD & Easter Regressors"),
    verbatimTextOutput(ns("summary"))
  )
}

calendarServer <- function(id, data.ts, start.time, period) {
  moduleServer(id, function(input, output, session) {
    td.weights <- reactiveVal(NULL)
    easter.regs <- reactiveVal(NULL)
    
    observeEvent(input$compute, {
      req(data.ts(), start.time())
      T <- dim(data.ts())[1]
      
      start.date <- c(start.time()[2], 1, start.time()[1])
      end.time <- start.time()
      end.time[1] <- start.time()[1] + floor((T - 1) / period())
      end.time[2] <- ((start.time()[2] - 1 + (T - 1)) %% period()) + 1
      end.day <- if (end.time[2] == 12) 31 else date2day(end.time[2] + 1, 1, end.time[1]) - date2day(end.time[2], 1, end.time[1])
      end.date <- c(end.time[2], end.day, end.time[1])
      
      td <- sapply(1:7, function(i) daily2monthly(rep(1, T * 35), start.date, i)[, 1])
      td <- td[1:T, ]
      td.weights(td[, 2:7] - td[, 1])
      
      easter.path <- system.file("extdata", "easter500.txt", package = "sigex")
      easter.dates <- read.table(easter.path)
      easter.reg <- gethol(easter.dates, 1, 0, start.date, end.date)
      easter.mat <- sapply(1:7, function(i) daily2monthly(easter.reg, start.date, i)[, 1])
      easter.regs(rowSums(easter.mat))
    })
    
    output$summary <- renderPrint({
      req(td.weights(), easter.regs())
      cat("TD weights matrix:", dim(td.weights()), "\n")
      cat("Easter regressor length:", length(easter.regs()), "\n")
    })
    
    return(list(td.weights = td.weights, easter.regs = easter.regs))
  })
}

# STEP 4: Model
modelUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Step 4: Model Construction"),
    actionButton(ns("construct_model"), "Construct SARIMA Model"),
    verbatimTextOutput(ns("model_summary"))
  )
}

modelServer <- function(id, data.ts, start.time, period, td.weights, easter.regs) {
  moduleServer(id, function(input, output, session) {
    mdl <- reactiveVal(NULL)
    
    observeEvent(input$construct_model, {
      req(data.ts(), start.time(), td.weights(), easter.regs())
      delta.seas <- c(1, -1, rep(0, period() - 2), -1, 1)
      model <- sigex.add(NULL, 1, "sarma", c(0, 1, 0, 1, 12), NULL, "process", delta.seas)
      model <- sigex.meaninit(model, data.ts(), 0)
      for (i in 1:6) {
        model <- sigex.reg(model, 1, ts(td.weights()[, i, drop = FALSE], start = start.time(), frequency = period(), names = paste("TD", i)))
      }
      model <- sigex.reg(model, 1, ts(as.matrix(easter.regs()), start = start.time(), frequency = period(), names = "Easter"))
      mdl(model)
    })
    
    output$model_summary <- renderPrint({
      req(mdl(), data.ts(), td.weights(), easter.regs())
      cat("✔ Model constructed successfully.\n\n")
      cat("Observations:", dim(data.ts())[1], "\n")
      cat("Regressors: TD (6) + Easter (1)\n")
    })
    
    return(mdl)
  })
}

# STEP 5: MLE Estimation with progress bar
mleUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Step 5: MLE Estimation and Diagnostics"),
    actionButton(ns("estimate_btn"), "Run MLE Estimation"),
    verbatimTextOutput(ns("progress_msg")),
    verbatimTextOutput(ns("likelihood")),
    verbatimTextOutput(ns("eigenvalues"))
  )
}

mleServer <- function(id, data.ts, mdl) {
  moduleServer(id, function(input, output, session) {
    par.mle <- reactiveVal(NULL)
    psi.mle <- reactiveVal(NULL)
    hess <- reactiveVal(NULL)
    
    observeEvent(input$estimate_btn, {
      req(data.ts(), mdl())
      
      withProgress(message = "Estimating model parameters...", value = 0.2, {
        incProgress(0.2, detail = "Preparing constraints")
        constraint <- rbind(NULL, sigex.constrainreg(mdl(), data.ts(), list(seq(2, 6)), NULL))
        
        incProgress(0.3, detail = "Initializing parameters")
        par0 <- sigex.default(mdl(), data.ts(), constraint)
        
        incProgress(0.3, detail = "Running optimization (MLE)")
        fit <- sigex.mlefit(data.ts(), par0, constraint, mdl(), "bfgs", debug = FALSE)
        
        incProgress(0.1, detail = "Finalizing output")
        psi.final <- sigex.eta2psi(fit[[1]]$par, constraint)
        
        par.mle(fit[[2]])
        psi.mle(psi.final)
        hess(fit[[1]]$hessian)
      })
      
      output$progress_msg <- renderText("✔ MLE completed successfully.")
    })
    
    output$likelihood <- renderPrint({
      req(psi.mle(), mdl(), data.ts())
      cat("Log-likelihood:\n")
      print(sigex.lik(psi.mle(), mdl(), data.ts()))
    })
    
    output$eigenvalues <- renderPrint({
      req(hess())
      cat("Eigenvalues of Hessian matrix:\n")
      print(eigen(hess())$values)
    })
  })
}

# UI + SERVER
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Step 1: Load + Transform", dataInputUI("step1")),
    tabPanel("Step 2: Spectral Analysis", spectralUI("step2")),
    tabPanel("Step 3: TD + Easter Regressors", calendarUI("step3")),
    tabPanel("Step 4: Model Construction", modelUI("step4")),
    tabPanel("Step 5: MLE Estimation", mleUI("step5"))
  )
)

server <- function(input, output, session) {
  step1 <- dataInputServer("step1")
  step3 <- calendarServer("step3", data.ts = step1$data.ts, start.time = step1$start.time, period = step1$period)
  spectralServer("step2", data.ts = step1$data.ts, period = step1$period)
  step4 <- modelServer("step4", data.ts = step1$data.ts, start.time = step1$start.time,
                       period = step1$period, td.weights = step3$td.weights, easter.regs = step3$easter.regs)
  mleServer("step5", data.ts = step1$data.ts, mdl = step4)
}

shinyApp(ui, server)
