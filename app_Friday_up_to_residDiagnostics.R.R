# ───────────────────────────────────────────────────────────────
# Load required packages
# ───────────────────────────────────────────────────────────────
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
  TRUE
})

# ───────────────────────────────────────────────────────────────
# STEP 1: Load and Transform Data
# ───────────────────────────────────────────────────────────────
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
    data.ts      <- reactiveVal(NULL)
    dataALL.ts   <- reactiveVal(NULL)
    uploaded_data<- reactiveVal(NULL)
    start.time   <- reactiveVal(NULL)
    
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
        matrix(uploaded_data()[[input$selected_col]], ncol = 1)
      }
      
      start <- c(input$start_year, input$start_month)
      start.time(start)
      
      dataALL.ts.val <- sigex.load(series, start, 12, c("Series"))
      data.ts.val    <- sigex.prep(dataALL.ts.val, input$transform, FALSE, 1, NULL)
      
      dataALL.ts(dataALL.ts.val)
      data.ts(data.ts.val)
    })
    
    output$ts_plot <- renderPlot({
      req(data.ts())
      ts.plot(data.ts(), main = "Transformed Time Series", ylab = "Value")
    })
    
    return(list(
      data.ts    = data.ts,
      dataALL.ts = dataALL.ts,
      start.time = start.time,
      period     = reactive(12)
    ))
  })
}

# ───────────────────────────────────────────────────────────────
# STEP 2: Spectral Analysis
# ───────────────────────────────────────────────────────────────
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
      sigex.specar(data.ts(),
                   as.logical(input$diff_option),
                   1,
                   period())
    })
  })
}

# ───────────────────────────────────────────────────────────────
# STEP 3: TD + Easter Regressors
# ───────────────────────────────────────────────────────────────
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
    easter.regs<- reactiveVal(NULL)
    
    observeEvent(input$compute, {
      req(data.ts(), start.time())
      T <- dim(data.ts())[1]
      
      sd <- c(start.time()[2], 1, start.time()[1])
      et <- start.time()
      et[1] <- et[1] + floor((T - 1) / period())
      et[2] <- ((sd[1] - 1 + (T - 1)) %% period()) + 1
      eday <- if (et[2] == 12) 31 else date2day(et[2] + 1, 1, et[1]) - date2day(et[2], 1, et[1])
      ed <- c(et[2], eday, et[1])
      
      tdmat <- sapply(1:7, function(i) daily2monthly(rep(1, T*35), sd, i)[,1])
      tdmat <- tdmat[1:T, ]
      td.weights(tdmat[,2:7] - tdmat[,1])
      
      ep <- system.file("extdata","easter500.txt",package="sigex")
      edates <- read.table(ep)
      er <- gethol(edates, 1, 0, sd, ed)
      emat <- sapply(1:7, function(i) daily2monthly(er, sd, i)[,1])
      easter.regs(rowSums(emat))
    })
    
    output$summary <- renderPrint({
      req(td.weights(), easter.regs())
      cat("TD weights dims:", dim(td.weights()), "\n")
      cat("Easter reg length:", length(easter.regs()), "\n")
    })
    
    return(list(
      td.weights  = td.weights,
      easter.regs = easter.regs
    ))
  })
}

# ───────────────────────────────────────────────────────────────
# STEP 4: Model Construction
# ───────────────────────────────────────────────────────────────
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
      delta <- c(1, -1, rep(0, period() - 2), -1, 1)
      m <- sigex.add(NULL, 1, "sarma", c(0,1,0,1,12), NULL, "process", delta)
      m <- sigex.meaninit(m, data.ts(), 0)
      for (i in 1:6) {
        rts <- ts(td.weights()[,i,drop=FALSE], start = start.time(), frequency = period())
        m <- sigex.reg(m, 1, rts)
      }
      ers <- ts(as.matrix(easter.regs()), start = start.time(), frequency = period())
      m <- sigex.reg(m, 1, ers)
      mdl(m)
    })
    
    output$model_summary <- renderPrint({
      req(mdl(), data.ts(), td.weights(), easter.regs())
      cat("✔ Model constructed.\n",
          "Obs:", dim(data.ts())[1],
          " | Regressors: 6 TD + Easter\n")
    })
    
    return(mdl)
  })
}

# ───────────────────────────────────────────────────────────────
# STEP 5: MLE Estimation + Diagnostics
# ───────────────────────────────────────────────────────────────
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
    psi.mle <- reactiveVal(NULL)
    hess    <- reactiveVal(NULL)
    
    observeEvent(input$estimate_btn, {
      req(data.ts(), mdl())
      withProgress(message="Estimating parameters...", value=0.2, {
        incProgress(0.2, detail="Constraints")
        constr <- rbind(NULL, sigex.constrainreg(mdl(), data.ts(), list(seq(2,6)), NULL))
        incProgress(0.3, detail="Init")
        p0 <- sigex.default(mdl(), data.ts(), constr)
        incProgress(0.3, detail="Optimizing")
        fit <- sigex.mlefit(data.ts(), p0, constr, mdl(), "bfgs", debug=FALSE)
        incProgress(0.1, detail="Finalizing")
        pf <- sigex.eta2psi(fit[[1]]$par, constr)
        psi.mle(pf); hess(fit[[1]]$hessian)
      })
      output$progress_msg <- renderText("✔ MLE completed.")
    })
    
    output$likelihood <- renderPrint({
      req(psi.mle(), mdl(), data.ts())
      cat("Log-likelihood:\n"); print(sigex.lik(psi.mle(), mdl(), data.ts()))
    })
    
    output$eigenvalues <- renderPrint({
      req(hess())
      cat("Hessian eigenvalues:\n"); print(eigen(hess())$values)
    })
    
    return(list(psi.mle = psi.mle, hess = hess))
  })
}

# ───────────────────────────────────────────────────────────────
# STEP 6: Residual Diagnostics (with series + ACF)
# ───────────────────────────────────────────────────────────────
residUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Step 6: Residual Diagnostics"),
    actionButton(ns("run_resid"), "Compute Residuals & ACF"),
    plotOutput(ns("resid_plot")),  # residual time‐series plot
    plotOutput(ns("acf_plot"))     # ACF plot
  )
}

residServer <- function(id, data.ts, mdl, psi.mle) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run_resid, {
      req(data.ts(), mdl(), psi.mle())
      
      # 1) compute the residual matrix
      resid.mat <- sigex.resid(psi.mle(), mdl(), data.ts())[[1]]
      
      # 2) rebuild as a ts via sigex.load
      resid.ts <- sigex.load(
        t(resid.mat),
        start(data.ts()),
        frequency(data.ts()),
        colnames(data.ts())
      )
      
      # 3) plot the residual series
      output$resid_plot <- renderPlot({
        plot(resid.ts,
             main = "Residual Series",
             ylab = "Residuals")
      })
      
      # 4) plot its ACF
      output$acf_plot <- renderPlot({
        acf(resid.ts,
            lag.max = 4 * frequency(data.ts()),
            main    = "Residual ACF (up to 4 seasons)")
      })
    })
  })
}

# ───────────────────────────────────────────────────────────────
# MAIN UI & SERVER
# ───────────────────────────────────────────────────────────────
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Step 1: Load + Transform",        dataInputUI("step1")),
    tabPanel("Step 2: Spectral Analysis",      spectralUI("step2")),
    tabPanel("Step 3: TD + Easter Regressors", calendarUI("step3")),
    tabPanel("Step 4: Model Construction",     modelUI("step4")),
    tabPanel("Step 5: MLE Estimation",         mleUI("step5")),
    tabPanel("Step 6: Residual Diagnostics",   residUI("step6"))
  )
)

server <- function(input, output, session) {
  step1 <- dataInputServer("step1")
  step3 <- calendarServer("step3",
                          data.ts    = step1$data.ts,
                          start.time = step1$start.time,
                          period     = step1$period)
  spectralServer("step2",
                 data.ts = step1$data.ts,
                 period  = step1$period)
  step4 <- modelServer("step4",
                       data.ts      = step1$data.ts,
                       start.time   = step1$start.time,
                       period       = step1$period,
                       td.weights   = step3$td.weights,
                       easter.regs  = step3$easter.regs)
  step5 <- mleServer("step5",
                     data.ts = step1$data.ts,
                     mdl     = step4)
  residServer("step6",
              data.ts  = step1$data.ts,
              mdl      = step4,
              psi.mle  = step5$psi.mle)
}

shinyApp(ui, server)
