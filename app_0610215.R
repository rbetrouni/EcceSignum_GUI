# ───────────────────────────────────────────────────────────────
# Load required packages
# ───────────────────────────────────────────────────────────────
required_pkgs <- c("shiny", "devtools", "Rcpp", "sigex","ggplot2")
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
    htmlOutput(ns("pkg_status")),
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
    htmlOutput(ns("load_message")),
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
    
    output$pkg_status <- renderUI({
      tags$div(
        style = "color: #155724; background: #d4edda; border: 1px solid #c3e6cb; border-radius:8px; padding:10px; display: flex; align-items: center; font-size: 16px;",
        tags$span(style = "font-size:24px; margin-right:10px;", "\u2705"), # green check emoji
        tags$span(
          tags$b("All required packages loaded successfully!"),
          tags$br(),
          tags$span(
            style="font-size:14px; color:#155724;",
            paste("Loaded:", paste(required_pkgs, collapse = ", "))
          )
        )
      )
    })
    
    
    observeEvent(input$csv_file, {
      df <- read.csv(input$csv_file$datapath)
      uploaded_data(df)
      updateSelectInput(session, "selected_col", choices = names(df))
    })
    
    output$col_selector <- renderUI({
      req(uploaded_data())
      selectInput(ns("selected_col"),
                  "Choose column for analysis:",
                  choices = names(uploaded_data()))
    })
    
    output$load_message <- renderUI({
      # Only show after a column is selected or built-in data is chosen
      if (input$use_builtin == "Use built-in airline data") {
        n_records <- length(AirPassengers)
        y_col <- "Passengers"
      } else {
        # Only show when both data and column are selected
        if (is.null(uploaded_data()) || is.null(input$selected_col)) return(NULL)
        n_records <- sum(!is.na(uploaded_data()[[input$selected_col]]))
        y_col <- input$selected_col
      }
      tags$div(
        style = "color: #155724; background: #d4edda; border: 1px solid #c3e6cb; border-radius:8px; padding:10px; display: flex; align-items: center; font-size: 15px; margin-bottom: 10px;",
        tags$span(style = "font-size:20px; margin-right:8px;", "\u2705"),
        paste0("Loaded ", n_records, " records from '", y_col, "'.")
      )
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
      req(input$transform)
      
      ts_obj <- data.ts()
      df <- data.frame(
        time = as.numeric(time(ts_obj)),
        value = as.numeric(ts_obj)
      )
      
      # Dynamically get Y label
      y_col <- if (input$use_builtin == "Use built-in airline data") {
        "Passengers"
      } else {
        req(input$selected_col) # ensure not NULL
        input$selected_col
      }
      
      plot_title <- if (input$transform == "log") {
        "Log-Transformed Time Series"
      } else {
        "Raw Time Series"
      }
      
      subtitle <- if (input$transform == "log") {
        paste("Log transformation applied to", y_col)
      } else {
        paste("No transformation: showing", y_col)
      }
      
      y_axis_label <- if (input$transform == "log") {
        paste0("Log(", y_col, ")")
      } else {
        y_col
      }
      
      library(ggplot2)
      ggplot(df, aes(x = time, y = value)) +
        geom_line(color = "#0072B2", size = 1.1) +
        labs(
          title = plot_title,
          subtitle = subtitle,
          x = "Time",
          y = y_axis_label
        ) +
        theme_minimal(base_size = 16) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 13)
        )
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
                 choices = c("Raw" = "FALSE", "Growth rate" = "TRUE"),
                 selected = character(0)    # 👈 No default selected
                 ),
    actionButton(ns("run_spec"), "Run Spectral Analysis"),   # 👈 RB
    plotOutput(ns("spec_plot"))
  )
}


spectralServer <- function(id, data.ts, period) {
  moduleServer(id, function(input, output, session) {
    # Create a trigger that updates when the button is clicked or diff_option is changed
    spec_trigger <- reactiveVal(0)
    observeEvent(input$run_spec, { spec_trigger(spec_trigger() + 1) })
    #observeEvent(input$diff_option, { spec_trigger(spec_trigger() + 1) }) # optional: rerun when radio changes
    
    output$spec_plot <- renderPlot({
      req(data.ts())
      req(input$diff_option != "" && !is.null(input$diff_option))   # 👈 User must select!
      req(spec_trigger() > 0)  # Require the button has been pressed
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
    #verbatimTextOutput(ns("summary"))
    htmlOutput(ns("summary"))   #RB
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
      eday <- if (et[2] == 12) 31 else 
        date2day(et[2] + 1, 1, et[1]) - date2day(et[2], 1, et[1])
      ed <- c(et[2], eday, et[1])
      
      tdmat <- sapply(1:7, function(i)
        daily2monthly(rep(1, T*35), sd, i)[,1])
      tdmat <- tdmat[1:T, ]
      td.weights(tdmat[,2:7] - tdmat[,1])
      
      ep <- system.file("extdata","easter500.txt",package="sigex")
      edates <- read.table(ep)
      er <- gethol(edates, 1, 0, sd, ed)
      emat <- sapply(1:7, function(i) 
        daily2monthly(er, sd, i)[,1])
      easter.regs(rowSums(emat))
    })
    
    # output$summary <- renderPrint({
    #   req(td.weights(), easter.regs())
    #   cat("TD weights dims:", dim(td.weights()), "\n")
    #   cat("Easter reg length:", length(easter.regs()), "\n")
    # })
    
    output$summary <- renderUI({
      req(td.weights(), easter.regs())
      tdmat <- td.weights()
      eastervec <- easter.regs()
      
      # Compute summaries for each TD regressor
      td_summary <- apply(tdmat, 2, function(x) {
        sprintf("Mean: %.2f, SD: %.2f, Min: %.2f, Max: %.2f", mean(x), sd(x), min(x), max(x))
      })
      
      # Make a small preview table (first 6 rows)
      td_preview <- as.data.frame(head(tdmat))
      names(td_preview) <- paste0("TD_", 1:6)
      
      tags$div(
        style = "color: #155724; background: #d4edda; border: 1px solid #c3e6cb; border-radius:8px; padding:12px; margin-bottom: 8px;",
        tags$span(style = "font-size:22px; margin-right:10px;", "\u2705"),
        tags$b("TD & Easter Regressors generated successfully!"),
        tags$hr(),
        tags$div(
          style = "margin-bottom: 8px;",
          paste("• TD (Trading Day) weights matrix: ", nrow(tdmat), " rows x ", ncol(tdmat), " columns."),
          tags$br(),
          lapply(seq_along(td_summary), function(i) {
            tags$div(paste0("  TD_", i, ": ", td_summary[[i]]))
          }),
          tags$br(),
          tags$div(paste("• Easter regressor: length =", length(eastervec),
                         "| Mean:", round(mean(eastervec),2),
                         "SD:", round(sd(eastervec),2),
                         "Min:", min(eastervec),
                         "Max:", max(eastervec))),
          tags$br(),
          tags$div(tags$b("TD matrix (first 6 rows):")),
          tags$pre(paste(capture.output(print(td_preview)), collapse = "\n")) # Fixed here
        )
      )
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
        rts <- ts(td.weights()[,i,drop=FALSE],
                  start = start.time(),
                  frequency = period())
        m <- sigex.reg(m, 1, rts)
      }
      ers <- ts(as.matrix(easter.regs()),
                start = start.time(),
                frequency = period())
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
# STEP 5: MLE Estimation + Diagnostics (with “Load saved MLE” option)
# ───────────────────────────────────────────────────────────────
mleUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Step 5: MLE Estimation and Diagnostics"),
    
    # ─── NEW: Checkbox + text input to load saved MLE from disk ───
    checkboxInput(ns("use_saved"),
                  "Load saved MLE from file (development)",
                  value = FALSE),
    textInput(ns("saved_path"),
              "Saved MLE path (full .RData)",
              value = "C:/path/to/saved_mle.RData"),
    tags$small("Your .RData must contain two objects: ",
               "`psi_saved` (vector) and `hess_saved` (matrix)."),
    tags$hr(),
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
      
      if (input$use_saved) {
        # ─────────────────────────────────────────────────────────
        # Load saved MLE from an external .RData file
        # ─────────────────────────────────────────────────────────
        saved_file <- input$saved_path
        if (!file.exists(saved_file)) {
          output$progress_msg <- renderText({
            paste0("❌ File not found: ", saved_file)
          })
          return()
        }
        # Assume that .RData contains objects `psi_saved` and `hess_saved`
        e <- tryCatch({
          load(saved_file, envir = environment())
          TRUE
        }, error = function(err) {
          FALSE
        })
        if (!e || (!exists("psi_saved") || !exists("hess_saved"))) {
          output$progress_msg <- renderText({
            "❌ .RData does not contain `psi_saved` and `hess_saved`."
          })
          return()
        }
        # Assign to reactiveVals
        psi.mle(psi_saved)
        hess(hess_saved)
        output$progress_msg <- renderText("✔ Loaded saved MLE from file.")
        
      } else {
        # ─────────────────────────────────────────────────────────
        # Full MLE branch (exactly as before)
        # ─────────────────────────────────────────────────────────
        withProgress(message="Estimating parameters...", value=0.2, {
          incProgress(0.2, detail="Constraints")
          constr <- rbind(NULL,
                          sigex.constrainreg(mdl(), data.ts(),
                                             list(seq(2,6)), NULL))
          incProgress(0.3, detail="Init")
          p0 <- sigex.default(mdl(), data.ts(), constr)
          incProgress(0.3, detail="Optimizing")
          fit <- sigex.mlefit(data.ts(), p0, constr, mdl(),
                              "bfgs", debug=FALSE)
          incProgress(0.1, detail="Finalizing")
          pf <- sigex.eta2psi(fit[[1]]$par, constr)
          psi.mle(pf)
          hess(fit[[1]]$hessian)
        })
        output$progress_msg <- renderText("✔ MLE completed.")
      }
    })
    
    output$likelihood <- renderPrint({
      req(psi.mle(), mdl(), data.ts())
      cat("Log-likelihood:\n")
      print(sigex.lik(psi.mle(), mdl(), data.ts()))
    })
    
    output$eigenvalues <- renderPrint({
      req(hess())
      cat("Hessian eigenvalues:\n")
      print(eigen(hess())$values)
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
    plotOutput(ns("resid_plot")),   # residual time‐series plot
    plotOutput(ns("acf_plot"))      # ACF plot
  )
}

residServer <- function(id, data.ts, mdl, psi.mle) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run_resid, {
      req(data.ts(), mdl(), psi.mle())
      
      resid.mat <- sigex.resid(psi.mle(), mdl(), data.ts())[[1]]
      
      resid.ts <- sigex.load(
        t(resid.mat),
        start(data.ts()),
        frequency(data.ts()),
        colnames(data.ts())
      )
      
      output$resid_plot <- renderPlot({
        plot(resid.ts,
             main = "Residual Series",
             ylab = "Residuals")
      })
      
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
    tabPanel("Step 3: TD + Easter Regressors",  calendarUI("step3")),
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
