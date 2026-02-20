library(shiny)
library(survival)
library(DT)
library(geecure)
# -------------------------
# Simulated survival dataset
# -------------------------
simulate_surv_data=function(n = 200, max_follow = 5, seed = 123) {
  set.seed(seed)
  id=1:n
  trt=rbinom(n, 1, 0.5)
  age=round(rnorm(n, 60, 8))
  baseline_hazard=0.2
  lp=-0.3 * trt + 0.02 * (age - 60)
  u=runif(n)
  surv_time=-log(u) / (baseline_hazard * exp(lp))
  censor_time=runif(n, 0, max_follow)
  time=pmin(surv_time, censor_time)
  status=as.integer(surv_time <= censor_time)
  data.frame(id = id, time = time, status = status, trt = trt, age = age)
}

# -------------------------
# UI
# -------------------------
ui=fluidPage(
  titlePanel("Survival Demo — Simulated, Uploaded, and Smoking Dataset"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_choice", "Data source:",
                   choices = c("Simulated" = "sim",
                               "Smoking (geecure)" = "smoking",
                               "Upload CSV" = "upload"),
                   selected = "sim"),
      conditionalPanel(
        "input.data_choice == 'upload'",
        fileInput("file_upload",
                  "Upload CSV (must contain columns: time, status)",
                  accept = ".csv")
      ),
      hr(),
      checkboxInput("show_km", "Show Kaplan–Meier curve", TRUE),
      uiOutput("km_group_ui"),
      hr(),
      checkboxInput("fit_cox", "Fit Cox model", TRUE),
      conditionalPanel("input.fit_cox == true",
                       uiOutput("cox_cov_ui"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DT::dataTableOutput("data_table")),
        tabPanel("Kaplan–Meier", plotOutput("km_plot")),
        tabPanel("Cox PH", verbatimTextOutput("cox_summary"))
      )
    )
  )
)

# -------------------------
# Server
# -------------------------
server=function(input, output, session) {
  
  data_surv=reactive({
    
    # -------------------------
    # Uploaded CSV
    # -------------------------
    if (input$data_choice == "upload") {
      f=input$file_upload
      if (is.null(f)) return(NULL)
      df=tryCatch(read.csv(f$datapath, stringsAsFactors = FALSE),
                     error = function(e) NULL)
      if (is.null(df)) return(NULL)
      if (!all(c("time", "status") %in% names(df))) {
        showNotification("Uploaded CSV must have 'time' and 'status' columns", type = "error")
        return(NULL)
      }
      return(df)
    }
    
    # -------------------------
    # Smoking dataset
    # -------------------------
    if (input$data_choice == "smoking") {
      
      if (!requireNamespace("geecure", quietly = TRUE)) {
        showNotification("Package 'geecure' not installed. Install via install.packages('geecure')",
                         type = "error")
        return(NULL)
      }
      
      data("smoking", package = "geecure")
      df=geecure::smoking
      
      # Construct survival time
      df$time=df$Timept2 - df$Timept1
      df$status=df$Relapse
      
      return(df)
    }
    
    # -------------------------
    # Simulated
    # -------------------------
    simulate_surv_data(n = 250)
  })
  
  # Data preview
  output$data_table=DT::renderDataTable({
    df=data_surv()
    if (is.null(df)) return(datatable(data.frame(msg = "No data")))
    datatable(df, options = list(pageLength = 10))
  })
  
  # Categorical variable detection
  categorical_vars=reactive({
    df=data_surv()
    if (is.null(df)) return(character(0))
    vars=setdiff(names(df), c("time", "status"))
    eligible=vapply(vars, function(v) {
      col=df[[v]]
      if (is.factor(col) || is.character(col)) return(TRUE)
      if (is.numeric(col) || is.integer(col)) {
        nuniq=length(unique(col[!is.na(col)]))
        return(nuniq <= 8)
      }
      FALSE
    }, logical(1))
    vars[eligible]
  })
  
  output$km_group_ui=renderUI({
    cats=categorical_vars()
    if (length(cats) == 0) {
      helpText("No categorical variables available.")
    } else {
      selectInput("km_group", "Group KM by:",
                  choices = c("None" = "", cats), selected = "")
    }
  })
  
  output$cox_cov_ui=renderUI({
    df=data_surv()
    if (is.null(df)) return(NULL)
    covs=setdiff(names(df), c("time", "status"))
    if (length(covs) == 0) {
      helpText("No covariates available.")
    } else {
      checkboxGroupInput("cox_covs", "Covariates for Cox:",
                         choices = covs,
                         selected = covs[1])
    }
  })
  
  # KM Plot (robust base plotting)
  output$km_plot=renderPlot({
    req(input$show_km)
    df=data_surv()
    req(df)
    
    if (!all(c("time", "status") %in% names(df))) {
      plot.new(); text(0.5, 0.5, "Missing time/status"); return()
    }
    
    strat_var=NULL
    if (!is.null(input$km_group) && input$km_group != "") {
      grp=factor(df[[input$km_group]])
      if (nlevels(grp) >= 2) {
        df$grp_temp=grp
        strat_var="grp_temp"
      }
    }
    
    formula_use=if (is.null(strat_var)) {
      Surv(time, status) ~ 1
    } else {
      as.formula(paste0("Surv(time, status) ~ ", strat_var))
    }
    
    fit=tryCatch(survfit(formula_use, data = df),
                    error = function(e) NULL)
    
    if (is.null(fit)) {
      plot.new(); text(0.5, 0.5, "KM fit failed"); return()
    }
    
    plot(fit,
         col = if (!is.null(strat_var)) rainbow(length(fit$strata)) else 1,
         lty = 1,
         xlab = "Time",
         ylab = "Survival",
         main = "Kaplan–Meier Curve",
         mark.time = TRUE)
    
    if (!is.null(strat_var)) {
      legend("topright",
             legend = levels(df$grp_temp),
             col = rainbow(length(levels(df$grp_temp))),
             lty = 1,
             bty = "n")
    }
  })
  
  # Cox model
  output$cox_summary=renderPrint({
    req(input$fit_cox)
    df=data_surv()
    req(df)
    
    covs=input$cox_covs
    form_text=if (is.null(covs) || length(covs) == 0) {
      "Surv(time, status) ~ 1"
    } else {
      paste0("Surv(time, status) ~ ", paste(covs, collapse = " + "))
    }
    
    fit=tryCatch(coxph(as.formula(form_text), data = df),
                    error = function(e) e)
    
    if (inherits(fit, "error")) {
      cat("Cox failed:", fit$message)
      return()
    }
    
    print(summary(fit))
    cat("\n--- PH Test ---\n")
    print(cox.zph(fit))
  })
}

shinyApp(ui, server)
