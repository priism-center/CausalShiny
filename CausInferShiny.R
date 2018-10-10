require(shiny)
require(bartCause)
require(treatSens)
require(foreign)
require(readstata13)
require(openxlsx)
require(ggplot2)
require(shinyBS)
require(png)
require(shinythemes)

########################################
csplotaxis <- c("Tree"= "tree", "PCA"= "pca", "Common Support Statistics"= "css", 
                  "Propensity Score"= "p.score", "Outcome"= "y", "y0", "y1", 
                  "Individual Treatment Effect Estimates" = "indiv.diff", 
                  "Propensity Score Weights" = "p.weights"
                  #, "Any Predictor Column" = "other"
                  )

text1 <- "testing popup window for information"

#source(functions.r)

######

ui <- fluidPage(theme = shinytheme("cerulean"), 
  
  # Navigating Tabs
  navbarPage("Causal Inference",
             
             # Tab Panel 1
             tabPanel("Introduction", 
                      
                  # Sidebar Layout
                  sidebarLayout(
                        
                      # Sidebar Panel
                      sidebarPanel(
                          
                          # Text
                          h3("Introduction"),
                          p("This RShiny application aims to simplify the process of conducting Causal Inference.")
                      ),
                        
                        # Main Panel
                        mainPanel(
                          
                          # Text
                          h1("Instructions"),
                          p("This application allows the users to explore different options through
                             the interface and get live results."),
                          hr(),
                          h4("Step 1. Upload data"),
                          p("Upload data and identify confounders (X), treatment (Z), 
                            response (Y), and ID column (optional)"),
                          
                          h4("Step 2. Estimand selection and advanced options"),
                          h5("A. Specify", actionLink("link1", "treatment effect"), "type"),
                          h5("B. Model propensity score?"),
                          p("(1). Method for treatment assignment? (bart, bart.xval, glm)"),
                          p("(2). How to include propensity score? (as weight or as covariate)"),
                          p("(3). Use TMLE adjustment?"),
                          h4("Step 3. Display results"),
                          p("Show summary of model fit"),
                          ######
                          
                          #Pop over vs. modal
                          bsPopover("link1", "Treatment Effect", content = text1, "hover"),
                          #bsModal("Modal1", "Treatment Effect", "link1", size = "large", 
                                  #text1),
                          
                          ######
                          h4("Step 4. Check for common support"),
                          p("Common support plot"),
                          h4("Step 5. Convergence diagnostics"),
                          p("Trace plots"),
                          hr(),
                          h4("R Package Used"),
                          p(a("BARTCause", href = "https://github.com/vdorie/bartCause")),
                          
                          hr(),
                          h1("Example"),
                          imageOutput("img1"),
                          hr(),
                          p("Specify X, Y, Z variables for the data uploaded, you can check 
                            if the dataframe is in working condition from the Status bar"),
                          imageOutput("img2")
                        )
                   )
              ),
             
             # Tab Panel 2
             tabPanel("Upload Data", 
                      
                      # Sidebar Layout
                      sidebarLayout(
                        
                        # Sidebar Panel
                        sidebarPanel(
                          
                          # Input: file type
                          selectInput("filetype", "Select File Type", 
                                      choices = c("csv" = "csv", 
                                                  "dta" = "dta",
                                                  "xlsx" = "xlsx",
                                                  "txt" = "txt",
                                                  "spss" = "spss")),
                          
                          # Input: file
                          fileInput("file", "Choose File",
                                    multiple = FALSE,
                                    accept = NULL
                                    #  c("text/csv",
                                    #           "text/comma-separated-values,text/plain",
                                    #           ".csv")
                                    ),
                          
                          hr(),
                          
                          # Input: header
                          checkboxInput("header", "Header", TRUE),
                          
                          hr(),
                          
                          # Input: ID Column
                          #checkboxInput("id", "Subject ID column?", FALSE), 
                          
                          #conditionalPanel(
                          #  condition = "input.id",
                          #  selectInput("idcol", "Subject ID Column", choices = NULL)
                          #),
                          
                          #hr(),
                          
                          # Column Selection for X and Y
                          selectInput("xcol", "Select Covariates (X) Columns", 
                                      choices = NULL, multiple = TRUE),
                          selectInput("ycol", "Select Response (Y) Column", choices = NULL),
                          
                          # Column Selection for Z, and identify treatment
                          selectInput("zcol", "Select Treatment (Z) Column", choices = NULL),
                          
                          #conditionalPanel(
                          #  condition = "input.zcol",
                            selectInput("trt.ind", "Select the Value Representing Receiving Treatment", 
                                        choices = NULL),
                          #)
                          
                          # Grouping Variable
                          checkboxInput("gvarcheck", "Include Grouping Variable?", FALSE), 
                          
                          conditionalPanel(
                            condition = "input.gvarcheck",
                            selectInput("gvar", "Select Grouping Variable", choices = NULL)
                          )
                          
                      ),
                        
                        # Main Panel
                        mainPanel(
                          
                          # Output: Data file
                          h4("Status"),
                          textOutput("uploadconfirm"),
                          #textOutput("variableconfirm"),
                          hr(),
                          h4("Data"),
                          tableOutput("uploads")
                      )
                  )
             ),
             
             # Tab Panel 3
             tabPanel("Select Estimand & Advanced Options", 
                      
                  # Sidebar layout
                  sidebarLayout(
                        
                      # Sidebar Panel
                      sidebarPanel(
                          
                          # Input: Select Estimand
                          radioButtons("estimand", "Select Estimand",
                                       choices = c("ATE" = "ate",
                                                   "ATT" = "att",
                                                   "ATC" = "atc"),
                                       selected = "ate")
                          ,
                          #####
                          checkboxInput("pscheck", "Model Propensity Score?", TRUE),
                          hr(),
                          
                          conditionalPanel(
                            condition = "input.pscheck",
                            radioButtons("trtmethod", "How to include P-Score?",
                                         choices = c("glm" = "glm",
                                                     "bart" = "bart",
                                                     "bart.xval" = "bart.xval"),
                                         selected = "glm")
                          ),
                          hr(),
                          
                          conditionalPanel(
                            condition = "input.pscheck",
                            radioButtons("pscoreas", "Propensity Score as?",
                                         choices = c("Covariate" = "cov",
                                                     "Weight" = "wgt"),
                                         selected = "cov")
                          ),
                          hr(),
                          
                          conditionalPanel(
                            condition = "input.pscheck",
                            checkboxInput("tmleadjust", "TMLE Adjustment?", FALSE)
                          )
                          #####
                      ),
                      
                      # Main Panel
                      mainPanel(
                          
                        # Output: plots
                        h4("Filtered Table"),
                        tableOutput("filteredtable")
                      )
                  )
              ),
             
             
             # Tab Panel 4
             tabPanel("Display Results", 
                      
                  # Sidebar layout
                  sidebarLayout(
                        
                      # Sidebar Panel
                      sidebarPanel(
                          
                          ############ new structure
                          # Input: Model Propensity Score
                          actionButton("runButton", "Run Model")
                          
                          ############
                          
                          
                          # Input: Survey Weights
                          #checkboxInput("sweight", "Survey Weights", FALSE),
                          
                        #  
                        #    #Input: Add common support cut
                        #    conditionalPanel(
                        #      condition = "input.csrule != 'none'",
                        #      radioButtons("cscut", "Select Common Support Cut",
                        #                   choices = c("NA real" = NA_real_,
                        #                               "1" = 1,
                        #                               "0.05" = 0.05),
                        #                   selected = NA_real_)),
                      ),
                        # Main Panel
                        mainPanel(
                          
                          # Output: plots
                          #h4("Filtered Table"),
                          #tableOutput("filteredtable2"),
                          ######
                          
                          verbatimTextOutput("summary")
                          ######
                      )
                  )
             ),
             
             # Tab Panel 5
             tabPanel("Common Support",
                      
                  # Sidebar layout
                  sidebarLayout(
                        
                      # Sidebar Panel
                      sidebarPanel(
                        
                        #Input: Add common support rule
                        radioButtons("csrule", "Select Common Support Rule",
                                    choices = c("none" = "none",
                                                "sd" = "sd",
                                                "chisq" = "chisq"),
                                     selected = "sd"),
                        
                        ### Use prediction?
                        ###checkboxInput("supportpredict", "Use potential outcomes for 
                        ###              common support plot?", TRUE)

                          
                        hr(),
                        
                        # Input: plot common support
                        conditionalPanel(
                          condition = "input.csrule != 'none'",
                          selectInput("xvar", "X Variable", 
                                      choices = csplotaxis)),
                        
                        conditionalPanel(
                          condition = "input.csrule != 'none'",
                          selectInput("yvar", "Y Variable", 
                                      choices = csplotaxis))
                            
                      ),
                      
                      # Main Panel
                      mainPanel(
                        h4("Common Support Plot"),
                        plotOutput("supplot"),
                        ##
                        downloadButton("supexp", "Download Plot")
                        ##
                      )
                  )
             ),
             
             # Tab Panel 6
             tabPanel("Trace Plots",
                      
                      # Sidebar layout
                      sidebarLayout(
                        
                        # Sidebar Panel
                        sidebarPanel(
                          
                          # Input: plots to show (plot_sigma, plot_est)
                          h4("Trace Plots")
                        ),
                        
                        # Main Panel
                        mainPanel(
                          h4("Trace Plots"),
                          h5("Sigma Plot"),
                          plotOutput("sigmaplot"),
                          h5("Estimation Plot"),
                          plotOutput("estplot")
                        )
                      )
             )
             
  )
  )



###################################################
server <- function(input, output, session) {
  
  # Image outputs
  output$img1 <- renderImage({
    return(list(
      src = "images/upload_file.png",
      filetype = "image/png",
      height = 365,
      width = 480
    ))
  }, deleteFile = F)
  
  output$img2 <- renderImage({
    return(list(
      src = "images/select_variables.png",
      filetype = "image/png",
      height = 365,
      width = 480
    ))
  }, deleteFile = F)
  
  # Create global data object
  my_data <- reactive({
    req(input$file, input$filetype)
    if (input$filetype == "csv") {
      df <- read.csv(input$file$datapath, header = input$header)
    }
    
    else if (input$filetype == "dta") {
      df <- read.dta13(input$file$datapath)
    }
    
    else if (input$filetype == "xlsx") {
      df <- read.xlsx(input$file$datapath, colNames = input$header)
    }
    
    else if (input$filetype == "txt") {
      df <- read.table(input$file$datapath, header = input$header)
    }
    
    else if (input$filetype == "spss") {
      df <- read.spss(input$file$datapath, to.data.frame = T)
    }
    
    df
  })
  
  # Upload Data tab outputs
  output$uploads <- renderTable({
    req(input$file)
    return(head(my_data(), 20))
  })
  
  output$uploadconfirm <- renderText({
    
    ##########
    datacheck <- function(data, confound, trt, resp) {
      if (is.null(data)) {
        "Please Upload a dataset"
      }
      
      else {
        
        if (is.null(confound) || trt == "" || resp == "") {
          "Please identify X, Y, Z"
        }
        
        else {
          
          if (length(unique(my_data()[, which(names(my_data()) == trt)])) > 2) {
            "Please check treatment variable selection, and/or missing values"
          }
          
          else {
            NULL
          }
        }
      }
    }
    
    validate(
      datacheck(input$file, input$xcol, input$zcol, input$ycol)
    )
    ##########
    paste("Upload complete")
  })
  
  # Variable Confirmation
  #output$variableconfirm <- renderText({
  #  req(filtered)
    #####
  #  idcheck <- function(matr) {
  #    idlist <- c()
  #    
  #    for (i in 1:ncol(matr)) {
  #      if (length(unique(matr[, i])) == nrow(matr)) {
  #        idlist <- c(idlist, names(matr)[i])
  #      }
  #    }
  #    
  #    invisible(idlist)
  #  }
  #  #####
  #  validate(
  #    idcheck(filtered())
  #  )
  #  paste("Variables are checked")
  #})
    
  # Updating column selection
  observe({
    req(input$file)
    vars <- names(my_data())
    updateSelectInput(session, "idcol", choices = vars)
    updateSelectInput(session, "xcol", choices = vars)
    updateSelectInput(session, "zcol", choices = vars)
    updateSelectInput(session, "ycol", choices = vars)
    updateSelectInput(session, "gvar", choices = vars)
    updateNumericInput(session, "xvar", max = ncol(my_data()))
  })
  
  ##########
  
  # Intermediate Filtered table object with original coding
  filtered_pre <- reactive({
    req(input$file, input$ycol, input$zcol, input$xcol)
    df_clean <- subset(my_data(), select = c(input$ycol, input$zcol, input$xcol))
    df_clean
  })
  
  observe({
    req(filtered_pre())
    trtvalues <- unique(filtered_pre()[, 2])
    updateSelectInput(session, "trt.ind", choices = trtvalues)
  })
  
  # Filtered table object
  filtered <- reactive({
    req(input$trt.ind)
    data0 <- filtered_pre()
    data0[which(data0[ ,2] == input$trt.ind), 2] <- 1L
    data0[which(data0[ ,2] != input$trt.ind), 2] <- 0L
    data0
  })
  
  # Filtered table display
  output$filteredtable <- renderTable({
    req(filtered())
    return(head(filtered(), 20))
  })

  # Translating fit input and recode variables
  rspmethod <- reactive({
    req(filtered())

    if (!input$pscheck) {
      "bart"
    }
    else if (input$tmleadjust) {
      "tmle"
    }
    else if (input$pscoreas == "wgt") {
      "p.weight"
    }
    else "bart"
  })
  
  # Running bartc function to store fit
#  fit <- reactive({
#    req(filtered())
#    
#    fit0 <- bartc(response = filtered()[, 1], treatment = filtered()[, 2], 
#                  confounders = as.matrix(filtered()[, c(-1, -2)]), 
#                  estimand = input$estimand, method.rsp = rspmethod(),
#                  method.trt = input$trtmethod, commonSup.rule = input$csrule,
#                  group.by = ifelse(input$gvarcheck, gvar, FALSE)
##                  ,commonSup.cut = input$cscut
##                  ,p.scoreAsCovariate = (input$pscoreas == "cov")
#                  )
#    fit0
#  })
  fit <- eventReactive(input$runButton, {
    req(filtered())
    
    progress <- Progress$new(session, min=1, max=10)
    on.exit(progress$close())
    
    progress$set(message = 'Model fitting in progress')
    
    for (i in 1:10) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    fit0 <- bartc(response = filtered()[, 1], treatment = filtered()[, 2],     
                  confounders = as.matrix(filtered()[, c(-1, -2)]), 
                  estimand = input$estimand, method.rsp = rspmethod(),
                  method.trt = input$trtmethod, commonSup.rule = input$csrule,
                  group.by = ifelse(input$gvarcheck, gvar, FALSE)
#                  ,commonSup.cut = input$cscut
#                  ,p.scoreAsCovariate = (input$pscoreas == "cov")
                  )
    fit0    
  })
  
  
  # Summary output
  output$summary <- renderPrint({
#    input$runButton
#    progress <- Progress$new(session, min=1, max=10)
#    on.exit(progress$close())
#    
#    progress$set(message = 'Model fitting in progress')
#    
#    for (i in 1:10) {
#      progress$set(value = i)
#      Sys.sleep(0.5)
#    }
    summary(fit())
  })
  
  
  ##########
  
  # plot_sigma
  output$sigmaplot <- renderPlot({
    req(fit())
    plot_sigma(fit())
  })
  
  # plot_est
  output$estplot <- renderPlot({
    req(fit())
    plot_est(fit())
  })
  
  
  #####
  # Define x and y axis
  csxvar <- reactive({
    req(filtered())
    
    if (input$xvar == "tree") {
      "tree.1"
    }
    else if (input$xvar == "pca") {
      "pca.1"
    }
    else if (input$xvar == "other") {
      "y"
    }
    else input$xvar
  })
  
  csyvar <- reactive({
    req(filtered())
    
    if (input$yvar == "tree") {
      "tree.2"
    }
    else if (input$yvar == "pca") {
      "pca.2"
    }
    else if (input$yvar == "other") {
      "y"
    }
    else input$yvar
  })
  
  # plot_support
  supplot1 <- reactive({
    req(fit())
    #if (input$csrule == "none") {
    #  NULL
    #  print("Common Support rule not specified, unable to plot")
    #}
    #else 
    plot_support(fit(), xvar = csxvar(), yvar = csyvar())
  })
  
  
  # plot_support output
  output$supplot <- renderPlot({
    supplot1()
  })
  
  output$supexp <- downloadHandler(
    filename = function(){
      "sup.png"
    },
    content = function(file){
      ggsave(file, plot = plot_support(fit(), xvar = csxvar(), yvar = csyvar()), device = "png")
    }
  )
  
  #####
  
}
 
# Run the application 
shinyApp(ui = ui, server = server)

#testdata <- read.csv("/Users/George/Desktop/A3SR/Others/Causal_Inference_Shiny/experiment/simpletest.csv", header = T)
#fit1 <- bartc(testdata$Outcome, testdata$Treatment, testdata[, 1:2], estimand = "ate")
#cpsdat <- read.csv("/Users/George/Desktop/A3SR/Others/Causal_Inference_Shiny/experiment/cps.csv", header = T)
#fit2 <- bartc(cpsdat$re78, cpsdat$treat, cpsdat$age + cpsdat$educ, estimand = "ate", method.trt = "glm", p.scoreAsCovariate = T, method.rsp = "bart", commonSup.rule = "sd")
