library(shiny)
library(bartCause)
library(treatSens)

########################################
ui <- fluidPage(
  
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
                          h1("Instructions", align = "center"),
                          p("Bypassing coding, this application allows the users to simply select different
                            options through the interface to get live results."),
                          br(),
                          h4("Step 1."),
                          p("Trim dataset so it only includes confounders (X), treatment (Z), 
                            response (Y), and ID column (optional)."),
                          h4("Step 2."),
                          p("Convert and save dataset into .csv file."),
                          h4("Step 3."),
                          p("Click Upload panel on top of the page and choose the file path to upload dataset. 
                            Specify the column number of ID column (if applicable), treatment column (Y), 
                            and response column (Z)."),
                          h4("Step 4."),
                          p("Use Option panel to select method of analysis."),
                          h3("R Packages Used"),
                          p(a("BARTCause", href = "https://github.com/vdorie/bartCause"), " and ",
                            a("treatSens", href = "https://cran.r-project.org/web/packages/treatSens/index.html"))
                          
                          
                        )
                   )
              ),
             
             # Tab Panel 2
             tabPanel("Upload Data", 
                      
                      # Sidebar Layout
                      sidebarLayout(
                        
                        # Sidebar Panel
                        sidebarPanel(
                          
                          # Input: file
                          fileInput("file", "Choose File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          hr(),
                          
                          # Input: header
                          checkboxInput("header", "Header", TRUE),
                          
                          hr(),
                          
                          # Input: ID Column
                          checkboxInput("id", "Subject ID column?", FALSE), 
                          
                          conditionalPanel(
                            condition = "input.id",
                            selectInput("idcol", "Subject ID Column", choices = NULL)
                          ),
                          
                          hr(),
                          
                          # Column Selection for X and Y
                          selectInput("xcol", "Select Covariates (X) Columns", 
                                      choices = NULL, multiple = TRUE),
                          selectInput("ycol", "Select Response (Y) Column", choices = NULL),
                          
                          # Column Selection for Z, and identify treatment
                          selectInput("zcol", "Select Treatment (Z) Column", choices = NULL),
                          
                          conditionalPanel(
                            condition = "input.zcol",
                            selectInput("trt.ind", "Select the Value Representing Receiving Treatment", 
                                        choices = NULL)
                          )
                          
                      ),
                        
                        # Main Panel
                        mainPanel(
                          
                          # Output: Data file
                          h4("Status"),
                          textOutput("uploadconfirm"),
                          hr(),
                          h4("Data"),
                          tableOutput("uploads")
                      )
                  )
             ),
             
             # Tab Panel 3
             tabPanel("Select Estimand", 
                      
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
             tabPanel("Advanced Options", 
                      
                  # Sidebar layout
                  sidebarLayout(
                        
                      # Sidebar Panel
                      sidebarPanel(
                          
                          ############ new structure
                          # Input: Model Propensity Score
                          checkboxInput("pscheck", "Model Propensity Score?", TRUE),
                          hr(),
                          
                          conditionalPanel(
                            condition = "input.pscheck",
                            radioButtons("trtmethod", "How to include P-Score?",
                                         choices = c("none" = "none",
                                                     "glm" = "glm",
                                                     "bart" = "bart",
                                                     "bart.xval" = "bart.xval"),
                                         selected = "none")
                          ),
                          hr(),
                          
                          conditionalPanel(
                            condition = "input.pscheck",
                            radioButtons("pscorebool", "Propensity Score as?",
                                         choices = c("Covariate" = FALSE,
                                                     "Weight" = TRUE),
                                         selected = FALSE)
                          ),
                          hr(),
                          
                          conditionalPanel(
                            condition = "input.pscheck",
                            checkboxInput("tmleadjust", "TMLE Adjustment?", FALSE)
                          )
                          
                            #Input: Add method for fitting response surface
                            ,radioButtons("rspmethod", "Select Method for Response Surface",
                                         choices = c("bart" = "bart",
                                                     "pweight" = "p.weight",
                                                     "tmle" = "tmle"),
                                         selected = "bart")
                          
                          ),
                          
                          ############
                          
                          
                          # Input: Survey Weights
                          #checkboxInput("sweight", "Survey Weights", FALSE),
                          
                          # Input: Add Propensity Score
                          #checkboxInput("pscore", "Add Propensity Score", FALSE),
                          
                          #conditionalPanel(
                          #  condition = "input.pscore",
                          #  radioButtons("pscoreas", "Propensity Score",
                          #               choices = c("As Covariate" = FALSE,
                          #                           "As Weight" = TRUE),
                          #               selected = FALSE)),
                          #
                          #hr(),
                        #  
                        #  #Input: Add method for fitting treatment assignment mechanism
                        #  radioButtons("trtmethod", "Select Method for Treatment Assignment Mechanism",
                        #               choices = c("none" = "none",
                        #                           "glm" = "glm",
                        #                           "bart" = "bart",
                        #                           "bart.xval" = "bart.xval"),
                        #               selected = "none"),
                        #  
                        #  hr(),
                        #  
                        #  #Input: Add method for fitting response surface
                        #  radioButtons("rspmethod", "Select Method for Response Surface",
                        #               choices = c("bart" = "bart",
                        #                           "pweight" = "p.weight",
                        #                           "tmle" = "tmle"),
                        #               selected = "bart"),
                        #  
                        #  hr(),
                        #  
                        #  #Input: Add common support rule
                        #  radioButtons("csrule", "Select Common Support Rule",
                        #               choices = c("none" = "none",
                        #                           "sd" = "sd",
                        #                           "chisq" = "chisq"),
                        #               selected = "none"),
                        #  
                        #  hr(),
                        #  
                        #    #Input: Add common support cut
                        #    conditionalPanel(
                        #      condition = "input.csrule != 'none'",
                        #      radioButtons("cscut", "Select Common Support Cut",
                        #                   choices = c("NA real" = NA_real_,
                        #                               "1" = 1,
                        #                               "0.05" = 0.05),
                        #                   selected = NA_real_)),
                        #  
                        #  hr(), 
                        #  
                        #  # Input: plots to show (plot_sigma, plot_est)
                        #  checkboxInput("plotsigma", "Traceplot Sigma", FALSE),
                        #  
                        #  checkboxInput("plotest", "Traceplot", FALSE),  
                        #  
                        #  # Input: plot common support
                        #  checkboxInput("plotsup", "Plot Common Support", FALSE), 
                        #  conditionalPanel(
                        #    condition = "input.plotsup",
                        #    numericInput("xvar", "X Variable", 1, 
                        #                 min = 1, max = 10)),
                        #  
                        #  
                        #  # Action Button for plotting
                        #  actionButton("showplot", "Plot")
                        #  
                      #),
                        
                        
                        # Main Panel
                        mainPanel(
                          
                          # Output: plots
                          h4("Filtered Table"),
                          tableOutput("filteredtable2"),
                          #h4("Trace Plots"),
                          #plotOutput("sigmaplot"),
                          #plotOutput("estplot"),
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
                          
                        hr(),
                        
                        # Input: plot common support
                        conditionalPanel(
                          condition = "input.csrule != 'none'",
                          numericInput("xvar", "X Variable", 1, 
                                       min = 1, max = 10))
                      ),
                      
                      # Main Panel
                      mainPanel(
                        h4("Plots"),
                        plotOutput("csplot")
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
                          checkboxInput("plotsigma", "Traceplot Sigma", FALSE),
                          
                          checkboxInput("plotest", "Traceplot", FALSE)  
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
  



#########################################
server <- function(input, output, session) {
  
  # Create global data object
  my_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = input$header)
    df
  })
  
  # Upload Data tab outputs
  output$uploads <- renderTable({
    req(input$file)
    return(head(my_data()))
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
    paste("Upload complete, proceed to next tab")
  })

    
  # Updating column selection
  observe({
    req(input$file)
    vars <- names(my_data())
    updateSelectInput(session, "idcol", choices = vars)
    updateSelectInput(session, "xcol", choices = vars)
    updateSelectInput(session, "zcol", choices = vars)
    updateSelectInput(session, "ycol", choices = vars)
    updateNumericInput(session, "xvar", max = ncol(my_data()))
  })
  
  ##########
  
  # Filtered table object
  filtered <- reactive({
    req(input$file, input$ycol, input$zcol, input$xcol)
    df_clean <- subset(my_data(), select = c(input$ycol, input$zcol, input$xcol))
    df_clean
  })
  
  observe({
    req(filtered())
    trtvalues <- unique(filtered()[, 2])
    updateSelectInput(session, "trt.ind", choices = trtvalues)
  })
  
  # Filtered table display
  output$filteredtable <- renderTable({
    req(filtered())
    return(head(filtered()))
  })
  
  output$filteredtable2 <- renderTable({
    req(filtered())
    return(head(filtered()))
  })
  
  # Running bartc function to store fit
  fit <- reactive({
    req(filtered)
    
    # Translating fit input and recode variables
    
    
    
    
    fit0 <- bartc(response = filtered()[, 1], treatment = filtered()[, 2], 
                  confounders = as.matrix(filtered()[, c(-1, -2)]), 
                  estimand = input$estimand, method.rsp = input$rspmethod,
                  method.trt = input$trtmethod, commonSup.rule = input$csrule
#                  ,commonSup.cut = input$cscut
#                  ,p.scoreAsCovariate = input$pscoreas
                  )
    fit0
  })
  
  # Summary output
  output$summary <- renderPrint({
    req(fit())
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
  
  # plot_support
  output$supplot <- renderPlot({
    req(fit())
    if (input.csrule == "none") {
      NULL
      print("Common Support rule not specified, unable to plot")
    }
    else plot_est(fit())
  })
  
  
  # Individual plots
  output$indplots <- renderPlot({
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#testdata <- read.csv("/Users/George/Desktop/A3SR/Others/Causal_Inference_Shiny/experiment/simpletest.csv", header = T)
#fit1 <- bartc(testdata$Outcome, testdata$Treatment, testdata[, 1:2], estimand = "ate")
