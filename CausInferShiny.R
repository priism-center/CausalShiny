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
                          p("Trim dataset so it only includes confounders (X), treatment (Y), 
                            response (Z), and ID column (optional)."),
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
             tabPanel("Upload", 
                      
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
                          
                          # Column Selection for X, Y, and Z
                          selectInput("xcol", "Select Covariates (X) Columns", 
                                      choices = NULL, multiple = TRUE),
                          selectInput("ycol", "Select Response (Y) Column", choices = NULL),
                          selectInput("zcol", "Select Treatment (Z) Column", choices = NULL)
                          
                      ),
                        
                        # Main Panel
                        mainPanel(
                          
                          # Output: Data file
                          tableOutput("uploads")
                      )
                  )
             ),
             
             # Tab Panel 3
             tabPanel("Fitting Options", 
                      
                  # Sidebar layout
                  sidebarLayout(
                        
                      # Sidebar Panel
                      sidebarPanel(
                          
                          # Input: Select Estimand
                          radioButtons("estimand", "Select Estimand",
                                       choices = c("ATE" = "ate",
                                                   "ATT" = "att",
                                                   "ATC" = "atc"),
                                       selected = "ate"),
                          
                          hr(),
                          
                          # Input: Survey Weights
                          checkboxInput("sweight", "Survey Weights", FALSE),
                          
                          # Input: Add Propensity Score
                          checkboxInput("pscore", "Add Propensity Score", FALSE),
                          
                          conditionalPanel(
                            condition = "input.pscore",
                            radioButtons("pscoreas", "Propensity Score",
                                         choices = c("As Covariate" = FALSE,
                                                     "As Weight" = TRUE),
                                         selected = FALSE)),
                          
                          hr(),
                          
                          #Input: Add method for fitting treatment assignment mechanism
                          radioButtons("trtmethod", "Select Method for Treatment Assignment Mechanism",
                                       choices = c("none" = "none",
                                                   "glm" = "glm",
                                                   "bart" = "bart",
                                                   "bart.xval" = "bart.xval"),
                                       selected = "none"),
                          
                          hr(),
                          
                          #Input: Add method for fitting response surface
                          radioButtons("rspmethod", "Select Method for Response Surface",
                                       choices = c("bart" = "bart",
                                                   "pweight" = "p.weight",
                                                   "tmle" = "tmle"),
                                       selected = "bart"),
                          
                          hr(),
                          
                          #Input: Add common support rule
                          radioButtons("csrule", "Select Common Support Rule",
                                       choices = c("none" = "none",
                                                   "sd" = "sd",
                                                   "chisq" = "chisq"),
                                       selected = "none"),
                          
                          hr(),
                          
                            #Input: Add common support cut
                            conditionalPanel(
                              condition = "input.csrule != 'none'",
                              radioButtons("cscut", "Select Common Support Cut",
                                           choices = c("NA real" = NA_real_,
                                                       "1" = 1,
                                                       "0.05" = 0.05),
                                           selected = NA_real_)),
                          
                          hr(), 
                          
                          # Input: plots to show (plot_sigma, plot_est)
                          checkboxInput("plotsigma", "Traceplot Sigma", FALSE),
                          
                          checkboxInput("plotest", "Traceplot", FALSE),  
                          
                          # Input: plot common support
                          checkboxInput("plotsup", "Plot Common Support", FALSE), 
                          conditionalPanel(
                            condition = "input.plotsup",
                            numericInput("xvar", "X Variable", 1, 
                                         min = 1, max = 10)),
                          
                          
                          # Action Button for plotting
                          actionButton("showplot", "Plot")
                          
                      ),
                        
                        
                        # Main Panel
                        mainPanel(
                          
                          # Output: plots
                          h4("Filtered Table"),
                          tableOutput("filteredtable"),
                          h4("Trace Plots"),
                          plotOutput("sigmaplot"),
                          plotOutput("estplot")
                          ######
                          ,
                          verbatimTextOutput("summary")
                          ######
                      )
                  )
             ),
             
             # Tab Panel 4
             tabPanel("Plots",
                      
                  # Sidebar layout
                  sidebarLayout(
                        
                      # Sidebar Panel
                      sidebarPanel(
                        
                        # Input: plots to show (plot_sigma, plot_est)
                        checkboxInput("plotsigma", "Traceplot Sigma", FALSE),
                        
                        checkboxInput("plotest", "Traceplot", FALSE),  
                        
                        # Input: plot common support
                        checkboxInput("plotsup", "Plot Common Support", FALSE), 
                        conditionalPanel(
                          condition = "input.plotsup",
                          numericInput("xvar", "X Variable", 1, 
                                       min = 1, max = 10))
                      ),
                      
                      # Main Panel
                      mainPanel(
                        h4("Plots"),
                        plotOutput("indivplot")
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
  
  output$uploads <- renderTable({
    req(input$file)
    return(head(my_data()))
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
  
  # Filtered table display
  output$filteredtable <- renderTable({
    req(filtered())
    return(head(filtered()))
  })
  
  # Running bartc function to store fit
  fit <- reactive({
    req(filtered)
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
    input$showplot  
    req(fit())
    plot_sigma(fit())
  })
  
  # plot_est
  output$estplot <- renderPlot({
    input$showplot  
    req(fit())
    plot_est(fit())
  })
  
  # Individual plots
  output$indplots <- renderPlot({
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)