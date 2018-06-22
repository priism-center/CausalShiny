library(shiny)
library(bartCause)

########################################
ui <- fluidPage(
  
  # Navigating Tabs
  navbarPage("Causal Inference",
             
             # Tab Panel 1
             tabPanel("Panel1 - Introduction", 
                      
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
             tabPanel("Panel2 - Upload", 
                      
                      # Sidebar Layout
                      sidebarLayout(
                        
                        # Sidebar Panel
                        sidebarPanel(
                          
                          # Input: file
                          fileInput("file", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          hr(),
                          
                          # Input: header
                          checkboxInput("header", "Header", TRUE),
                          
                          # Input: ID Column
                          checkboxInput("id", "Subject ID column?", FALSE), 
                          
                          conditionalPanel(
                            condition = "input.id",
                            numericInput("idcol", "Subject ID Column Number", value = 1)
                          ),
                          
                          hr(),
                          
                          # Input: Y Column and Z column
                          numericInput("zcol", "Treatment (Y) Column Number", value = 2),
                          numericInput("ycol", "Response (Z) Column Number", value = 3)
                        ),
                        
                        # Main Panel
                        mainPanel(
                          
                          # Output: Data file ----
                          tableOutput("uploads")
                        )
                      )
             ),
             
             # Tab Panel 3
             tabPanel("Panel3 - Options", 
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar Panel
                        sidebarPanel(
                          
                          # Input: Select Estimand
                          radioButtons("estimand", "Select Estimand",
                                       choices = c(ATE = "ATE",
                                                   ATT = "ATT",
                                                   ATC = "ATC"),
                                       selected = "ATE"),
                          
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
                                                   "pweight" = "pweight",
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
                          radioButtons("cscut", "Select Common Support Cut",
                                       choices = c("NA real" = "NA_real_",
                                                   "1" = 1,
                                                   "0.05" = 0.05),
                                       selected = "NA_real_")
                          
                        ),
                        
                        
                        # Main Panel
                        mainPanel(
                          
                          # Output: plots
                          plotOutput("postplot")
                        )
                      )
             )
             
  )
  )




#########################################
server <- function(input, output) {
  
  output$uploads <- renderTable({
    
    req(input$file)
    
    tryCatch(
      {
        df <- read.csv(input$file$datapath,
                       header = input$header)
        
        ### add check for variables are categorical
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    return(head(df))
    
  })
  
  output$postplot <- renderPlot({
    
    tryCatch(
      {
        fit1 <- bartc(response = df[, input$ycol], treatment = df[, input$zcol], 
                      confounders = df[, c(-input$ycol, -input$zcol)], data = df, 
                      method.rsp = input$rspmethod, method.trt = input$trtmethod, 
                      estimand = input$estimand, commonSup.rule = input$csrule, 
                      commonSup.cut = input$cscut,
                      propensityScoreAsCovariate = input$pscoreas)
        plot_sigma(fit1)
        plot_support(fit1)
        plot_est(fit1)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)