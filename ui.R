
ui <- pageWithSidebar(
  
  #  Application title
  headerPanel("Minimization Randomization table for clinical trials"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
    textInput("title", "Set your study title:", "My trial name"),
    fileInput("file_rand", "Upload the file", multiple = FALSE),
    numericInput("seed", label = "Set your secret passcode(optional for unblinded):", value = NA),
    #     textOutput("text1"),
    #     textOutput("text2"),
    #     textOutput("text3"),
    #     textOutput("version"),
    selectInput("pt_id", "Select the column of patient unique id",
                choices = NULL),
    selectInput("covariance", "Select Variable to control randomization", choices = NULL, multiple = T),
    textInput("trt", "Name your treatment, separate by comma',' :", NA),
    selectInput("Treatment.col", "Select the column of treatment, 
                all the values in the column should be a subset from your input above ",
                choices = NULL),
    helpText("Random table for minimization randomization. 
             Written in R/Shiny by Tingchang Wang."),
    downloadButton('downloadData', 'Download random table')
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tableOutput("randTable")
  )
)### end ui
