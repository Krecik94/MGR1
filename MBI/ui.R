library(shiny)

fluidPage(
  
  # App title ----
  titlePanel("MBI"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Name
      textInput(inputId = "name",
                   label = "Name:",
                   value = "chr1")

      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("coverage_plot")
    )
  )
)