library(shiny)

fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      numericInput(inputId = "rangeStart",
                  label = "Range start:",
                  value = 10000),
      numericInput(inputId = "rangeEnd",
                   label = "Range end:",
                   value = 15000),
      actionButton(inputId = "button",
                   label = "Button")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      #plotOutput(outputId = "distPlot"),
      textOutput(outputId = "sequence1"),
      textOutput(outputId = "sequence2")
      #plotOutput(outputId = "distPlot2")
      
    )
  )
)