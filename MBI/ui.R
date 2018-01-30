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
                   value = "chr1"),
      # Range start
      numericInput(inputId = "rangeStart",
                  label = "Range start:",
                  value = 10000),
      # Range end
      numericInput(inputId = "rangeEnd",
                   label = "Range end:",
                   value = 15000),
      # Button
      actionButton(inputId = "button",
                   label = "Calculate")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Sequence from 1st BAM file
      textOutput(outputId = "sequence1"),
      # Sequence from 2nd BAM file
      textOutput(outputId = "sequence2"),
      
      plotOutput("coverage_plot")
    )
  )
)