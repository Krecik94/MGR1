#Shiny main 
library(shiny)

runApp <- fuction () {

	ui <- fluidPage()
	
	server <- function(input, output) {}

	shinyApp(ui=ui, server=server)
}

runApp()