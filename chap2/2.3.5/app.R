library(shiny)

ui <- fluidPage(
	plotOutput("plot", 
		 height = "300px",
		 width = "700px")
)

server <- function(input, output, session){
	output$plot <- renderPlot(plot(1:5, main = "A scatterplot of five random numbers"),
			      res = 96)
	
}

shinyApp(ui, server)