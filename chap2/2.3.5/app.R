library(shiny)

ui <- fluidPage(
	plotOutput("plot", 
		 height = "300px",
		 width = "700px"),
	dataTableOutput("table")
)

server <- function(input, output, session){
	output$plot <- renderPlot(plot(1:5, main = "A scatterplot of five random numbers"),
			      res = 96)
	output$table <- renderDataTable(mtcars,
				  options = list(pageLength = 5,
				  	       ordering = FALSE,
				  	       searching = FALSE))
}

shinyApp(ui, server)
