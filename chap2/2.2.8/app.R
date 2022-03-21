library(shiny)

ui <- fluidPage(
	textInput("name", label = NULL, placeholder = "Your name"),
	textOutput("greeting"),
	sliderInput("date", "When should we deliver?",
		  value = as.Date("2020-09-17"),
		  min = as.Date("2020-09-16"),
		  max = as.Date("2020-09-23"))
	
)
server <- function(input, output, session) {
	
	output$greeting <- renderText({
		paste0("Hello ", input$name)
	})
	
}
shinyApp(ui, server)
