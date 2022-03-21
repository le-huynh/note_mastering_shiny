library(shiny)

ui <- fluidPage(
	textInput("name", label = NULL, placeholder = "Your name"),
	textOutput("greeting")
)
server <- function(input, output, session) {
	output$greeting <- renderText({
		paste0("Hello ", input$name)
	})
}
shinyApp(ui, server)
