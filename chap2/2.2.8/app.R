library(shiny)

ui <- fluidPage(
	textInput("name", label = NULL, placeholder = "Your name"),
	textOutput("greeting"),
	sliderInput("date", "When should we deliver?",
		  value = as.Date("2020-09-17"),
		  min = as.Date("2020-09-16"),
		  max = as.Date("2020-09-23")),
	sliderInput("range", "",
		  value = 25,
		  min = 0,
		  max = 100,
		  step = 5,
		  animate = TRUE),
	selectInput("breed",
		  "Select your favorite animal breed:",
		  choices = list(`dogs` = list('German Shepherd', 
				           'Bulldog', 
				           'Labrador Retriever'),
			       `cats` = list('Persian cat', 
			     	           'Bengal cat', 
			     	           'Siamese Cat'))
	)
)
server <- function(input, output, session) {
	
	output$greeting <- renderText({
		paste0("Hello ", input$name)
	})
	
}
shinyApp(ui, server)
