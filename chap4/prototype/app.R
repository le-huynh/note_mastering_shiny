library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

if (!exists("injuries")) {
	injuries <- vroom::vroom(here("chap4/data/injuries.tsv.gz"))
	products <- vroom::vroom(here("chap4/data/products.tsv"))
	population <- vroom::vroom(here("chap4/data/population.tsv"))
}

# ui
prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
	fluidRow(
		column(8,
		       selectInput("code",
		       	         "Product",
		       	         choices = prod_codes,
		       	         width = "100%")),
		column(2, numericInput("rows", 
				   "Number of Rows",
				   min = 1, 
				   max = 10, 
				   value = 5)),
		column(2, 
		       selectInput("y", "Y axis", c("rate", "count")))
	),
	fluidRow(
		column(4, tableOutput("diag")),
		column(4, tableOutput("body_part")),
		column(4, tableOutput("location"))
	),
	fluidRow(
		column(12, plotOutput("age_sex"))
	),
	fluidRow(
		column(2, actionButton("story", "Tell me a story")),
		column(10, textOutput("narrative"))
	)
)
#>> count top
count_top <- function(df, var, n = 5) {
	df %>%
		mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
		group_by({{ var }}) %>%
		summarise(n = as.integer(sum(weight)))
}
#<<

# server
server <- function(input, output, session) {
	selected <- reactive(injuries %>% filter(prod_code == input$code))
	
	# Find the maximum possible of rows.
	max_no_rows <- reactive(
		max(length(unique(selected()$diag)),
		    length(unique(selected()$body_part)),
		    length(unique(selected()$location)))
	)
	
	# Update the maximum value for the numericInput based on max_no_rows().
	observeEvent(input$code, {
		updateNumericInput(session, "rows", max = max_no_rows())
	})
	
	table_rows <- reactive(input$rows - 1)
	
	output$diag <- renderTable(
		count_top(selected(), diag, n = table_rows()), width = "100%")
	
	output$body_part <- renderTable(
		count_top(selected(), body_part, n = table_rows()), width = "100%")
	
	output$location <- renderTable(
		count_top(selected(), location, n = table_rows()), width = "100%")	
	
	summary <- reactive({
		selected() %>%
			count(age, sex, wt = weight) %>%
			left_join(population, by = c("age", "sex")) %>%
			mutate(rate = n / population * 1e4)
	})
	
	#>> rate vs count
	output$age_sex <- renderPlot({
		if (input$y == "count") {
			summary() %>%
				ggplot(aes(age, n, colour = sex)) +
				geom_line() +
				labs(y = "Estimated number of injuries")
		} else {
			summary() %>%
				ggplot(aes(age, rate, colour = sex)) +
				geom_line(na.rm = TRUE) +
				labs(y = "Injuries per 10,000 people")
		}
	}, res = 96)
	
	#>> narrative
	narrative_sample <- eventReactive(
		list(input$story, selected()),
		selected() %>% pull(narrative) %>% sample(1)
	)
	output$narrative <- renderText(narrative_sample())
	}
#>>

shinyApp(ui, server)

