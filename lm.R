library(shiny)
library(DT)
library(tidyverse)
library(broom)
f <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/zombies.csv"
d <- read_csv(f, col_names = TRUE)
d <- select(d, height, weight, age, gender, major)
d$gender <- factor(d$gender)
d$major <- factor(d$major)
r <- c("height", "weight", "age")
p <- names(d)
# Define the UI ----
ui <- fluidPage(
  titlePanel(h1("Simple LM Visualizer")),
  sidebarLayout(
    sidebarPanel(width = 5,
      selectInput(
        "response",
        label = "Choose a response variable...",
        choices = c("", r)
      ),
      br(),
      selectInput(
        "predictors",
        label = "Choose one or more predictor variables...",
        choices = p,
        multiple = TRUE
      ),
      br(),
      textOutput("model"),
      br(),
      tableOutput("modelresults"),
      style="text-align:center"
    ),
    mainPanel(width = 7,
      dataTableOutput("datatable"),
      plotOutput("plot")
    )
  )
)

# Define server logic ----
server <- function(input, output) {

  output$datatable <-
    renderDataTable(d, options = list(
      paging = TRUE,
      lengthMenu = list(c(5, 10, 25, -1), c('5', '10', '25', 'All')),
      pageLength = 5
    ))
  
  m <- reactive({
    mod <- NULL
    if (input$response == "" |
        length(input$predictors) == 0) {
      return(mod)
    }
    mod <- paste0(input$response, " ~ ", input$predictors[1])
    if (length(input$predictors) > 1) {
      for (i in 2:length(input$predictors)) {
        mod <- paste0(mod, " + ", input$predictors[i])
      }
    }
    return(mod)
  })
  
  output$model <- renderText({paste0("Model: ",print(m()))})
  
  output$modelresults <- renderTable({
    if (!is.null(m())) {
      res <- lm(data = d, formula = m())
      tidy(res) |> select(term, estimate, p.value)
    }
  }, width = "100%", rownames = TRUE, striped = TRUE, spacing = "s", bordered =
    TRUE, align = "c", digits = 3)

  output$plot <- renderPlot({
    if (!is.null(m()) & length(input$predictors) == 1) {
      y <- input$response
      x <- input$predictors
      if (class(d[[x]]) != "factor") {
        p <- ggplot(data = d, aes(x = .data[[x]], y = .data[[y]])) +
          geom_point() +
          geom_smooth(method = lm)
      } else {
        p <- ggplot(data = d, aes(x = .data[[x]], y = .data[[y]])) +
          geom_violin() +
          geom_jitter(width = 0.2, alpha = 0.5)
      }
      p <- p + xlab(x) + ylab(y) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      p
    } else if (!is.null(m()) & length(input$predictors) == 2) {
      y <- input$response
      x <- input$predictors
      if (class(d[[x[1]]]) == "factor" & class(d[[x[2]]]) == "factor") {
        p <- ggplot(data = d, aes(x = .data[[x[1]]], y = .data[[y]])) +
          geom_violin() +
          geom_jitter(width = 0.2, alpha = 0.5) +
          facet_wrap(~ d[[x[2]]])
        p <- p + xlab(x[1]) + ylab(y)
      } else if (class(d[[x[1]]]) != "factor" & class(d[[x[2]]]) == "factor"){
        p <- ggplot(data = d, aes(x = .data[[x[1]]], y = .data[[y]])) +
          geom_point() +
          geom_smooth(method = lm) +
          facet_wrap(~ d[[x[2]]])
        p <- p + xlab(x[1]) + ylab(y)
      } else if (class(d[[x[1]]]) == "factor" & class(d[[x[2]]]) != "factor"){
        p <- ggplot(data = d, aes(x = .data[[x[2]]], y = .data[[y]])) +
          geom_point() +
          geom_smooth(method = lm) +
          facet_wrap(~ d[[x[1]]])
        p <- p + xlab(x[2]) + ylab(y)
      } else {
        p <- NULL
      }
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      p
    }
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
