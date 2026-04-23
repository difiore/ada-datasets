library(shiny)

# Define the UI ----
ui <- fluidPage(
  titlePanel(h1("Monster Picker")),
  sidebarLayout(
    sidebarPanel(
      selectInput("favorite_monster",
                  label = "Choose one of the following...",
                  choices = c("Zombie", "Vampire", "Alien", "Werewolf"),
                  selected = "Zombie"
      )
    ),
    mainPanel(
      h3("Wanna see a picture of your favorite monster?"),
      h4("This is really cool!"),
      br(),
      textOutput("favorite_monster"),
      br(),
      uiOutput("monster_image")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$favorite_monster <- renderText({
    paste0("You have picked ", input$favorite_monster)
  })

  output$monster_image <- renderUI({
    img_src <- case_when(
      input$favorite_monster == "Zombie" ~ "zombie.png",
      input$favorite_monster == "Vampire" ~ "vampire.png",
      input$favorite_monster == "Alien" ~ "alien.png",
      input$favorite_monster == "Werewolf" ~ "werewolf.png"
    )
    img(src = img_src, height = 300)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
