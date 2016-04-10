source('~/Desktop/dvis/src/start.R')
source(paste(SRCDIR, 'climate_plot.R', sep = ''))
city.w <- get(load(file = '~/Desktop/dvis/data/cities/201508181.RData'))

library(shiny)

ui <- fluidPage(
  tags$h1('City Weather Radials'),
  tags$h3('Data Visualization Project'),
  tags$h5('Shiny app built by Miquel Torrens (c) 2016'),
  tags$a(href= 'http://github.com/mtorrens/weather', 'Code available here'),
  tags$br(),
  tags$br(),

  tabsetPanel(
    tabPanel("Weather Radials", 
      sidebarLayout(
       sidebarPanel(
         selectInput(inputId = 'zone', label = 'World zone:',
                     choices = c('Europe', 'America', 'Africa'), selected = 'Europe'),
         
         selectInput(inputId = 'city', label = 'City:',
                     choices = c('Barcelona'), selected = 'Barcelona'),
         
         selectInput(inputId = 'variable', label = 'Variable:',
                     choices = c('Temperature'), selected = 'Temperature'),

         selectInput(inputId = 'year', label = 'Year:',
                     choices = as.character(2015:2000), selected = '2015'),

                  actionButton(inputId = "click", label = "Submit")
       ),
       mainPanel(
         tags$br(),
         plotOutput('main', width = '100%'))
      )
    ),
    tabPanel("Barcelona 2015", "Add D3 Barcelona 2015"),
    tabPanel("Oslo 2015", "Add D3 Oslo 2015"),
    navbarMenu(title = "Other",
               tabPanel("NYC", "contents"),
               tabPanel("LA", "contents"),
               tabPanel("Sydney", "contents")
    )
  )#,
  
  #tags$br(),
  #tags$p('Any requests to add cities and years, please contact the',
  #       tags$a(href= 'mailto:miquel.torrens@barcelonagse.eu', 'author'), '.')
  
  
  
  
)

server <- function(input, output) {
  #plotOutput(outputId = 'main')
  
  weather.plot <- eventReactive(input$click, {
    climate.plot(city.w, input$city, input$year)
  })
  
  output$main <- renderPlot(height = 600, units = "px", { weather.plot() })
}

shinyApp(ui = ui, server = server)
