source('~/Desktop/dvis/src/start.R')
source(paste(SRCDIR, 'climate_plot.R', sep = ''))
#city.w <- get(load(file = '~/Desktop/dvis/data/cities/201508181.RData'))
cool.cities <- get(load(file = '~/Desktop/dvis/data/cool_cities.R'))

library(shiny)

zones <- sort(unique(cool.cities[, 'Zone']))
vars <- c('Temperature', 'Dew Point', 'Relative Humidity', 'Sea Level Pressure',
          'Visibility', 'Wind Speed')

################################################################################
ui <- fluidPage(
################################################################################
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
         helpText('Choose a place that you would like to discover:'),
         # selectInput(inputId = 'zone', label = 'World zone',
         #             choices = zones, selected = 'Europe'),
         
         selectInput(inputId = 'country', label = 'Country',
                     choices = sort(unique(cool.cities[, 'FullCountry'])), 
                     #choices = c('--Select--', sort(unique(cool.cities[which(cool.cities[, 'Zone'] == 'Europe'), 'FullCountry']))), 
                     selected = 'Catalonia'),
         #uiOutput('country_select'),
         
         uiOutput('city'),

         helpText('Select the weather condition you are interested in and a particular year from this century:'),
         selectInput(inputId = 'variable', label = 'Variable',
                     choices = vars, selected = 'Temperature'),

         selectInput(inputId = 'year', label = 'Year',
                     choices = c('Average', as.character(2015:2000)),
                     selected = '2015'),

         helpText('Hit the button to see the results:'),
         actionButton(inputId = "click", label = "Discover")
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

################################################################################
server <- function(input, output, session) {
################################################################################
  # Special slider
  output$city <- renderUI(
    selectInput('city', 'City',
      sort(unique(cool.cities[which(cool.cities[, 'FullCountry'] == input$country),
                                'DisplayName'])), 'Barcelona')
  )

  # Make the plot
  weather.plot <- eventReactive(input$click, {
    mine <- which(cool.cities[, 'DisplayName'] == input$city)[1]
    code <- cool.cities[mine, 'WMO']
    new.file <- paste(DATDIR, 'cities/', input$year, code, '.RData', sep = '')
    if (! file.exists(new.file)) {
      return(NULL)
    } else {
      city.w <- get(load(file = new.file))
      print(head(city.w))
      pl <- climate.plot(city.w, input$city, input$year, input$variable)
      return(pl)
    }
  })
  
  # Result
  if (is.null(eventReactive(input$click, { weather.plot() }))) {
    output$main <- tags$h3('Oops! Information not available...')
  } else {
    output$main <- renderPlot(height = 600, units = "px", { weather.plot() })
  }
}

# Run the app
shinyApp(ui = ui, server = server)
