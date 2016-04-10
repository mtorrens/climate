source('~/Desktop/dvis/src/00_start.R')
#city.w <- get(load(file = '~/Desktop/dvis/data/cities/201508181.RData'))
cool.cities <- get(load(file = '~/Desktop/dvis/data/cool_cities.RData'))

library(shiny)

zones <- sort(unique(cool.cities[, 'Zone']))
vars <- c('Temperature', 'Relative Humidity', 'Sea Level Pressure',
          'Wind Speed', 'Dew Point', 'Visibility')

################################################################################
ui <- fluidPage(
################################################################################
  tags$h1('City Weather Radials'),
  tags$h3('Data Visualization Project'),
  tags$h5('Shiny app built by Miquel Torrens © 2016'),
  tags$a(href= 'http://github.com/mtorrens/weather',
         'Source code available here'),
  tags$br(),
  tags$br(),

  tabsetPanel(
    tabPanel('Weather Radials', 
      sidebarLayout(
        sidebarPanel(
         helpText('Choose a place that you would like to discover from the set of available cities'),
         selectInput(inputId = 'country', label = 'Country',
                     choices = sort(unique(cool.cities[, 'FullCountry'])), 
                     selected = 'Catalonia'),
        
         uiOutput('city'),
        
         helpText('Select the weather variable that you are interested in and pick a particular year'),
         selectInput(inputId = 'variable', label = 'Variable',
                     choices = vars, selected = 'Temperature'),
        
         selectInput(inputId = 'year', label = 'Year',
                     choices = c('Average', as.character(2015:2000)),
                     selected = '2015'),
        
         helpText('Hit the button to find out the results!'),
         actionButton(inputId = 'click', label = 'Discover')
        ),
        mainPanel(
         tags$br(),
         plotOutput('main', width = '90%')
        ))
    ),
    tabPanel("Barcelona", 
      tags$br(),
      'text'
      #includeHTML(paste(DATDIR, "d3csv/index.html", sep = ''))
    ),
    tabPanel("New York City", "Text"),
    tabPanel("London", "Text"),
    tabPanel("Paris", "Text"),
    tabPanel("Tokyo", "Text"),
    tabPanel("Sydney", "Text"),
    tabPanel("Tromsø", "Text"),
    navbarMenu(title = "Other cities",
               tabPanel("Longyearbyen", "Text"),
               tabPanel("Cairo", "Text"),
               tabPanel("Bombay", "Text"),
               tabPanel("Shanghai", "Text"),
               tabPanel("Mexico City", "Text"),
               tabPanel("Los Angeles", "Text"),
               tabPanel("Honolulu", "Text"),
               tabPanel("São Paulo", "Text"),
               tabPanel("Ushuaia", "Text")
    )
  ),
  tags$br(),
  tags$p('Please contact the',
         tags$a(href= 'mailto:miquel.torrens@barcelonagse.eu', 'author'), '
         if you wish to make any requests on adding new cities and years.'),
  tags$br()
)

################################################################################
server <- function(input, output, session) {
################################################################################
  # Special slider
  output$city <- renderUI(
    selectInput('city', 'City',
      sort(unique(cool.cities[which(cool.cities[, 'FullCountry'] ==
                                      input$country), 'DisplayName'])),
      'Barcelona')
  )

  # Make the plot
  weather.plot <- eventReactive(input$click, {
    mine <- which(cool.cities[, 'DisplayName'] == input$city)[1]
    code <- cool.cities[mine, 'WMO']
    new.file <- paste(DATDIR, 'cities/', input$year, code, '.RData', sep = '')
    validate(
      need(file.exists(new.file), 'Oops! You got me. Info not available...')
    )
    city.w <- get(load(file = new.file))
    print(head(city.w))
    pl <- climate.plot(city.w, input$city, input$year, input$variable)
    return(pl)
  })
  
  # Result
  if (is.null(eventReactive(input$click, { weather.plot() }))) {
    output$main <- NULL
  } else {
    output$main <- renderPlot(height = 530, units = "px", { weather.plot() })
  }
}

# Run the app
shinyApp(ui = ui, server = server)
