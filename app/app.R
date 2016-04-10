################################################################################
# Script: app.R
source('~/Desktop/bgse/projects/dvis/src/00_start.R')
################################################################################

# Shiny
library(shiny)

# General variables
zones <- sort(unique(cool.cities[, 'Zone']))
vars <- c('Temperature', 'Precipitation', 'Relative Humidity',
          'Sea Level Pressure', 'Wind Speed', 'Dew Point', 'Visibility',
          'Cloud coverage')

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
                     choices = c('Mean (2000-2015)', 'Median (2000-2015)',
                                 as.character(2015:2000)),
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
      'Text'
      #includeHTML(paste(DATDIR, "d3csv/index.html", sep = ''))
    ),
    tabPanel("New York City", "Text"),
    tabPanel("London", "Text"),
    tabPanel("Paris", "Text"),
    tabPanel("Tokyo", "Text"),
    tabPanel("Sydney", "Text"),
    tabPanel("Tromsø", "Text"),
    navbarMenu(title = "Other cities",
               tabPanel("Bombay", "Text"),
               tabPanel("Cairo", "Text"),
               tabPanel("Cape Town", "Text"),
               tabPanel("Dubai", "Text"),
               tabPanel("Honolulu", "Text"),
               tabPanel("Longyearbyen", "Text"),
               tabPanel("Los Angeles", "Text"),
               tabPanel("Mexico City", "Text"),
               tabPanel("São Paulo", "Text"),
               tabPanel("Shanghai", "Text"),
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
    if (input$year == 'Mean (2000-2015)') {
      var.year <- 'avg'
    } else if (input$year == 'Median (2000-2015)') {
      var.year <- 'med'
    } else {
      var.year <- input$year
    }
    new.file <- paste(DATDIR, 'cities/', var.year, code, '.RData', sep = '')
    validate(
      need(file.exists(new.file), 'Oops! Information not available...')
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
# END OF SCRIPT
