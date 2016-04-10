################################################################################
# Script: app.R
#source('~/Desktop/bgse/projects/dvis/src/00_start.R')
source('src/start.R')
################################################################################

# Shiny
library(shiny)

# General variables
zones <- sort(unique(cool.cities[, 'Zone']))
vars <- c('Temperature', 'Precipitation', 'Relative Humidity',
          'Sea Level Pressure', 'Wind Speed', 'Dew Point', 'Visibility',
          'Cloud coverage')

# Size of the radials
width <- 830
height <- 760

# Text
txt <- 'The weather radial shows the oscillation in temperatures of a given place across an entire year. Each bar is a day, and it is delimited by its minimum and maximum temperatures. The color of the bar shows the average temperature with respect to the the rest of the year. The blue balls illustrate (when available) if there has been significant precipitation during that day, and their size indicates the amount of precipitation. This plot has been rendered using D3.'

################################################################################
ui <- fluidPage(
################################################################################
  tags$h1('City Weather Radials'),
  tags$h3('Data Visualization Project'),
  tags$h5('Shiny app built by Miquel Torrens © 2016'),
  tags$a(href = 'http://github.com/mtorrens/weather',
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
        )),
      tags$br(),
      tags$p('Please contact the',
             tags$a(href = 'mailto:miquel.torrens@barcelonagse.eu', 'author'), '
             if you wish to make any requests on adding new cities and years.'),
      tags$br()
      
    ),
    tabPanel("Barcelona", 
             tags$br(),
             txt,
             tags$br(),
             tags$br(),
             imageOutput("bcn")
    ),
    tabPanel("New York City",
             tags$br(),
             txt,
             tags$br(),
             tags$br(),
             imageOutput("nyc")
    ),
    tabPanel("London",
             tags$br(),
             txt,
             tags$br(),
             tags$br(),
             imageOutput("london")
    ),
    tabPanel("Paris",
             tags$br(),
             txt,
             tags$br(),
             tags$br(),
             imageOutput("paris")
    ),
    tabPanel("Tokyo",
             tags$br(),
             txt,
             tags$br(),
             tags$br(),
             imageOutput("tokyo")
    ),
    tabPanel("Sydney",
             tags$br(),
             txt,
             tags$br(),
             tags$br(),
             imageOutput("sydney")
    ),
    tabPanel("Tromsø",
             tags$br(),
             txt,
             tags$br(),
             tags$br(),
             imageOutput("tromso")
    ),
    navbarMenu(title = "Other cities",
               tabPanel("Bombay",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("bombay")
               ),
               tabPanel("Cairo",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("cairo")
               ),
               tabPanel("Cape Town",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("capetown")
               ),
               tabPanel("Dubai",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("dubai")
               ),
               tabPanel("Honolulu",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("honolulu")
               ),
               tabPanel("Longyearbyen",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("longyearbyen")
               ),
               tabPanel("Los Angeles",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("la")
               ),
               tabPanel("Mexico City",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("mexico")
               ),
               tabPanel("San Francisco",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("sf")
               ),
               tabPanel("São Paulo",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("saopaulo")
               ),
               tabPanel("Shanghai",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("shanghai")
               ),
               tabPanel("Ushuaia",
                        tags$br(),
                        txt,
                        tags$br(),
                        tags$br(),
                        imageOutput("ushuaia")
               )
    )
  )
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
    new.file <- paste('cities/', var.year, code, '.RData', sep = '')
    validate(
      need(file.exists(new.file), 'Oops! Information not available...')
    )
    city.w <- get(load(file = new.file))
    pl <- climate.plot(city.w, input$city, input$year, input$variable)
    return(pl)
  })
  
  # Result
  if (is.null(eventReactive(input$click, { weather.plot() }))) {
    output$main <- NULL
  } else {
    output$main <- renderPlot(height = 530, units = "px", {
      weather.plot()
    })
  }
  
  output$bcn <- renderImage({
    list(src = 'img/barcelona.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)

  output$bombay <- renderImage({
    list(src = 'img/bombay.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$cairo <- renderImage({
    list(src = 'img/cairo.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$capetown <- renderImage({
    list(src = 'img/capetown.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$dubai <- renderImage({
    list(src = 'img/dubai.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$honolulu <- renderImage({
    list(src = 'img/honolulu.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$la <- renderImage({
    list(src = 'img/la.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$london <- renderImage({
    list(src = 'img/london.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$longyearbyen <- renderImage({
    list(src = 'img/longyearbyen.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$mexico <- renderImage({
    list(src = 'img/mexico.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$nyc <- renderImage({
    list(src = 'img/nyc.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$paris <- renderImage({
    list(src = 'img/paris.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)

  output$saopaulo <- renderImage({
    list(src = 'img/saopaulo.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$sf <- renderImage({
    list(src = 'img/sf.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$shanghai <- renderImage({
    list(src = 'img/shanghai.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$singapore <- renderImage({
    list(src = 'img/singapore.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$sydney <- renderImage({
    list(src = 'img/sydney.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$tokyo <- renderImage({
    list(src = 'img/tokyo.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$tromso <- renderImage({
    list(src = 'img/tromso.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  output$ushuaia <- renderImage({
    list(src = 'img/ushuaia.png',
         contentType = 'image/png',
         width = width,
         height = height)
  }, deleteFile = FALSE)  
}

# Run the app
shinyApp(ui = ui, server = server)
# END OF SCRIPT
