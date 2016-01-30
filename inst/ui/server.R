
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(senegal)
map.path <- system.file(package = "senegal","maps","dakar_map.rds")
dakar.map <- readRDS(map.path)


shinyServer(function(input, output) {
  vals <- reactiveValues(country=NULL)
  ratios <- reactive(get_ratios(db = db))
  output$dakarmap <- renderLeaflet({
    pop_map(dakar.map)
  })




  showCountryPopup <- function(country, lat, lng) {
    content = popup_content(country ,dakar.map[dakar.map$pop==country,]$Total)
    leafletProxy("dakarmap") %>% addPopups(lng, lat, content, layerId = country)
  }

  observe({
    if(!is.null(vals$country))
      output[["investChart"]] <-
        renderChart(pop_chart(dakar.map,vals$country,id="investChart"))
  })


  ## When map is clicked
  ## Show a popup with city info

  observe({
    leafletProxy("dakarmap") %>% clearPopups()
    event <- input$dakarmap_shape_click
    if (is.null(event))
      return()

    isolate({
      vals$country <- event$id
      showCountryPopup(event$id, event$lat, event$lng)
    })
   })

})
