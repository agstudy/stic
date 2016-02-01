
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

    vals <- dakar.map@data[dakar.map$pop==country,c("Total","NAME_2","NAME_3")]
    content = popup_content(country ,vals$Total,vals$NAME_2,vals$NAME_3)
    leafletProxy("dakarmap") %>% addPopups(lng, lat, content, layerId = country)
  }

  observe({
    if(!is.null(vals$country))
      output[["investChart"]] <-
        renderChart2(pop_chart(dakar.map,vals$country,id="investChart"))
  })


  ## When map is clicked
  ## Show a popup with city info

  observe({
    leafletProxy("dakarmap") %>% clearPopups()
    event <- input$dakarmap_shape_mouseover
    if (is.null(event))
      return()

    isolate({
      vals$country <- event$id
      showCountryPopup(event$id, event$lat, event$lng)
    })
   })

})
