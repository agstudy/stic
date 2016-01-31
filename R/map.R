
#' Create dakar map with population
#'
#' @import data.table
#' @export
create_map <-
  function(){
    map.path <- system.file(package = "senegal","maps","senegal4.rds")
    dakar.map <- readRDS(map.path)
    dakar.map <- dakar.map[dakar.map$NAME_1=="Dakar",]
    map.data <- setkey(setDT(dakar.map@data)[,pop:=tolower(NAME_4)],pop)


    pop.path <- system.file(package = "senegal","pop","pop.csv")
    pop.data <- fread(pop.path)[,pop:=tolower(Commune)]

    ## special cities with no default mapping
    pop.data[grep("rufisque est",pop),pop:="rufisque  est"]
    pop.data[grep("tapee",pop),pop:=map.data[grep("tapee",pop),pop]]
    pop.data[grep("bao",pop),pop:=map.data[grep("bao",pop),pop]]
    pop.data[grep("gor$",pop),pop:=map.data[grep("gor$",pop),pop]]
    pop.data[grep("b.*lor$",pop),pop:=map.data[grep("b.*lor$",pop),pop]]
    pop.data[grep("aire",pop),pop:=map.data[grep("aire",pop),pop]]
    pop.data[grep("th.*mer",pop),pop:=map.data[grep("th.*mer",pop),pop]]
    pop.data[grep("oie",pop),pop:=map.data[grep("oie",pop),pop]]
    pop.data[grep("lima",pop),pop:=map.data[grep("lima",pop),pop]]
    setkey(pop.data,pop)
    ## merge
    slot(dakar.map,"data") <- setkey(pop.data[map.data],OBJECTID)
    saveRDS(dakar.map,"inst/maps/dakar_map.rds")
  }

format_nbr <-
  function(nbr)
    format(nbr, decimal.mark=",",big.mark=" ", small.interval=3)


#' Create popup content
#'
#' @param commune character
#' @param depart character
#' @param arrond character
#' @param nbr numeric popluation
#'
#' @return character popup content
#' @export

popup_content <-
  function(commune,nbr,depart,arrond){
    paste0(
      "<b>Departement:</b>",depart,"<br/>",
      "<b>Arrondissment:</b>",arrond,"<br/>",
      "<b>Commune:</b>",commune,"<br/>",
      "<b>Popoulation:</b>",format_nbr(nbr)," Personnes"
    )
  }
create_pop <- function(dat){
  popup_content(dat$pop,dat$Total,dat$NAME_2,dat$NAME_3)

}


#' Create population map
#'
#' @return leaflet map
#' @import leaflet
#' @import geosphere
#' @export
#'
pop_map <- function(dakar.map){

  if(missing(dakar.map)){
    map.path <- system.file(package = "senegal","maps","dakar_map.rds")
    dakar.map <- readRDS(map.path)

  }

  qpal <- colorBin("Reds", c(0,max(dakar.map$Total,na.rm=TRUE)),bins =10)
  url_tile <-
    "http://server.arcgisonline.com/ArcGIS/rest/services/World_Terrain_Base/MapServer/tile/{z}/{y}/{x}"
  map <- leaflet(dakar.map) %>%
    addTiles(
      urlTemplate=url_tile,
      options = tileOptions(noWrap = FALSE)) %>%
    setView(lat=14.75985,lng= -17.30106,zoom=12)



  map %>% addPolygons(
    layerId=~pop,
    stroke = TRUE,
    fillOpacity = 0.7,
    dashArray= '3',
    color="white",
    fillColor = ~qpal(Total)) %>%
    addLegend(pal = qpal,
              values = ~Total,
              opacity = 1,
              position="topleft",
              title="Population")
}

