
#' @import XLConnect
#' @export
create_map <-
  function(){
    map.path <- system.file(package = "senegal","maps","senegal4.rds")
    map.data <- readRDS(map.path)
    dakar.map <- map.data[map.data$NAME_1=="Dakar",]

    ## compute area in km
    areas <- sapply(dakar.map@polygons,
                    function(com)
                      sum(sapply(com@Polygons,
                                 function(x)areaPolygon(x@coords))))

    dakar.map$area <- areas


    pop.path <- system.file(package = "senegal","pop","Population_Commune_RGPHAE_2013.xlsx")
    wb <- loadWorkbook(pop.path)
    pop.data <- readWorksheet(wb, sheet = 1)

    function(x)
      switch (x,
              "ngor" = iconv("n’gor","UTF-8","UTF-8"),
              x
      )

    pop.coms <- tolower(pop.data$Commune)
    pop.coms[grep('n.gor',pop.coms)] <- "ngor"
    pop.coms <- gsub('m.bao','mbao',pop.coms)
    pop.coms[pop.coms=="rufisque est"] <- "rufisque  est"
    pop.coms[pop.coms=="colobanefassgueule tapee"] <- "gueule tapeecolobanefass"


    dakar.coms <- tolower(dakar.map$NAME_4)
    dakar.coms <- gsub('m.bao','mbao',dakar.coms)

    pop.coms <- gsub('[/]','',pop.coms)
    dakar.coms <- gsub('[/]','',dakar.coms)
    mapping <- sapply(dakar.coms[!dakar.coms %in% pop.coms],
                      function(x){
                        vals <- agrep(x,pop.coms,value=TRUE)
                        if (length(vals)==0)"NOT FOUND"
                        else if(length(vals)==1)vals
                      })


    dat1 <- data.frame(map=names(mapping),pop=mapping)
    dat2 <- data.frame(map=dakar.coms[dakar.coms %in% pop.coms],pop=dakar.coms[dakar.coms %in% pop.coms])
    dat <- rbind(dat1,dat2)
    pop.data$Commune <- pop.coms
    pop.dat.all <- merge(dat,pop.data,by.x="pop",by.y="Commune")
    dakar.map$NAME_4 <- tolower(dakar.map$NAME_4)
    dd <- merge(pop.dat.all,dakar.map,by.x="pop",by.y="NAME_4",all.y=TRUE)

    slot(dakar.map,"data") <- dd
    saveRDS(dakar.map,"inst/maps/dakar_map.rds")
  }

format_nbr <-
  function(nbr)
    format(nbr, decimal.mark=",",big.mark=" ", small.interval=3)


#' Create popup content
#'
#' @param region character
#' @param nbr numeric popluation
#'
#' @return character popup content
#' @export

popup_content <-
  function(region,nbr){
    paste0(
      "<b>",region,"</b><br/>",
      "<b>",format_nbr(nbr)," Personnes </b>"
    )
  }
create_pop <- function(dat){
  popup_content(dat$pop,dat$Total)

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

  qpal <- colorBin("Reds", c(0,max(dakar.map$area,na.rm=TRUE)),bins =10)
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

