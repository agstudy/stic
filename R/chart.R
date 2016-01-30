#' Plot pie chart of population
#'
#' @param dat data set
#' @param county character commune name
#'
#' @import rCharts
#' @return ndv3 plot discretBarChart
#' @export
pop_chart <- function (dat,county,id_dom="investChart") {

  xx <- slot(dat,"data")[dat$pop ==county,][,
                                           c("pop","Masculin","Feminin")
                                           ]
  xx <- melt(xx,id="pop")
  a <- hPlot(value~variable,
             data =xx,
             type = "pie",
             title = "Population",
             subtitle = county)


  #  a$params$width <- 400
  a$set(dom = id_dom)
  a
}
