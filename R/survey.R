library(googlesheets)

.e <- new.env()
format_conso <- function(var)
{
  if(grepl("Consommation..D",var,fixed=TRUE)) "cDésodorisant"
  else  switch (var,
                "Consommation..Javel." = "cJavel",
                "Consommation..Lave.Linge.Liquide."="cLaveLinge",
                "Consommation..Poudre.Machine."="cPoudreMachine",
                "Consommation..Autres.Air.Fresh.Lave.vitre.."="cAutres",
                "Consommation..Liquide.Vaisselle."="cVaisselle",
                "Consommation..Poudre.multi.usage."="cPoudreMultiUsage",
                var)

}

#' Extract survey raw data
#'
#' Survey data are stored in google spredsheetin Drive.
#'
#' @return data.frame
#' @export
#' @import googlesheets
#' @import data.table
#' @examples
#' load_survey
load_survey <-
  function(force=FALSE){

    if(is.null(.e$data) || force).raw_data()
    yy <- setDT(.e$data)[,Timestamp:=as.POSIXct(Timestamp,"%d/%m/%Y %H:%M:%S",tz="")]
    ss <- suppressWarnings(melt(yy,id=c("Timestamp","Foyer"))[!is.na(value)])

    setkey(ss,Timestamp)
    ss[,variable := gsub(".*[.]([0-9]+.*)[.]","\\1",variable)]
    ss[,Foyer:=factor(Foyer,levels=c("1","2-3","3-5",">5"))]


    ss[,variable := sapply(variable , format_conso)]

    ss[!grep("Poudre",value),Type:="Liquide"]
    ss[grep("Poudre",value),Type:="Poudre"]
    ss
  }

#' Load Imports and Exports
#'
#' @importFrom  httr GET
#' @importFrom  XML readHTMLTable
#' @export
impo_expo <-
  function(){

    r <- GET("http://atlas.cid.harvard.edu/country/sen")
    ll <- readHTMLTable(content(r,"text"),stringsAsFactors=FALSE)
    imp.sen <- ll[[4]]
    imp.sen <- transform(imp.sen,Value= as.numeric(gsub(",|[$]","",Value)))

    expo.sen <- ll[[2]]
    expo.sen <- transform(expo.sen,Value= as.numeric(gsub(",|[$]","",Value)))
    list(impo=imp.sen,expo=expo.sen)
  }



.raw_data <-
  function(){
    url <- "https://docs.google.com/spreadsheets/d/1FdjLSdg1fkVBI5IT-rfVb-w2ngZGQJXUdZUhb81SlY4/edit#gid=1686733536&vpid=A2"
    suppressMessages({
      xx <- gs_url(url)
      yy <- gs_read(xx)
    })
    .e$data <- yy
  }

#' @export
get_nbr <- function(force=FALSE){
  if(is.null(.e$data) || force).raw_data()
  nrow(.e$data)
}

