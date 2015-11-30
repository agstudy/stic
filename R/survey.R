library(googlesheets)


format_conso <- function(var)
{
  switch (var,
          "Consommation..Javel." = "cJavel",
          "Consommation..Lave.Linge.Liquide."="cLaveLinge",
          "Consommation..Poudre.Machine."="cPoudreMachine",
          "Consommation..Autres.Air.Fresh.Lave.vitre.."="cAutres",
          "Consommation..Liquide.Vaisselle."="cVaisselle",
          "Consommation..Poudre.multi.usage."="cPoudreMultiUsage",
          "Consommation..Désodorisant.Liquide."="Désodorisant",
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
  function(){
    url <- "https://docs.google.com/spreadsheets/d/1FdjLSdg1fkVBI5IT-rfVb-w2ngZGQJXUdZUhb81SlY4/edit#gid=1686733536&vpid=A2"
    suppressMessages({
    xx <- gs_url(url)
    yy <- gs_read(xx)
    })

    ## yy <- setNames(yy,c("Timestamp","Product","cJavel","cVaisselle","cLinge","cPoudre","cOthers","nbrFamily"))
    setDT(yy)[,Timestamp:=as.POSIXct(Timestamp,"%d/%m/%Y %H:%M:%S",tz="")]
    ss <- suppressWarnings(melt(yy,id=1)[!is.na(value)])

    setkey(ss,Timestamp)
    ss[,variable := gsub(".*[.]([0-9]+).*","\\1",variable)]



    ss[,variable := lapply(variable , format_conso)]

    ss[]
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
