

#' @import jsonlite
#' @export
issalat <-
  function(){
    op <- Sys.setlocale("LC_TIME","french")
    on.exit(Sys.setlocale("LC_TIME",op))

    path <- system.file(package = "senegal","2015-pray.json")
    dd <- fromJSON(path)

    dd[[format(Sys.Date(),"%B")]][format(Sys.Date(),"%d"),]

  }
