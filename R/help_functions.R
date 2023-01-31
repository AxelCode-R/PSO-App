mrunif <- function(nr, nc, lower, upper) {
  return(matrix(runif(nr*nc,0,1),nrow=nr,ncol=nc)*(upper-lower)+lower)
}


html_save <- function(html_chart, zoom = 4, vheight = 300, vwidth = 600, delay = 2, force = F, expand = NULL, ...){
    if(!dir.exists("img/")){
      dir.create("img/")
    }
    suppressMessages({
      htmlwidgets::saveWidget(html_chart, "img/p.html")
      webshot2::webshot("img/p.html", "img/p.png", delay=delay, zoom=zoom, vheight=vheight, vwidth = vwidth, expand = expand, ...)
    })
}

assl <- function(...){
  attribut_list <- list(...)
  for(x in names(attribut_list)[names(attribut_list) != ""]){
    assign(x,attribut_list[[x]], envir = .GlobalEnv)
  }
}
