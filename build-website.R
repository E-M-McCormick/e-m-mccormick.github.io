Rmds <- list.files(path = "./static/pubs/", 
                   pattern = ".Rmd", 
                   recursive = TRUE,
                   full.names = TRUE)

htmls <- list.files(path = "./static/pubs/", 
                    pattern = ".html", 
                    recursive = TRUE,
                    full.names = TRUE)

lapply(1:length(Rmds), function(n){
  if (file.info(Rmds[n])$ctime > file.info(htmls[n])$ctime){
    rmarkdown::render(Rmds[n])
  }
})

rmarkdown::render_site(encoding = 'UTF-8')
