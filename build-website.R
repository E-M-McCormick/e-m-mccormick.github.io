Rmds <- list.files(path = "./static/pubs/", 
                   pattern = ".Rmd", 
                   recursive = TRUE,
                   full.names = TRUE)

lapply(Rmds, function(x){rmarkdown::render(x)})



rmarkdown::render_site(encoding = 'UTF-8')
