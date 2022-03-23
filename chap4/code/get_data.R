download <- function(name) {
	url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
	download.file(paste0(url, name), paste0("chap4/data/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")