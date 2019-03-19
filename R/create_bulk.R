#' Get city boundaries 
#' 
#' @param cities A character vector of cities located in the same country.
#' @param country The name of a country. 
#' @return Nothing, used for its side effects (creates html files with maps and basic text)
#' @examples
#' 
#' Create_bulk(cities = c("Sibiu", "Timișoara"), country = "Romania")
#' 
#' @export
#' 

Create_bulk <- function(cities, country) {
  dir.create("html", showWarnings = FALSE) 
  
  # if already processed, don't do them again
  citiesDone <- list.files(path = file.path("html"), include.dirs = FALSE) %>%
    stringr::str_remove(".html")
  cities <- cities[is.element(cities, citiesDone)==FALSE]
  
  for (i in cities) {
    rmarkdown::render(input = system.file("templates", "template.Rmd", package = "genderedstreetnames"),
                      params = list(
                        city = i,
                        country = country
                      ),
                      output_file = paste0(i,".html"),
                      output_dir = "html")
  }
  
  
}