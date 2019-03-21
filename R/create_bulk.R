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
  cities <- tolower(cities)
  country <- tolower(country)
  
  dir.create("html", showWarnings = FALSE) 
  
  # if already processed, don't do them again
  citiesDone <- list.files(path = file.path("html"), include.dirs = FALSE) %>%
    stringr::str_remove(".html")
  cities <- cities[is.element(cities, citiesDone)==FALSE]
  
  if (length(citiesDone)>0) {
    message(paste0("The following cities have already been processed. To re-create the files, delete them first: ", paste(citiesDone, sep = ", ")))
  }

  for (i in cities) {
    file_location_fixed <- normalizePath(file.path("data", "gendered_street_names_fixed_geo", country, paste0("city_roads_gender_fixed_geo-", i, ".rds")))
    if (file.exists(file_location_fixed)) {
      city_roads_path <- file_location_fixed
    } else if (file.exists(normalizePath(file.path("data", "gendered_street_names", country, paste0("city_roads_gender-", i, ".rds"))))) {
      city_roads_path <- normalizePath(file.path("data", "gendered_street_names", country, paste0("city_roads_gender-", i, ".rds")))
    } else {
      Download_OSM(countries = country)
      
      roads <- Extract_roads(countries = country)
      
      city_boundary <- Get_city_boundaries(city = i, country = country, cache = TRUE)
      
      if (country=="romania") {
        city_roads <- Subset_roads(boundary = city_boundary, roads = roads) %>% 
          mutate(name_clean = Remove_first_word(name)) 
        if (file.exists(file.path("data", "wiki_street_names", country, paste0("wiki_street_names-", country, ".rds")))==FALSE) {
          wiki_street_names <- purrr::map_dfr(.x = city_roads %>% pull(name_clean) %>% unique(),
                                              .f = FindGender,
                                              language = "ro",
                                              quietly = TRUE)
          
          dir.create(path = file.path("data", "wiki_street_names"), showWarnings = FALSE)
          dir.create(path = file.path("data", "wiki_street_names", "romania"), showWarnings = FALSE)
          saveRDS(object = wiki_street_names, file = file.path("data", "wiki_street_names", "romania", "wiki_street_names-romania.rds"))
        } else {
          wiki_street_names <- readRDS(file.path("data", "wiki_street_names", "romania", "wiki_street_names-romania.rds"))
        }
      }
    
      city_roads_gender <- city_roads %>% 
        left_join(wiki_street_names %>% rename(name_clean = Query), by = "name_clean")
      
      city_roads_path <- normalizePath(file.path("data", "gendered_street_names", country, paste0("city_roads_gender-", i, ".rds")))
      
      saveRDS(object = city_roads_path, file = city_roads_path)
    }

    
    rmarkdown::render(input = system.file("template.Rmd", package = "genderedstreetnames"),
                      params = list(
                        city = i,
                        country = country, 
                        city_roads_path = city_roads_path
                      ),
                      output_file = paste0(i,".html"),
                      output_dir = "html")
  }
  
  
}
