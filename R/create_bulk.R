#' Creates html pages with maps and basic stats for multiple cities
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
    city_boundary <- Get_city_boundaries(city = i, country = country, cache = TRUE)
    file_location_fixed_geo <- suppressWarnings(normalizePath(file.path("data", "gendered_street_names_fixed_geo", country, paste0("city_roads_gender_fixed_geo-", i, ".rds"))))
    file_location_not_fixed_geo <- suppressWarnings(normalizePath(file.path("data", "gendered_street_names_geo", country, paste0("city_roads_gender-", i, ".rds"))))
    if (file.exists(file_location_fixed_geo)) {
      city_roads_path <- file_location_fixed_geo
    } else if (file.exists(file_location_not_fixed_geo)) {
      warning(paste0("Gender has not been manually checked for ", stringr::str_to_title(i)))
      city_roads_path <- file_location_not_fixed_geo
    } else {
      Download_OSM(countries = country)
      
      roads <- Extract_roads(countries = country)
      
      city_boundary <- Get_city_boundaries(city = i, country = country, cache = TRUE)

      ## This section will need a parameter to introduce different approaches for each country
      city_roads <- genderedstreetnames::Subset_roads(boundary = city_boundary, roads = roads) %>% 
        dplyr::mutate(name_clean = Remove_first_word(name)) 
      wiki_street_names <- purrr::map_dfr(.x = city_roads %>% dplyr::pull(name_clean) %>% unique(),
                                          .f = FindGender,
                                          language = "ro",
                                          quietly = TRUE)
      
      city_roads_gender <- city_roads %>% 
        dplyr::left_join(wiki_street_names %>%
                           dplyr::rename(name_clean = Query), by = "name_clean")
      dir.create(file.path("data", "gendered_street_names_geo"), showWarnings = FALSE)
      dir.create(file.path("data", "gendered_street_names_geo", country), showWarnings = FALSE)
      saveRDS(object = city_roads_gender, file = file_location_not_fixed_geo)
      city_roads_path <- file_location_not_fixed_geo
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
