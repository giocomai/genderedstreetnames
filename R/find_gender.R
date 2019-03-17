#' Find if a string refers to a person, and if so, finds gender by searching Wikidata
#' 
#' @param search The query to be searched.
#' @param language Two letters language codes to be passed to WikiData
#' @param cache Logical, defaults to TRUE. IF TRUE, it stores data on the search string and the resulting Wikidata object. Data are stored inside a `wikidata` folder (if it does not exist, it is automatically created).
#' @param wait Numeric, defaults to 0.1. Waiting time between queries, relevant when making a large number of queries. 
#' @param only_cached Logical, defaults to FALSE. If TRUE, it only uses locally cached information.
#' @param exclude_without_uppercase Logical, degaults to TRUE. If the search string does not include any upper case letter, then assume it's not a person.
#' @return A data.frame (a tibble) with one row and four columns: Query, Gender, Description, and WikidataID
#' @examples
#' 
#' FindGender(search = "Garibaldi", language = "it")
#' 
#' @export
#' 


FindGender <- function(search,
                       language = "en",
                       cache = TRUE,
                       wait = 0.1,
                       only_cached = FALSE,
                       exclude_without_uppercase = TRUE,
                       quietly = FALSE) {
  # If no capital letters, just say it's not a person
  if (is.na(search)==FALSE) {
    if (exclude_without_uppercase==TRUE&stringr::str_detect(string = search, pattern = stringr::regex(pattern = "\\p{Lu}"))==FALSE) {
      return(tibble::tibble(Query = search, Gender = as.character(NA), Description = NA, WikidataID = NA)) 
    }
    
    if (cache==TRUE) {
      dir.create(path = "wikidata", showWarnings = FALSE)
      dir.create(path = file.path("wikidata", "search"), showWarnings = FALSE)
      dir.create(path = file.path("wikidata", "search", language), showWarnings = FALSE)
      dir.create(path = file.path("wikidata", "item"), showWarnings = FALSE)
    }
    # search term
    search_response_location <- file.path("wikidata", "search", language, paste0(gsub(pattern = "/", replacement = "_", x = search), ".rds"))
    if (file.exists(search_response_location)==FALSE) {
      if (only_cached==TRUE) {
        return(NULL)
      }
      search_response <- tryCatch(WikidataR::find_item(search_term = search, language = language, limit = 3), error = function(e) warning(e))
      if (is.null(search_response)) (return(NULL))
      saveRDS(object = search_response, file = search_response_location)
    } else {
      search_response <- readRDS(file = search_response_location)
    }
    # search among results to find person
    for (i in seq_along(search_response)) {
      item_response_location <- file.path("wikidata", "item", paste0(search_response[[i]]$id, ".rds"))
      if (file.exists(item_response_location)==FALSE) {
        item <- tryCatch(WikidataR::get_item(id = search_response[[i]]$id), error = function(e) return(tibble::tibble(Query = search, Gender = NA, Description = NA, WikidataID = NA)))
        saveRDS(object = item, file = item_response_location)
        Sys.sleep(time = wait)
      } else {
        item <- readRDS(item_response_location)
      }
      
      claim <- WikidataR::extract_claims(items = item, claims = "P21")
      if (is.na(claim[1][[1]][[1]])[1]) {
        # if not person, return description
        #        return(tibble::tibble(Query = search, Gender = NA, Description = item[[1]]$descriptions[[1]][[2]], WikidataID = item[[1]]$id))
        
      } else {
        # if person, check gender
        genderCode <- claim[1][[1]][[1]]$mainsnak$datavalue$value$id
        if (is.null(genderCode)) {
          gender <- NA
        } else if (length(genderCode)==0) {
          gender <- NA
        } else if (genderCode == "Q6581097") {
          gender <-  "male"
        } else if (genderCode == "Q6581072") {
          gender <- "female"
        } else {
          gender <- "other"
        }
        
        if (length(item[[1]]$descriptions)>0) {
          description <- item[[1]]$descriptions[[1]][[2]]
        } else {
          description <- NA
        }
        return(tibble::tibble(Query = search, Gender = as.character(gender), Description = description, WikidataID = item[[1]]$id))
      }
    }
  } 
  warning(paste("No Wikidata entry matches search", dQuote(search)))
  return(tibble::tibble(Query = search, Gender = as.character(NA), Description = NA, WikidataID = NA))
}
