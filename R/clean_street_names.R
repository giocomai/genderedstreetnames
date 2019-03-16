#' Find if a string refers to a person, and if so, finds gender by searching Wikidata
#' 
#' @param string A character string.
#' @return A character string. 
#' @examples
#' 
#' Remove_first_word(string = "via Garibaldi")
#' 
#' @export
#' 

Remove_first_word <- function(string) {
  stringr::str_squish(string = stringr::str_remove(string = string, pattern = "\\w+"))
}