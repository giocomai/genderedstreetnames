% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_gender.R
\name{find_gender}
\alias{find_gender}
\title{Find if a string refers to a person, and if so, finds gender by searching Wikidata}
\usage{
find_gender(search, language = "en", description_language = "en",
  cache = TRUE, wait = 0.1, only_cached = FALSE,
  exclude_without_uppercase = TRUE, quietly = FALSE)
}
\arguments{
\item{search}{The query to be searched.}

\item{language}{Two letters language codes to be passed to WikiData to determine search results; it works similarly as looking in the corresponding version of Wikipedia.}

\item{description_language}{The language in which preferably to return the description as a two letter code. If not available, defaults to English. If English is not available, defaults to the language used to search in Wikidata. If not available, it returns NA.}

\item{cache}{Logical, defaults to TRUE. IF TRUE, it stores data on the search string and the resulting Wikidata object. Data are stored inside a `wikidata` folder (if it does not exist, it is automatically created).}

\item{wait}{Numeric, defaults to 0.1. Waiting time between queries, relevant when making a large number of queries.}

\item{only_cached}{Logical, defaults to FALSE. If TRUE, it only uses locally cached information.}

\item{exclude_without_uppercase}{Logical, degaults to TRUE. If the search string does not include any upper case letter, then assume it's not a person.}

\item{quietly}{Logical, defaults to FALSE. If TRUE, no warning message appears when no relevent Wikidata entry is found.}
}
\value{
A data.frame (a tibble) with one row and four columns: Query, Gender, Description, and WikidataID
}
\description{
Find if a string refers to a person, and if so, finds gender by searching Wikidata
}
\examples{

find_gender(search = "Garibaldi", language = "it")

}
