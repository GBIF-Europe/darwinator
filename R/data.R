#' Darwin Core Terms
#'
#' A dataset containing recommended Darwin Core terms - http://rs.tdwg.org/dwc/
#'
#' @format A data frame with 201 rows and 7 variables:
#' \describe{
#'   \item{label}{the name or label for the term}
#'   \item{is_simple}{boolean that indicates that the term belongs to a subset of the Darwin Core terms that can be used as field or column names and that have common use across a wide variety of biodiversity applications}
#'   \item{definition}{the term definition which describes shortly the intended use for the term}
#'   \item{term_iri}{the internationalized version of a URI - a more modern Unicode-style URI - internationalized version of URI}
#'   \item{issued}{the date of issue for the term as determined by TDWG}
#'   \item{iri}{link to follow for metadata about the term}
#'   \item{rdf_type}{the RDF type for the term}
#' }
#' @source \url{https://github.com/tdwg/dwc/}
"dwc_terms"
