#' Generate an access token
#'
#' @param client_id ID for the ArcGIS REST API
#' @param client_secret Secret for the ArcGIS REST API
#' @param expiration The length of time the token should last in minutes
#' @references https://developers.arcgis.com/
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#' @export
generate_token <- function(client_id, client_secret, expiration = "60") {

  url <- paste0("https://www.arcgis.com/sharing/rest/oauth2/token?client_id=",
                client_id, "&client_secret=", client_secret,
                "&grant_type=client_credentials&expiration=", expiration)
  token <- httr::POST(url)
  token <- httr::content(token)
  token <- jsonlite::fromJSON(token)$access_token

  token
}
