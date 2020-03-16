#' Get lat/long for address
#'
#' @param address,state,zip_code Address for geocoding
#' @export
get_lat_long <- function(token, address, state, zip_code) {

  # address <- "2 Slartibartfast"; state = "IN"; zip_code = "46143"

  addresses <- paste0('{"records": [{"attributes": {"OBJECTID": 1, "Address": "',
                      address, '", "Region": "', state, '", "Postal": "', zip_code,
                      '"}}]}')

  lat_long <- httr::POST("https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/geocodeAddresses?",
                           httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded'),
                           body = list(f = "json",
                                       token = token,
                                       addresses = addresses),
                           encode = "form")

  content <- httr::content(lat_long)

  location <- data.frame(address = address,
                         city = content$locations[[1]]$attributes$City,
                         state = state,
                         zip_code = zip_code,
                         x = content$locations[[1]]$location$x,
                         y = content$locations[[1]]$location$y,
                         stringsAsFactors = FALSE)

  location
}
