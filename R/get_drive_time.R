#' Get drive time between two points
#'
#' @param x1,y1,x2,y2 Eastings and northings of two points
#' @export
get_drive_time <- function(token, x1, y1, x2, y2) {

  # x1 <- -118.243529; y1 <- 34.053879; x2 <- -118.273939; y2 <- 34.123480

  stops <- paste0('{"type":"features","features":  [{"geometry": {"x":', x1,
                  ',"y":', y1, ',"spatialReference": {"wkid": "4326"}}},{"geometry": {"x":',
                  x2, ',"y":', y2, ',"spatialReference": {"wkid": "4326"}}}]}')

  drive_time <- httr::POST("https://route.arcgis.com/arcgis/rest/services/World/Route/NAServer/Route_World/solve",
                             httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded'),
                             body = list(f = "json",
                                         token = token,
                                         stops = stops),
                             encode = "form")

  content <- httr::content(drive_time)

  travel_time <- content$routes$features[[1]]$attributes$Total_TravelTime

  travel_time
}
