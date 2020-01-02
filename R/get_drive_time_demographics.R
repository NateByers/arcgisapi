#' Get demographic data from ArcGIS
#'
#'
#' @param token API access token for ArcGIS
#' @param x Easting (longitude)
#' @param y Northing (lattitude)
#' @param drive_time Distance in driving time
#' @param unit Drive time unit (Hours or Minutes)
#' @param demographics What demographic information (see Description)
#' @description This function queries demographic and lifestyle information within
#' a drive time polygon from the ArcGIS World GeoEnrichmentService. See the
#' documentation here:
#' https://developers.arcgis.com/rest/geoenrichment/api-reference/enrich.htm
#' @importFrom dplyr %>% select rename mutate summarize group_by pull
#' @importFrom tidyr spread gather
#' @export
#' @examples
#' \dontrun{
#' client_id <- "your client id"
#' client_secret <- "your client secret"
#'
#' token <- generate_token(client_id, client_secret)
#'
#' get_drive_time_demographics(token, -86.157963, 39.768454)
#' }
get_drive_time_demographics <- function(token, x, y, drive_time = 10,
                                        unit = "Minutes",
                                        demographics = c("KeyGlobalFacts",
                                                         "KeyUSFacts")) {

  study_areas <- paste0('[{"geometry":{"x":', x, ',"y":', y,
                        '},"areaType":"NetworkServiceArea","bufferUnits":"',
                        unit, '","bufferRadii":[', drive_time,
                        '],"travel_mode":"Driving"}]')

  data_collections <- paste0('["', paste(demographics, collapse = '", "'), '"]')

  demographics <- httr::POST("https://geoenrich.arcgis.com/arcgis/rest/services/World/geoenrichmentserver/GeoEnrichment/enrich",
                             httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded'),
                             body = list(f = "json",
                                         token = token,
                                         outSr = "4326",
                                         nSRi = "4326",
                                         studyAreas = study_areas,
                                         returnGeometry = "true",
                                         dataCollections = data_collections),
                             encode = "form")

  content <- httr::content(demographics)

  json <- jsonlite::fromJSON(content)

  features <- json$results$value$FeatureSet[[1]]$features[[1]]$attributes

  aliases <- json$results$value$FeatureSet[[1]]$fieldAliases

  for(i in 1:ncol(features)) {
    names(features)[i] <- aliases[1, i]
  }

  geometry <- json$results$value$FeatureSet[[1]]$features[[1]]$geometry$rings[[1]] %>%
    as.data.frame.table() %>%
    tidyr::spread(Var3, Freq) %>%
    dplyr::rename(x = A, y  = B) %>%
    dplyr::select(x, y)

  demographics <- list(features = features, geometry = geometry)

  demographics
}
