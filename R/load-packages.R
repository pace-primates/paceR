#' #' Loads various data munging packages.
#'
#' @export
#' @examples
#' load_pace_packages()
load_pace_packages <- function(...)
{
  packages <- c('adehabitatHR', 'adehabitatLT', 'plyr', 'scales', 'stringr',
                'RColorBrewer', 'rgdal', 'gridExtra', 'rgeos', 'colorspace',
                'stringr', 'sf', 'lubridate', 'tidyverse') 
  for (package in packages) {
    if (!isTRUE(require(package, character.only = TRUE))) {
      install.packages(package)
      require(package, character.only = TRUE, ...)
    } else
      require(package, character.only = TRUE, ...)
  }
}