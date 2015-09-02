#' Get any table from PACE without the annoying warning messages.
#' Note that the foreign key IDs are not matched to their readable equivalents.
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param tbl_name Name of the table, view, or query you want to get.
#' @param collect Option to collect the data or not. Default is TRUE.
#'
#' @export
#' @examples
#' get_pace_tbl(pace_db, "tblGroup")

get_pace_tbl <- function(pace_db, tbl_name, collect = TRUE){

  get_pace_tbl_internal <- function(pace_db, tbl_name, collect){

    t1 <- pace_db %>%
      tbl(tbl_name) %>%
      select(everything(), -matches("TimeStamp"), -matches("UserAdd"))

    if (collect) {
      return(collect(t1))
    }
    else {
     return(t1)
    }
  }

  t2 <- suppressWarnings(get_pace_tbl_internal(pace_db, tbl_name, collect))

  return(t2)

}
