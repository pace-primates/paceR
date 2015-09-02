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


#' Get the Alphamale-tenure table.
#'
#' @param pace_db The src_mysql connection to the PACE Database.
#' @param full Option to return the full table (TRUE) or just a condensed version (FALSE). Default is TRUE.
#'
#' @export
#' @examples
#' get_alphamale_tenures(pace_db)

get_alphamale_tenures <- function(pace_db, full = TRUE){

  amt <- get_pace_tbl(pace_db, "tblAlphaMaleTenure") %>%
    select(AlphaMaleTenureID = ID, GroupID, AlphaMaleID, AMT_DateStart = DateStart,
            AMT_DateEnd = DateEnd, AMT_Comments = Comments)


  # Get the individuals table
  males_amt <- get_individuals(pace_db) %>%
    select(AlphaMaleID = IndividualID, AlphaMale = NameOf, AlphaMaleDOB = DateOfBirth)

  groups_amt <- get_pace_tbl(pace_db, "tblGroup") %>%
    select(GroupID = ID, Group = NameCode)

  alpha_tenures <- amt %>%
    inner_join(males_amt, by = "AlphaMaleID") %>%
    inner_join(groups_amt, by = "GroupID") %>%
    arrange(GroupID, AMT_DateStart) %>%
    select(GroupID, Group, AMT_DateStart, AMT_DateEnd, AlphaMaleID, AlphaMale,
           AlphaMaleDOB, AMT_Comments, AlphaMaleTenureID)

  alpha_tenures <- alpha_tenures %>%
    mutate_each(funs(as.Date), contains("Date"), AlphaMaleDOB)

  if (!full) {
    alpha_tenures <- alpha_tenures %>%
      select(-GroupID, -AlphaMaleID, -AlphaMaleDOB, -AMT_Comments, -AlphaMaleTenureID)
  }

  return(alpha_tenures)

}
