#' Split vector into chunk-sized chunks
#'
#'
#' @param x vector to split
#' @param chunk_size chunk size
#'
#' @return list of vectors sized max chunk size
#' @export
chunk_data <- function(x, chunk_size) {
  split(x, ceiling(seq_along(x) / chunk_size))
}



#' Get reporter codes - countries in ISO format
#'
#' Helper function to get vector of valid reporter (or partner) codes using
#' ISO3 which is what `comtradr` funcitons require.
#'
#' @return character vector of valid funs.
#' @export
get_reporter_codes <- function(){
  reporter_codes <- comtradr:::ct_get_ref_table(dataset_id = 'reporter',
                                                update = FALSE,
                                                verbose = FALSE)
  # no idea why they are here, remove them
  reporter_codes |>
    dplyr::filter(iso_3 != "R4 ") |>
    dplyr::filter(iso_3 != "EUR") |>
    dplyr::pull(iso_3)
}


# get commodity codes level 3
#' Get valid commodity codes at certain aggregation
#'
#' Extracts a vector of commodity codes from a specific classification e.g.
#' "S4", "HS".. at a level of aggregation - this is determined solely based
#' on the number of digits in the classificaiton code. Level 3 means it has
#' three digits.
#'
#' @param classification e.g "HS", "H6", "S3", "S4..."
#' @param level numeric, defaults to 3
#'
#' @return vector of legal classificaiton codes
#' @export
#'
get_commodity_codes <- function(classification = "S4", level = 3){
  x <- comtradr::ct_get_ref_table(classification)
  x |>
    dplyr::filter(grepl(paste0("^[0-9]{",level,"}$"), id)) |>
    dplyr::pull(id)
}
