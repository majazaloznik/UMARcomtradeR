#' Fetch trade flows by commodity codes
#'
#' Function to fetch records of goods imports and exports for each reporting
#' country for each commodity code classification with the rest of the world.
#' Uses teh S4 agg level 3 commodity classification and fetches annual data for both
#' imports and exports of goods over a specified period of time. The period can
#' be max 12 years.
#'
#' Due to API limits but even more so due to random internal server errors that
#' keep cropping up, this funciton should be used with small chunks of reporter
#' countries. Chunk sizes of 3 work quite reliably.
#'
#' The funciton also attemts 3 retries in case of server errors, and will report
#' any chunks that were not completed in the end.
#'
#' @param chunk vector of valid reporter codes you can get from \link[UMARcomtradeR]{get_reporter_codes}
#' @param start_date character value od start year
#' @param end_date character value od end year
#'
#' @return fetched data or NULL
#' @export
#'
fetch_by_commodity <- function(chunk, start_date = '2011', end_date = '2022') {
  max_retries <- 3
  for (i in 1:max_retries) {
    # Try making the API request
    print(chunk)
    try_result <- try({
      data <- comtradr::ct_get_data(
        type = 'goods',
        commodity_classification = 'S4',
        commodity_code = get_commodity_codes(),
        reporter = chunk,
        partner = 'World',
        start_date = start_date,
        end_date = end_date,
        flow_direction = c('import', 'export'),
        verbose = TRUE
      )
      if (is.data.frame(data) && ncol(data) == 1 && nrow(data) == 1 && data[1, 1] == 0) {
        return(NULL)
      } else {
        return(data)
      }
    }, silent = TRUE)

    # If successful, break out of the loop
    if (!inherits(try_result, "try-error")) {
      return(try_result)
    }

    # If not successful, wait before retrying
    Sys.sleep(5)
  }
  # If all retries fail, return NULL or stop with an error message
  warning(paste("Failed to fetch data for chunk:", paste(chunk, collapse = ",")))
  return(NULL)
}


#' Fetch trade flows by partner country
#'
#' Function to fetch records of goods imports and exports for each reporting
#' country and partner country combination (for the totals only).
#' Fetches annual data for both imports and exports of goods over a specified
#' period of time. The period can be max 12 years.
#'
#' Due to API limits but even more so due to random internal server errors that
#' keep cropping up, this funciton should be used with small chunks of reporter
#' countries. Chunk sizes of 3 work quite reliably.
#'
#' The funciton also attemts 3 retries in case of server errors, and will report
#' any chunks that were not completed in the end.
#'
#' @param chunk vector of valid reporter codes you can get from \link[UMARcomtradeR]{get_reporter_codes}
#' @param start_date character value od start year
#' @param end_date character value od end year
#'
#' @return fetched data or NULL
#' @export
#'
fetch_by_country <- function(chunk, start_date = '2011', end_date = '2022') {
  max_retries <- 3
  for (i in 1:max_retries) {
    # Try making the API request
    print(chunk)
    try_result <- try({
      data <- comtradr::ct_get_data(
        type = 'goods',
        commodity_classification = 'S4',
        reporter = chunk,
        partner = get_reporter_codes(),
        start_date = '2011',
        end_date = '2022',
        flow_direction = c('import', 'export'),
        verbose = TRUE
      )
      if (is.data.frame(data) && ncol(data) == 1 && nrow(data) == 1 && data[1, 1] == 0) {
        return(NULL)
      } else {
        return(data)
      }
    }, silent = TRUE)

    # If successful, break out of the loop
    if (!inherits(try_result, "try-error")) {
      return(try_result)
    }

    # If not successful, wait before retrying
    Sys.sleep(5)
  }
  # If all retries fail, return NULL or stop with an error message
  warning(paste("Failed to fetch data for chunk:", paste(chunk, collapse = ",")))
  return(NULL)
}




#' Fetch trade flows by partner country and commodity codes for single country
#'
#' Function to fetch records of goods imports and exports for a sinlge reporting
#' country and all partner country combination by commodity codes.
#'
#' Fetches annual data for both imports and exports of goods over a specified
#' period of time. The period can be max 12 years.
#'
#' Due to API limits but even more so due to random internal server errors that
#' keep cropping up, this funciton may not always complete.
#'
#' The funciton also attemts 3 retries in case of server errors.
#'
#' @param reporter character value of valid reporter codes you can get from \link[UMARcomtradeR]{get_reporter_codes}
#' @param start_date character value od start year
#' @param end_date character value od end year
#'
#' @return fetched data or NULL
#' @export
#'
fetch_full <- function(reporter = "SVN", start_date = '2011', end_date = '2022') {
  max_retries <- 3
  for (i in 1:max_retries) {
    # Try making the API request
    print(chunk)
    try_result <- try({
      data <- comtradr::ct_get_data(
        type = 'goods',
        commodity_classification = 'S4',
        commodity_code = get_commodity_codes(),
        reporter = repoerter,
        partner = get_reporter_codes(),
        start_date = '2011',
        end_date = '2022',
        flow_direction = c('import', 'export'),
        verbose = TRUE
      )
      if (is.data.frame(data) && ncol(data) == 1 && nrow(data) == 1 && data[1, 1] == 0) {
        return(NULL)
      } else {
        return(data)
      }
    }, silent = TRUE)

    # If successful, break out of the loop
    if (!inherits(try_result, "try-error")) {
      return(try_result)
    }

    # If not successful, wait before retrying
    Sys.sleep(5)
  }
  # If all retries fail, return NULL or stop with an error message
  warning(paste("Failed to fetch data for:", paste(reporter, collapse = ",")))
  return(NULL)
}
