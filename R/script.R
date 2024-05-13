#' Scripting fun for fetch_by_commodity
#'
#'
#'
#' @param start_date start year
#' @param end_date end year
#' @param cmd classification type
#' @param agg_l aggregation level
#' @param chunks list of vectors of valid reporter codes you can get from
#' \link[UMARcomtradeR]{get_reporter_codes}
#'
#' @return dataframe with response
#' @export
reporter_by_world_w_cassifications <- function(start_date = '2011',
                                               end_date = '2022',
                                               cmd = "S4", agg_l = 3, chunks = NULL){
  # split countries into chunks
  if(is.null(chunks)) {
  reporter_codes <- get_reporter_codes()
  chunks <- chunk_data(reporter_codes, 3)}

  results_list <- Map(fetch_by_commodity,
                      chunk = chunks,
                      start_date = start_date,
                      end_date = end_date,
                      cmd = cmd,
                      agg_l = agg_l)

  # Extract the data and failed chunks separately
  data_list <- lapply(results_list, function(x) x$data)
  failed_chunks <- sapply(results_list, function(x) x$failed_chunk)

  # Only keep non-null data and rbind them
  data_non_null <- data_list[sapply(data_list, function(x) !is.null(x))]
  results <- do.call(rbind, data_non_null)

  # Print the failed chunks
  failed_chunks <- failed_chunks[!sapply(failed_chunks, is.null)]
  if (length(failed_chunks) > 0) {
    print(paste("Failed chunks:", paste(failed_chunks, collapse = ", ")))
  }

  if(!is.null(results)){
    results <- results %>%
      dplyr::select(period, reporterCode, reporterISO, reporterDesc,
                    flowCode, flowDesc, partnerCode, partnerISO, partnerDesc,
                    classificationCode, cmdCode, cmdDesc, aggrLevel,
                    qtyUnitAbbr, qty, netWgt, grossWgt, primaryValue) |>
      dplyr::distinct() }

  # Return both results and failed chunks
  list(data = results, failed_chunks = failed_chunks)
}



#' Scripting fun for fetch_full
#'
#' @param reporter singele country
#' @param start_date start year
#' @param end_date end year
#' @param cmd classification type
#' @param agg_l aggregation level
#' @param partner_codes char vector, probably from \link[UMARcomtradeR]{get_partner_codes}
#' @param chunk_size numeric
#'
#' @return dataframe with response
#' @export
full_single_country <- function(reporter = "SVN", start_date = '2011',
                                end_date = '2022', cmd = "S4", agg_l = 3,
                                partner_codes = NULL,
                                chunk_size = 5){
  # split countries into chunks
  if(is.null(partner_codes)) partner_codes <- get_partner_codes()
  chunks <- chunk_data(partner_codes, chunk_size)

  # Use Map to collect both data and failed chunks
  results_list <- Map(fetch_full,
                      reporter = reporter,
                      chunk = chunks,
                      start_date = start_date,
                      end_date = end_date,
                      cmd = cmd,
                      agg_l = agg_l)

  # Extract the data and failed chunks separately
  data_list <- lapply(results_list, function(x) x$data)
  failed_chunks <- sapply(results_list, function(x) x$failed_chunk)

  # Only keep non-null data and rbind them
  data_non_null <- data_list[sapply(data_list, function(x) !is.null(x))]
  results <- do.call(rbind, data_non_null)

  # Print the failed chunks
  failed_chunks <- failed_chunks[!sapply(failed_chunks, is.null)]
  if (length(failed_chunks) > 0) {
    print(paste("Failed chunks:", paste(failed_chunks, collapse = ", ")))
  }

  if(!is.null(results)){
  results <- results %>%
    dplyr::select(period, reporterCode, reporterISO, reporterDesc,
                  flowCode, flowDesc, partnerCode, partnerISO, partnerDesc,
                  classificationCode, cmdCode, cmdDesc, aggrLevel,
                  qtyUnitAbbr, qty, netWgt, grossWgt, primaryValue) |>
    dplyr::distinct()}

  # Return both results and failed chunks
  list(data = results, failed_chunks = failed_chunks)
}



#' Scripting fun for fetch_full
#'
#' @param reporter_codes char vector, probably from \link[UMARcomtradeR]{get_reporter_codes}
#' @param start_date start year
#' @param end_date end year
#' @param cmd classification type
#' @param chunk_size defaults to 5
#'
#' @return dataframe with response
#' @export
reporter_by_partner_total <- function(start_date = '2011',
                                end_date = '2022', cmd = "S4",
                                reporter_codes = NULL,
                                chunk_size = 5){
  # split countries into chunks
  if(is.null(reporter_codes)) reporter_codes <- get_reporter_codes()
  chunks <- chunk_data(reporter_codes, chunk_size)

  # Use Map to collect both data and failed chunks
  results_list <- Map(fetch_by_country,
                      chunk = chunks,
                      start_date = start_date,
                      end_date = end_date,
                      cmd = cmd)

  # Extract the data and failed chunks separately
  data_list <- lapply(results_list, function(x) x$data)
  failed_chunks <- sapply(results_list, function(x) x$failed_chunk)

  # Only keep non-null data and rbind them
  data_non_null <- data_list[sapply(data_list, function(x) !is.null(x))]
  results <- do.call(rbind, data_non_null)
  results <- results |> dplyr::distinct()

  # Print the failed chunks
  failed_chunks <- failed_chunks[!sapply(failed_chunks, is.null)]
  if (length(failed_chunks) > 0) {
    print(paste("Failed chunks:", paste(failed_chunks, collapse = ", ")))
  }

  if(!is.null(results)){
    results <- results %>%
      dplyr::select(period, reporterCode, reporterISO, reporterDesc,
                    flowCode, flowDesc, partnerCode, partnerISO, partnerDesc,
                    classificationCode, cmdCode, cmdDesc, aggrLevel,
                    qtyUnitAbbr, qty, netWgt, grossWgt, primaryValue) }

  # Return both results and failed chunks
  list(data = results, failed_chunks = failed_chunks)
}



