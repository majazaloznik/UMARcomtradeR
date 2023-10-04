#' Replacemente for ct_check_partnerCode
#'
#' @param partner code
#' @param update logical
#' @param verbose logical
#'
#' @return iso code
#' @export
mz_check_partnerCode <- function (partner, update = FALSE, verbose = FALSE)
{
  iso_3 <- id <- group <- NULL
  if (!is.null(partner)) {
    partner <- as.character(partner)
  }
  else {
    rlang::abort("You need to provide at least one partner.")
  }
  partner_codes <- comtradr:::ct_get_ref_table(dataset_id = "partner",
                                    update = update, verbose = verbose)
  if (length(partner) > 1 | !any(partner == "all")) {
    partner <- substr(partner, 1,3)
    if (any(partner == "all")) {
      rlang::abort("\"all\" can only be provided as a single argument.")
    }
    if (!all(partner %in% partner_codes$iso_3)) {
      rlang::abort(paste("The following partner you provided are invalid: ",
                         setdiff(partner, partner_codes$iso_3), collapse = ", "))
    }
  }
  if (length(partner) > 1 | !any(partner == "all")) {
    partner <- paste(poorman::pull(poorman::filter(partner_codes,
                                                   iso_3 %in% partner), id), collapse = ",")
  }
  else if (partner == "all") {
    partner <- paste(poorman::pull(poorman::filter(partner_codes,
                                                   group == FALSE), id), collapse = ",")
  }
  return(partner)
}


#' Replacememnt for ct_check_params
#'
#' @param frequency The frequency of returned trade data. A character value. Possible values are: 'A' for annual data and 'M' for monthly data. The default value is 'A'.
#' @param type The type of returned trade data. A character value. Possible values are: 'goods' for trade in goods and 'services' for trade in services. The default value is 'goods'.
#' @param commodity_classification The trade (IMTS) classification scheme. A character value. The only possible value is 'HS'. This is the default.
#' @param commodity_code The commodity code(s). A character vector. All possible values are provided in the `comtradr::ct_get_ref_table()` function. You should use the relevant value from the `id` column. The default value is 'TOTAL': the sum of all commodities.
#' @param flow_direction The direction of trade flows. A character vector. Possible values are: 'import' for imports, 'export' for exports, 're-import' for re-imports, 're-export' for re-exports, or 'all' for imports, exports, re-imports, and re-exports. The default value is 'all'.
#' @param reporter Reporter ISO3 code(s). A character vector. Possible values include the desired country's ISO3 code. A full list of these can be found in the `comtradr::country_codes` dataset. 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries). The default value is 'all'.
#' @param partner Partner ISO3 code(s). A character vector. Possible values include the desired country's ISO3 code. A full list of these can be found in the `comtradr::country_codes` dataset. 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries). The default value is 'World' which returns the trade with all partner countries as an aggregate.
#' @param start_date The start date of the query. A character value. Yearly values should be in the format: `yyyy`. Monthly values should be in the format: `yyyy-mm`.
#' @param end_date The end date of the query. A character value. Yearly values should be in the format: `yyyy`. Monthly values should be in the format: `yyyy-mm`. This can be a maximum of 12 years after the start date for the annual data or one year after the start date for monthly data.
#' @param verbose A logical value. If TRUE, sends status updates to the console. If FALSE, runs functions quietly.
#' @param update A logical value. If TRUE, will download the possibly updated reference tables from the UN.
#' @param ... You can pass in further parameters to the API that will not be checked and passed on as query parameters as is.
#' @param mode_of_transport The Mode of Transport is set to `0`, which is the default for TOTAL across all modes of transportation.
#' @param partner_2 This value is set as a default to `0`, which is most likely the most general value and also the default on the Comtrade website.
#' @param customs_code The customs code is set to the default of `C00` which is the default for TOTAL across all customs procedures.
#'
#' @return
#' @export

mz_check_params <- function (type, frequency, commodity_classification, commodity_code,
                             flow_direction, reporter, partner, start_date, end_date,
                             mode_of_transport, partner_2, customs_code, update, verbose,
                             ...)
{
  type <- comtradr:::check_type(type)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of type."))
  }
  frequency <- comtradr:::check_freq(type, frequency)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of frequency."))
  }
  commodity_classification <- comtradr:::check_clCode(type, commodity_classification)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of commodity_classification."))
  }
  flow_direction <- comtradr:::check_flowCode(flow_direction)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of flow_direction."))
  }
  commodity_code <- comtradr:::check_cmdCode(commodity_classification,
                                             commodity_code, update = update, verbose = verbose)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of commodity_code."))
  }
  reporter <- comtradr:::check_reporterCode(reporter, update = update,
                                            verbose = verbose)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of reporter."))
  }
  partner <- mz_check_partnerCode(partner, update = update, verbose = verbose)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of partner."))
  }
  partner_2 <- comtradr:::check_partner2Code(partner_2, update = update,
                                             verbose = verbose)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of partner_2."))
  }
  mode_of_transport <- comtradr:::check_motCode(mode_of_transport, update = update,
                                                verbose = verbose)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of mode_of_transport."))
  }
  customs_code <- comtradr:::check_customsCode(customs_code, update = update,
                                               verbose = verbose)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of customs_code."))
  }
  period <- comtradr:::check_date(start_date, end_date, frequency)
  if (verbose) {
    cli::cli_inform(c(v = "Checked validity of start and end dates."))
  }
  params <- list(query_params = list(cmdCode = commodity_code,
                                     flowCode = flow_direction, partnerCode = partner, reporterCode = reporter,
                                     period = period, motCode = mode_of_transport, partner2Code = partner_2,
                                     customsCode = customs_code, ...), url_params = list(type = type,
                                                                                         freq = frequency, clCode = commodity_classification))
  return(params)
}


#' Replacement for ct_get_data
#'
#' @param frequency The frequency of returned trade data. A character value. Possible values are: 'A' for annual data and 'M' for monthly data. The default value is 'A'.
#' @param type The type of returned trade data. A character value. Possible values are: 'goods' for trade in goods and 'services' for trade in services. The default value is 'goods'.
#' @param commodity_classification The trade (IMTS) classification scheme. A character value. The only possible value is 'HS'. This is the default.
#' @param commodity_code The commodity code(s). A character vector. All possible values are provided in the `comtradr::ct_get_ref_table()` function. You should use the relevant value from the `id` column. The default value is 'TOTAL': the sum of all commodities.
#' @param flow_direction The direction of trade flows. A character vector. Possible values are: 'import' for imports, 'export' for exports, 're-import' for re-imports, 're-export' for re-exports, or 'all' for imports, exports, re-imports, and re-exports. The default value is 'all'.
#' @param reporter Reporter ISO3 code(s). A character vector. Possible values include the desired country's ISO3 code. A full list of these can be found in the `comtradr::country_codes` dataset. 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries). The default value is 'all'.
#' @param partner Partner ISO3 code(s). A character vector. Possible values include the desired country's ISO3 code. A full list of these can be found in the `comtradr::country_codes` dataset. 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries). The default value is 'World' which returns the trade with all partner countries as an aggregate.
#' @param start_date The start date of the query. A character value. Yearly values should be in the format: `yyyy`. Monthly values should be in the format: `yyyy-mm`.
#' @param end_date The end date of the query. A character value. Yearly values should be in the format: `yyyy`. Monthly values should be in the format: `yyyy-mm`. This can be a maximum of 12 years after the start date for the annual data or one year after the start date for monthly data.
#' @param primary_token Your primary UN Comtrade API token. A character value. Default is to check in environment for stored token, if not passed through the `comtradr::set_primary_comtrade_key` function.
#' @param process A logical value. If TRUE, returns a data.frame with the results. If FALSE, returns the raw httr2 request. Defaults to TRUE.
#' @param verbose A logical value. If TRUE, sends status updates to the console. If FALSE, runs functions quietly.
#' @param update A logical value. If TRUE, will download the possibly updated reference tables from the UN.
#' @param ... You can pass in further parameters to the API that will not be checked and passed on as query parameters as is.
#' @param mode_of_transport The Mode of Transport is set to `0`, which is the default for TOTAL across all modes of transportation.
#' @param partner_2 This value is set as a default to `0`, which is most likely the most general value and also the default on the Comtrade website.
#' @param customs_code The customs code is set to the default of `C00` which is the default for TOTAL across all customs procedures.
#' @return
#' @export
mz_get_data <- function (type = "goods", frequency = "A", commodity_classification = "HS",
                         commodity_code = "TOTAL", flow_direction = "all",
                         reporter = "all", partner = "World", start_date = NULL,
                         end_date = NULL, process = TRUE, verbose = FALSE, primary_token = comtradr:::get_primary_comtrade_key(),
                         mode_of_transport = "0", partner_2 = "World",
                         customs_code = "C00", update = FALSE, ...) {
  params <- mz_check_params(type = type, frequency = frequency,
                            commodity_classification = commodity_classification,
                            commodity_code = commodity_code, flow_direction = flow_direction,
                            partner = partner, reporter = reporter, start_date = start_date,
                            end_date = end_date, verbose = verbose, mode_of_transport = mode_of_transport,
                            partner_2 = partner_2, customs_code = customs_code, includeDesc = "TRUE",
                            update = update, ...)
  req <- comtradr:::ct_build_request(params, verbose = verbose, primary_token = primary_token)
  resp <- comtradr:::ct_perform_request(req, verbose = verbose)
  if (process) {
    result <- comtradr:::ct_process_response(resp, verbose = verbose)
    return(result)
  }
  else {
    return(resp)
  }
}
