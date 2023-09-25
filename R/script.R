# library(UMARcomtradeR)
# # get reporter codes ie countries
# reporter_codes <- get_reporter_codes()
#
# # get commodity codes level 3
# commodity_codes <- get_commodity_codes()
#
#
# comtradr:::check_reporterCode("R4 ")
#
# # split countries into chunks
# chunks <- chunk_data(reporter_codes, 3)
# # Use lapply with the retry function and concatenate results
# results <- do.call(rbind, lapply(chunks, fetch_by_commodity))
#
# saveRDS(results, file = "reporter_by_world_S4_ag3.rds")
#
# x1 <- fetch_by_commodity(c("SVK"))
# results <- rbind(results, x)
# x2 <- fetch_by_commodity(c("SVN"))
# results <- rbind(results, x2)
# x3 <- fetch_by_commodity(c("SLB"))
# results <- rbind(results, 3)
#
# x2 <- fetch_by_commodity(c("ZA1","ESP","LKA"))
# results <- rbind(results, 2)
#
# # ct <- table(results$flowCode, results$cmdCode, results$reporterISO)
# # sum(ct)
#
#
# #
# # chunks <- chunk_data(reporter_codes, 3)
# # # Use lapply with the new function and concatenate results
# # results_both <- do.call(rbind, lapply(chunks, fetch_by_country))
# #
# # # ct <- table(results_both$flowCode, results_both$partnerISO, results_both$reporterISO)
# # # sum(ct)
# #
# #
# # saveRDS(results_both, file = "reporter_by_partner_totals.rds")
