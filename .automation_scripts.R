library(dplyr)
path <- "\\\\192.168.38.7\\public$\\Avtomatizacija\\comtrade\\"

# scrpt for SLovenia full
slovenia_full_11_22_ag1 <- full_single_country(reporter = "SVN", start_date = '2011', end_date = '2022', cmd = "S4", agg_l = 1)

# missing_11_22_ag1 <- unlist(slovenia_full_11_22_ag1$failed_chunks)
# slovenia_full_missing_11_22_ag1 <- full_single_country(reporter = "SVN", start_date = '2011', end_date = '2022', cmd = "S4", agg_l = 1,
#                                                        partner_codes =  missing_11_22_ag1, chunk_size = 1)
# slovenia_full_11_22_ag1 <- rbind(slovenia_full_11_22_ag1$data,
#                                  slovenia_full_missing_11_22_ag1$data)

slovenia_full_11_22_ag1 <- slovenia_full_11_22_ag1$data

slovenia_full_11_22_ag2 <- full_single_country(reporter = "SVN", start_date = '2011', end_date = '2022', cmd = "S4", agg_l = 2)
slovenia_full_11_22_ag2 <- slovenia_full_11_22_ag2$data

slovenia_full_11_22_ag3 <- full_single_country(reporter = "SVN", start_date = '2011', end_date = '2022', cmd = "S4", agg_l = 3)
slovenia_full_11_22_ag3 <- slovenia_full_11_22_ag3$data

slovenia_full_11_22 <- rbind(slovenia_full_11_22_ag1,
                             slovenia_full_11_22_ag2,
                             slovenia_full_11_22_ag3)
saveRDS(slovenia_full_11_22, paste0(path, "slovenia_full_11_22.rds"))


slovenia_full_00_10_ag1 <- full_single_country(reporter = "SVN", start_date = '2000', end_date = '2010', cmd = "SS", agg_l = 1)
slovenia_full_00_10_ag2 <- full_single_country(reporter = "SVN", start_date = '2000', end_date = '2010', cmd = "SS", agg_l = 2)
slovenia_full_00_10_ag3 <- full_single_country(reporter = "SVN", start_date = '2000', end_date = '2010', cmd = "SS", agg_l = 3)

slovenia_full_00_10_ag1 <- slovenia_full_00_10_ag1$data
slovenia_full_00_10_ag2 <- slovenia_full_00_10_ag2$data
slovenia_full_00_10_ag3 <- slovenia_full_00_10_ag3$data

slovenia_full_00_10 <- rbind(slovenia_full_00_10_ag1,
                             slovenia_full_00_10_ag2,
                             slovenia_full_00_10_ag3)
saveRDS(slovenia_full_00_10, paste0(path, "slovenia_full_00_10.rds"))

slovenia_full_00_22 <- rbind(slovenia_full_00_10,
                             slovenia_full_11_22) |>
  dplyr::arrange(period, partnerDesc, cmdCode, flowDesc)

saveRDS(slovenia_full_00_22, paste0(path, "slovenia_full_00_22.rds"))

# reporter_by_partner_total_00_10
reporter_by_partner_total_00_10 <- reporter_by_partner_total(start_date = '2000',
                                                             end_date = '2010', cmd = "SS",
                                                             chunk_size = 5)

reporter_by_partner_total_11_22 <- reporter_by_partner_total(start_date = '2011',
                                                             end_date = '2022', cmd = "SS",
                                                             chunk_size = 5)
reporter_by_partner_total_00_10$failed_chunks
reporter_by_partner_total_11_22$failed_chunks

reporter_by_partner_total_00_10 <- reporter_by_partner_total_00_10$data
reporter_by_partner_total_11_22 <- reporter_by_partner_total_11_22$data
reporter_by_partner_total_00_22 <- rbind(reporter_by_partner_total_00_10,
                                         reporter_by_partner_total_11_22)|>
  dplyr::arrange(period, reporterDesc, partnerDesc, flowDesc)

saveRDS(reporter_by_partner_total_00_22, paste0(path, "reporter_by_partner_total_00_22.rds"))


# reporter by world ag3 and ag2
reporter_by_world_ag2_00_10$failed_chunks
reporter_by_world_ag2_11_22$failed_chunks
reporter_by_world_ag3_00_10$failed_chunks
reporter_by_world_ag3_11_22$failed_chunks

reporter_by_world_ag2_00_10 <- reporter_by_world_ag2_00_10$data
reporter_by_world_ag2_11_22 <- reporter_by_world_ag2_11_22$data
reporter_by_world_ag3_00_10 <- reporter_by_world_ag3_00_10$data
reporter_by_world_ag3_11_22 <- reporter_by_world_ag3_11_22$data


reporter_by_world_ag2_ag3_00_22 <- rbind(reporter_by_world_ag2_00_10,
                                         reporter_by_world_ag2_11_22,
                                         reporter_by_world_ag3_00_10,
                                         reporter_by_world_ag3_11_22)

saveRDS(reporter_by_world_ag2_ag3_00_22, paste0(path, "reporter_by_world_ag2_ag3_00_22.rds"))
