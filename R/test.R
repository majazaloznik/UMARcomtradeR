library(dplyr)
library(tidyr)
path <- "\\\\192.168.38.7\\public$\\Avtomatizacija\\comtrade\\"

reporter_by_partner_total_00_22 <- readRDS( paste0(path, "reporter_by_partner_total_00_22.rds"))
slovenia_full_00_22 <- readRDS( paste0(path, "slovenia_full_00_22.rds"))
reporter_by_world_ag2_ag3_00_22 <- readRDS( paste0(path, "reporter_by_world_ag2_ag3_00_22.rds"))

slovenia_full_00_22 |>
  group_by(period, flowDesc, aggrLevel) |>
  summarise(sum = sum(primaryValue)) |>
  pivot_wider(names_from = aggrLevel, values_from = sum) |>
  left_join(reporter_by_partner_total_00_22 |>
  filter(reporterDesc == "Slovenia") |>
  group_by(period, flowDesc, aggrLevel) |>
  summarise(sum = sum(primaryValue))) |>
  select(-aggrLevel) |>
  mutate(check1 = `1` - sum,
         check2 = `2` - sum,
         check3 = `3` - sum, .keep = "unused")

slovenia_full_00_22 |>
  group_by(period, flowDesc, aggrLevel) |>
  summarise(sum = sum(primaryValue)) |>
  pivot_wider(names_from = aggrLevel, values_from = sum)|>
  left_join(reporter_by_world_ag2_ag3_00_22 |>
  filter(reporterDesc == "Slovenia") |>
  group_by(period, flowDesc, aggrLevel) |>
  summarise(sum = sum(primaryValue)) |>
  pivot_wider(names_from = aggrLevel, values_from = sum),
  by = c("period", "flowDesc")) |>
  mutate(check1 = `1` - `2.y`,
         check2 = `2.x` - `2.y`,
         check3 = `3.x` - `2.y`,
         check4 = `1` - `3.y`,
         check5 = `2.x` - `3.y`,
         check6 = `3.x` - `3.y`,.keep = "unused")


canada <- read.csv("O:\\Users\\MHribernik\\comtrade\\comtrade_2021_124.gz", sep = "\t")
canada <- read.csv("O:\\Users\\MHribernik\\comtrade\\C_A_124_C_A_124_202101_S4_D.txt.gz", sep = "\t")

canada_ag2_ag3 <- reporter_by_world_ag2_ag3_00_22 |>
  filter(reporterCode == 124)

canada_by_partner_total <- reporter_by_partner_total_00_22 |>
  filter(reporterCode == 124)

canada_ag1 <- canada |>
  filter(mosCode == "0", motCode == "0", flowCode %in% c("M", "X"), customsCode == "C00") |>
  filter(grepl("^[0-9]{1}$", cmdCode)) |>
  select(period, flowCode, cmdCode, partnerCode, primaryValue) |>
  mutate(period = as.character(period)) |>
  group_by(period, flowCode, partnerCode) |>
  summarise(sum = sum(primaryValue)) |>
  ungroup() |>
  left_join(canada_by_partner_total |>
              filter(period == "2021"), by = c("period", "flowCode", "partnerCode"))
