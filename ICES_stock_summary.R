rm(list = ls())
library(dplyr)
library(tidyr)
library(glue)
# devtools::install_github("ices-tools-prod/icesSAG")
library(icesSAG)
source("utilities.R")
# library(openxlsx)

active_year <- 2017
stock_summary_table <- get_stock_summary(active_year = 2017, output_type = "csv")
write.csv(stock_summary_table, file = "stock_summary_11122017.csv", row.names = FALSE)
