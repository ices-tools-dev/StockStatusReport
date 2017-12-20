
library(dplyr)
library(tidyr)
library(glue)
# devtools::install_github("ices-tools-prod/icesSAG")
library(icesSAG)
source("utilities.R")

active_year <- 2017
stock_summary_table <- get_stock_summary(active_year = 2017, output_type = "docx")

png_list <- c("GREEN.png", "GREY.png", "ORANGE.png", 
              "qual_DOWN.png", "qual_GREEN.png", "qual_RED.png",
              "qual_STEADY.png", "qual_UP.png", "RED.png", "UNDEFINED.png")

png_list <- png_list[!png_list %in% list.files(path = ".", pattern = "*..png")]

if(length(png_list) != 0L){
  
  lapply(0:9, function(x) download.file(sprintf("http://sg.ices.dk/download/icons/%s.png", x), 
                                        destfile = sprintf("%s.png", x),
                                        mode = "wb"))
  
  png_names <- data.frame(status = list.files(pattern="\\d{1}.png")) %>%
    mutate(status = gsub(".png", "", status),
           status = case_when(status == 0 ~ "UNDEFINED.png",
                     status == 1 ~ "GREEN.png",
                     status == 2 ~ "qual_GREEN.png", #qualitative green
                     status == 3 ~ "ORANGE.png",
                     status == 4 ~ "RED.png",
                     status == 5 ~ "qual_RED.png", #qualitative red
                     status == 6 ~ "GREY.png",
                     status == 7 ~ "qual_UP.png",
                     status == 8 ~ "qual_STEADY.png",
                     status == 9 ~ "qual_DOWN.png",
                     TRUE ~ NA_character_)) %>% 
    pull(status)
  
  file.rename(list.files(pattern="\\d{1}.png"), png_names)
}

colkeys <- colnames(stock_summary_table[,names(stock_summary_table) != c("SpeciesScientificName")])
masterkeys <- c("stock_code", "StockKeyDescription", 
                "YearOfLastAssessment", "DataCategory",
                "AdviceCategory")

foldtype <- c("lim", "MSY", "PA", "MGT", "Qual")
for(fold in foldtype){
  if(fold == "lim"){
    foldkeys <- c(masterkeys, colkeys[grepl(fold, colkeys)])
  } else if(fold != "lim") {
    foldkeys <- colkeys[grepl(fold, colkeys)]
  }
  iter_val = 1
  iter <- NULL
  for(iter in 1:6){
    sst <- stock_summary_table[iter_val:(43*iter),]
    FT <- sst %>% 
      select(one_of(foldkeys)) %>%
      flextable::flextable(col_keys = foldkeys)
    
    col_name <- foldkeys
    for(i in 1:length(col_name)) {
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'GREEN'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "GREEN.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'GREY'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "GREY.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'ORANGE'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "ORANGE.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'qual_DOWN'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "qual_DOWN.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'qual_GREEN'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "qual_GREEN.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'qual_RED'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "qual_RED.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'qual_STEADY'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "qual_STEADY.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'qual_UP'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "qual_UP.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'RED'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "RED.png", 
                                                                          width = .25, 
                                                                          height = .25)))
      row_name <- as.formula(paste0("~ ", col_name[i], " == 'UNDEFINED'"))
      flextable::display(x = FT, i = row_name, col_key = col_name[i],
                         pattern = "{{add_icon}}",
                         formatters = list(add_icon ~ flextable::as_image(1L,
                                                                          src = "UNDEFINED.png", 
                                                                          width = .25, 
                                                                          height = .25)))
    }
    
    FT <- FT %>% 
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::autofit()
    
    doc <- officer::read_docx() %>% 
      officer::body_add_par(value = '\r\n', style = "Normal") %>% 
      flextable::body_add_flextable(FT, align = "left", pos = "before") %>%
      officer::body_end_section(landscape = TRUE) %>% 
      print(target = sprintf("advice-overview-2017_%s_%s.docx", fold, iter)) %>% 
      invisible()
    
    iter_val <- iter*43 + 1
  } # close iter loop
} # closes foldtype

write.csv(stock_summary_table[, c("stock_code", "advice_url")], 
          file = "advice_overview-2017_urls.csv",
          row.names = FALSE)