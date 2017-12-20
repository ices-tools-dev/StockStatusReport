output_type = "docx"
active_year = 2017
get_stock_summary <- function(active_year, output_type) {
  
  stock_list_raw <- jsonlite::fromJSON("http://sd.ices.dk/services/odata3/StockListDWs3",
                                       simplifyDataFrame = TRUE)$value
  stock_list <- stock_list_raw %>% 
    filter(ActiveYear == active_year) %>%
    distinct(.keep_all = TRUE) %>%
    mutate(DataCategory = floor(as.numeric(DataCategory)), 
           StockKeyDescription = gsub("Skyliorhinus stellaris",
                                      "Scyliorhinus stellaris",
                                      StockKeyDescription),
           stock_code = dplyr::case_when(YearOfLastAssessment <= 2016 ~ PreviousStockKeyLabel,
                                         YearOfLastAssessment >= 2017 ~ StockKeyLabel,
                                         TRUE ~ NA_character_),
           stock_code = dplyr::case_when(stock_code == "cod.27.25-32" ~ "cod.27.24-32",
                                         stock_code == "pan-flad" ~ "pand-flad",
                                         stock_code == "ple.27.7fg" ~ " ple.27.7f-g",
                                         !stock_code %in% c("cod.27.25-32", "pan-flad", "ple.27.7fg") ~ stock_code,
                                         TRUE ~ NA_character_)) %>% 
    filter(!is.na(stock_code)) %>% 
    select(stock_code,
           StockKeyDescription,
           SpeciesScientificName,
           YearOfLastAssessment,
           DataCategory,
           ActiveYear,
           AdviceCategory,
           EcoRegion,
           StockKeyLabel)
  
  if(output_type == "html"){
    
    # Format so the species names will be italicized
    stock_list_frmt <- bind_rows(
      # Normal binomial names
      stock_list %>%
        filter(grepl("[[:space:]]", SpeciesScientificName)) %>%
        mutate(StockKeyDescription = stringr::str_replace_all(string = StockKeyDescription,
                                                              pattern = SpeciesScientificName,
                                                              replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
      # Groups of species (.spp)
      stock_list %>%
        filter(grepl(" spp.*$", StockKeyDescription)) %>%
        mutate(StockKeyDescription = stringr::str_replace_all(string = StockKeyDescription,
                                                              pattern = SpeciesScientificName,
                                                              replacement = paste0("<em>", SpeciesScientificName, "</em>"))),
      # A bit different notation (embedded in 2 sets of parentheses)
      stock_list %>%
        filter(stock_code %in% c("raj-mar", "raj.27.1012")) %>%
        mutate(StockKeyDescription = stringr::str_replace_all(string = StockKeyDescription,
                                                              pattern = "Raja clavata",
                                                              replacement = "<em>Raja clavata</em>")),
      
      # The "others" with no species name
      stock_list %>%
        filter(!grepl(" spp.*$", StockKeyDescription)) %>%
        filter(!stock_code %in% c("raj-mar", "raj.27.1012")) %>%
        filter(!grepl("[[:space:]]", SpeciesScientificName))
    ) 
  }
  if(output_type == "docx"){
    stock_list_frmt <- stock_list
  }
  stock_list_frmt <- stock_list_frmt %>% 
    mutate(docx_url = glue::glue("http://www.ices.dk/sites/pub/Publication%20Reports/Advice/{YearOfLastAssessment}/{YearOfLastAssessment}/{stock_code}.pdf"),
           html_url = glue::glue('<a href={docx_url} target="_blank" title="Click here for the most recent advice">{StockKeyLabel}</a>')) %>% 
    select(-StockKeyLabel)
  
  year_range <- min(stock_list_frmt$YearOfLastAssessment):max(stock_list_frmt$YearOfLastAssessment)
  sag_keys_raw <- do.call("rbind", lapply(year_range,
                                          function(x) icesSAG::findAssessmentKey(stock = NULL,
                                                                                 year = x,
                                                                                 full = TRUE)[, c("AssessmentYear", "AssessmentKey", "StockKeyLabel")]))
  
  get_stock_status <- function(assessmentKey) {
    dat <- icesSAG::getStockStatusValues(assessmentKey)[[1]]
    
    if(is.null(dat)) stop(paste0("NULL value returned for assessmentKey = ", assessmentKey))
    dat
  }
  
  sag_stock_status_raw <- sag_keys_raw %>%
    mutate(stock_status = purrr::map(.x = AssessmentKey, 
                                     purrr::possibly(get_stock_status, 
                                                     otherwise = NA_real_))) %>% 
    select(-AssessmentKey) %>% 
    distinct(.keep_all = TRUE, StockKeyLabel) %>% 
    filter(!is.na(stock_status)) %>%
    tidyr::unnest(stock_status)
  
  status_url_raw <- '<img src= "http://sg.ices.dk/download/icons/STATUS.png" title = "TITLE" height ="35%" width="35%"></img>'
  
  # Clean up the SAG output
  sag_stock_status <- sag_stock_status_raw %>%
    mutate(fishingPressure = case_when(fishingPressure == "-" &
                                         type == "Fishing pressure" ~ "FQual",
                                       TRUE ~ fishingPressure),
           stockSize = case_when(stockSize == "-" &
                                   type == "Stock Size" ~ "SSBQual",
                                 TRUE ~ stockSize),
           stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
           variable = case_when(type == "Fishing pressure" ~ fishingPressure,
                                type == "Stock Size" ~ stockSize,
                                TRUE ~ type),
           variable = case_when(lineDescription == "Management plan" &
                                  type == "Fishing pressure" ~ "FMGT",
                                lineDescription == "Management plan" &
                                  type == "Stock Size" ~ "SSBMGT",
                                TRUE ~ variable),
           variable = case_when(
             grepl("Fpa|HRpa", variable) ~ "FPA",
             grepl("Bpa", variable) ~ "BPA",
             grepl("^Qual*", variable) ~ "SSBQual",
             grepl("-", variable) ~ "FQual",
             grepl("^BMGT", variable) ~ "SSBMGT",
             grepl("MSYBtrigger|Btrigger|MSYBtescapement|MSYBe", variable) ~ "BMSY",
             grepl("FMSY|HRMSY", variable) ~ "FMSY",
             TRUE ~ variable
           )) %>%
    filter(variable != "-") %>%
    arrange(variable, year) %>%
    mutate(name = paste0(variable, year),
           name = factor(name, levels = unique(name))) %>% 
    select(AssessmentYear,
           StockKeyLabel,
           name, 
           status) %>% 
    tidyr::spread(name, status) %>%  
    mutate_all(funs(replace(., is.na(.), 6))) %>%
    tidyr::gather(name, status, -AssessmentYear, -StockKeyLabel) %>% 
    mutate(status_name = case_when(status == 0 ~ "UNDEFINED",
                                   status == 1 ~ "GREEN",
                                   status == 2 ~ "qual_GREEN", #qualitative green
                                   status == 3 ~ "ORANGE",
                                   status == 4 ~ "RED",
                                   status == 5 ~ "qual_RED", #qualitative red
                                   status == 6 ~ "GREY",
                                   status == 7 ~ "qual_UP",
                                   status == 8 ~ "qual_STEADY",
                                   status == 9 ~ "qual_DOWN",
                                   TRUE ~ "OTHER"),
           status_url = stringr::str_replace_all(string = status_url_raw, 
                                                 pattern = "TITLE", 
                                                 replacement = as.character(status_name)),
           status_url = stringr::str_replace_all(string = status_url, 
                                             pattern = "STATUS", 
                                             replacement = as.character(status)),
           AssessmentYear = as.numeric(AssessmentYear))

  if(output_type == "html"){
    sag_stock_status <- sag_stock_status  %>%
      select(AssessmentYear,
             StockKeyLabel,
             name, 
             status = status_url) %>% 
      tidyr::spread(name, status)
  }
  if(output_type == "docx"){
    sag_stock_status <- sag_stock_status  %>%
      select(AssessmentYear,
             StockKeyLabel,
             name, 
             status = status_name) %>% 
      tidyr::spread(name, status)
  }

  # ID the list of stocks
  stock_summary_table <- stock_list_frmt %>%
    select(-EcoRegion) %>%
    distinct(.keep_all = TRUE) %>%
    left_join(sag_stock_status, by = c("stock_code" = "StockKeyLabel",
                                       "YearOfLastAssessment" = "AssessmentYear")) %>%
    select(-ActiveYear)
  
  if(output_type == "html"){
    stock_summary_table <- stock_summary_table %>% 
      mutate_all(funs(replace(., is.na(.), '<img src= "http://sg.ices.dk/download/icons/6.png" title = "GREY" height ="35%" width="35%"></img>'))) %>% 
    mutate_all(funs(factor(.))) %>% 
      mutate(advice_url = html_url) %>% 
      select(-docx_url,
             -html_url)
  }
  if(output_type == "docx"){
    stock_summary_table <- stock_summary_table %>% 
      mutate_all(funs(replace(., is.na(.), "GREY"))) %>% 
      mutate_all(funs(factor(.))) %>% 
      mutate(advice_url = docx_url) %>% 
      select(-docx_url,
             -html_url)
  }
  
  return(stock_summary_table)
}



th_cols <- function(regex) {
  cols <- colnames(stock_summary_table)
  # cols <- colnames(sag_stock_status)
  my_cols <- grep(regex, cols, value = TRUE)
  
  col_years <- stringr::str_sub(my_cols, -4)
  col_vars <- stringr::str_sub(my_cols, end = nchar(my_cols) - 4)
  
  col_num <- do.call("c", lapply(unique(col_vars), function(x) length(grep(x, cols))))
  col_y <- data.frame(colspan = col_num,
                      name = unique(col_vars))
  
  
  col_structure <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, "Stock code"),
        th(rowspan = 2, "Stock name"),
        th(rowspan = 2, "Data category"),
        th(rowspan = 2, "Advice category"),
        th(colspan = col_y$colspan[1], col_y$name[1]),
        th(colspan = col_y$colspan[2], col_y$name[2])
      ),
      tr(
        lapply(col_years, th)
      )
    )
  ))
  
  return(col_structure)
}



