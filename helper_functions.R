library(icesSharePoint)


tt <- getListStocks(year = year) %>%
  filter(Status == "Published") %>%
  mutate(StockCode = tolower(FishStockName)) %>% 
  left_join(slFull, by = c("FishStockName" = "StockCode")) %>% 
  mutate(site = gsub("(.*?)(\\..*)", "\\1", SectionNumber))

tt$site <- case_when(tt$site == 2 ~ "Iceland",
                     tt$site == 3 ~ "BarentsSea",
                     tt$site == 4 ~ "Faroes",
                     tt$site == 5 ~ "CelticSea",
                     tt$site == 6 ~ "NorthSea",
                     tt$site == 7 ~ "BayOfBiscay",
                     tt$site == 8 ~ "BalticSea",
                     tt$site == 9 ~ "Widely",
                     tt$site == 10 ~ "Salmon",
                     is.na(tt$site) ~ "OTHER")

find_file <- function(StockCode, book) {
  files <- spfiles(site = sprintf("/Advice/Advice2017/%s",
                                  book),
                   dir = "Released_Advice") %>% 
    grep(pattern = StockCode, x = ., value = TRUE)
  
  if(length(files) == 1){
    files <- paste0("Released_Advice/", files)
    return(files)
  }
  if(length(files) == 0L) {
    files <- NA
    cat("There are no files with this stock code... double check.")
    return(files)
  } 
  if(length(files) > 1){
    stop("There are multiple files with this stock code... double check.")
  }
}

tt$files <- unlist(map2(.x = tt$StockCode, .y = tt$site, function(x, y) find_file(StockCode = x, book = y)))


# spfiles(dir = "Released_Advice", site = "/Advice/Advice2017/NorthSea")

td <- tempdir()
map2(.x = tt$files, .y = tt$site, function(x, y) spgetfile(x, site = sprintf("/Advice/Advice2017/%s", y), dir = td))


doc <- read_docx(file.path(paste0(td, "/", tt$StockCode[1], ".docx")))

# tableCount <- unlist(lapply(fileName, function(x) docx_tbl_count(doc)))
columnNames <- lapply(seq(1: docx_tbl_count(doc)),
                      function(x) colnames(docx_extract_tbl(doc, tbl_number = x, header = TRUE)))

tbl_number <- grep("ices advice",  tolower(columnNames))

#####################
# Extract the table #
#####################
tableDat <- lapply(tbl_number, function(x) docx_extract_tbl(doc, 
                                                            tbl_number = x,
                                                            header = TRUE))

tableDat[[1]] %>% 
  tbl_df %>%
  select(dplyr::contains("Catch corresponding")) %>% 
  do(tail(., n = 1)) %>% 
  gsub(pattern = "[^[:digit:]]", replacement =  "", .) 

names(tableDat) <- paste0("T", tbl_number)








stock.code = "san.sa.1r"
library(docxtractr)

fileName <- fileList$file.path[fileList$StockCode == stock.code]
doc <- read_docx(fileName)

DT::datatable(docx_extract_tbl(doc, tbl_number = 7, header = TRUE))

