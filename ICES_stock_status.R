rm(list = ls())
input_file <- "~/rCode/stock_status/ICES_stock_status_report.Rmd"
output_dir <- "D:/IIS/web/"
output_file <- "stockstatus.html"
file_path <- paste0(output_dir, output_file)

if(!file.exists(input_file)) stop("input_file: '", input_file, "' is not found. Please check spelling/file location and try again.\n")

if(file.exists(file_path)) {
  cat(paste0(file_path, " was last modified: ", file.info(file_path)$mtime, ".\n"))
}

if(!rmarkdown::pandoc_available()) {
  cat("pandoc not found, setting path as 'C:/Program Files/RStudio/bin/pandoc'")
  Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
}


rmarkdown::render(input = input_file,
                  output_file = file_path,
                  envir = new.env())

## TODO:

## Check SAG connection

## If cache is not present, download all 2017 stock status

## If cache is present, download all new stocks

## If no new stocks stop()