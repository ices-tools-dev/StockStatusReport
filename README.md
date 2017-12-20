# StockStatusReport
Tool to aggregate and present ICES stock status


## Background
The EU AA requres that ICES provide an up to date report of ICES advice and stock status for all published stocks. 

## Workflow
1. Aggregate data
* Query Stock Assessment Graphs web services via [icesSAG](https://github.com/ices-tools-prod/icesSAG) for stock status table
* Query Stock Databse web service via [icesSD](https://github.com/ices-tools-prod/icesSLD) for data category
* Query released advice via [icesSharePoint]() for advice value
2. Link together into a data table via [DT](https://rstudio.github.io/DT/)
3. Render as an html document via [knitr](https://cran.r-project.org/web/packages/knitr/index.html).

## Files
* To render the .html output, run ICES_stock_summary.rmd
    - ICES_stock_status.R provides a framework to run it as a chron job on a server
* To create .docx files that can be pasted into a .xlsx, run ICES_stock_summary.R 
    - Note: Right now there isn't a good way to save images to ordered cells in an .xlsx using R. The workaround, here, is pretty tedious.
* Utilities.R has the meat and potatoes behind the scripts, above. 


## Outstanding issues
* Make this a cron-job so that every midnight a check is run to see if new advice has been published
* Does it make sense to automatically publish to the web?
