# financialdataInR
Financial data analyses using R 

This document presents several fundamental indicators for analyzing financial data in the U.S. stock market. The present material is licensed under a [Creative Commons Attribution-ShareAlike License 3.0](https://creativecommons.org/licenses/by-sa/3.0/). Some of the material is inspired by [Lei&LoneCapital](https://lonecapital.com/) and the developers of [breadth.app](http://breadth.app). This work is not possible without Lei's inspiration and support. 

### Environment

* R version 4.0.3
* RStudio
* Several key R packages: 
  * `rmarkdown`
  * `dplyr`
  * `quantmod`
  * `kableExtra`

### 1. Financial Data Prep

Run the `datascraping_v0.0.1.R` script file. 
The R script file will download, clean, process, and save the financial data as of `Sys.Date()` in the .csv format under the `data/processed` and `data/raw` folder.

### 2. Financial Data Visualization

Step 2: run the `MarketDashboard.Rmd` file to visulzation the tables. 
The rmd file will load the data files processed on `Sys.Date()` under the `data/processed` folder for visualization. 

Note: 
1. `Sys.Date()` is used when rmarkdown file loads the data for visualization.


### Demo 

The HTML output file can be found at [https://rpubs.com/tianyuan/marketbreadth](https://rpubs.com/tianyuan/marketbreadth)

