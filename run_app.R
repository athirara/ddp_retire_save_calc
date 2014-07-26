library(shiny)

setwd("~/Dropbox/data_science_coursera/ddp/project/retire_saving_calc")
setwd("C:/Users/hiuyan/Dropbox/data_science_coursera/ddp/project/retire_saving_calc")

runApp(display.mode='showcase')

# deploy app on RStudio ShinyApps.io
# devtools::install_github('rstudio/shinyapps')
library(shinyapps)
deployApp()
