library(lmtest)
require(readxl)
library(orcutt)
library(sandwich)

# data <- read.table('./nov/20.11/tema7.csv', dec=',', header=TRUE)
data <- read_xlsx("./nov/20.11/tema7.xlsx")