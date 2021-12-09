library(lmtest)
require(readxl)
library(orcutt)
library(sandwich)
# library(AER)


data <- read_xlsx("./dec/04.12/hw/Нелинейная модель/data.xlsx")


data_learning <- data[1:13, ]