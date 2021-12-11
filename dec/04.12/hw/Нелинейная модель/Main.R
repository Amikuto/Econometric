library(lmtest)
require(readxl)
library(orcutt)
library(sandwich)
# library(AER)


data <- read_xlsx("./dec/04.12/hw/Нелинейная модель/data.xlsx")


data_needed <- data[1:13, ]

Q <- data_needed$Q
L <- data_needed$L
K <- data_needed$K

mlm <- lm('Q ~ L + K', data=data_needed)
smlm <- summary(mlm); smlm

elastichnost <- c(
  smlm$coefficients[2] * mean(L) / mean(Q),
  smlm$coefficients[3] * mean(K) / mean(Q)
); elastichnost

b1 <- elastichnost[1]; b1
b2 <- elastichnost[2]; b2
# a <-
