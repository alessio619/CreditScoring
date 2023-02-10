
# A. Setup ============================================================

### install.packages(c('data.table', 'googlesheets4', 'XML'))


### 1. Package loading --------------------------------------

library(data.table)
library(ggplot2)
library(mlr3)

### 2. Data loading -----------------------------------------

dt_tr = fread(file.path('data', 'transazioni_test.csv'))

dt_tr[, amount := substr()]

unique_voices = unique(dt_tr$categoria)

dt_tr_byday = dt_tr[, .(balance = sum(amount))]

ggplot()