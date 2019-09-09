#!/usr/bin/env Rscript
library(testthat)
source("tests/testthat/helper-server.R")
source("tests/testthat/helper-queue.R")
options(error = traceback)
server <- hintr_server()
httr::GET(server$url)
server$process$kill()
