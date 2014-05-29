library(testthat)
library(tm)
library(tm.plugin.webmining)

setwd("/home/mario/workspace_private/_pkg_tm-plugin-webmining")
load("data/nytimes_appid.rda")

test_dir("inst/tests")