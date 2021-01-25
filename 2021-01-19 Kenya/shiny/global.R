library(tidyverse)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggrepel)
library("rKenyaCensus")
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(ggdark)
library(ggfortify)
library(sf)

livestock <- rKenyaCensus::V4_T2.23 %>% data.table::as.data.table()

# livestock[, County := as.factor(County)]
# livestock[, SubCounty := as.factor(SubCounty)]
# livestock[, AdminArea := as.factor(AdminArea)]

ls <- livestock[AdminArea == "County"] %>% tidyr::drop_na()

ls[, -1:-3] %>% prcomp(scale = TRUE, center = TRUE) -> pcafit

shp <- st_as_sf(rKenyaCensus::KenyaCounties_SHP)

dat <- p1$data

dat$shade <- ifelse(dat$PC1 > 0, "Farming", "Desert")
st_as_sf(left_join(dat, shp, by = "County")) -> shp


