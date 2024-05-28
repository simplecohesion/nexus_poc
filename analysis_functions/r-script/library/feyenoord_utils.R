####################
### Loading packages
library(optparse)
# library(StatsBombR)
# library(httr)
# library(later)
# library(devtools)
# library(igraph)
# library(magrittr)
# library(dplyr)
# library(ggplot2)
# library(lattice)
# library(ggpubr)
# library(cowplot)
# library(Hmisc)
# library(plyr)
# library(png)
# library(ggpmisc)
# library(XML)
# library(xml2)
# library(tidyverse)
# library(lubridate)
# library(clue)
# library(stringr)
library(readxl)
# library(ggrepel)
# library(ggtext)
# library(factoextra)
# library(REdaS)
# library(data.table)
# library(formattable)
# library(grid)
# library(gridExtra)
# library(patchwork)
# library(gghighlight)
# library(jsonlite)

# Load the specific functions needed to run the final code
calc_overperformance <- function(benchmark, actual, playing_minutes) {
  if (playing_minutes >= 90) {
    ratio_minutes <- 1
  } else if (playing_minutes < 90) {
    ratio_minutes <- playing_minutes / 90
  } else {
    cat("Wrong Playing Minutes Input")
  }

  score <- -1 * ((benchmark - (actual * ratio_minutes)) / benchmark)
  score <- round(score, digits = 2)
  score
}
#
read_excel_sheet <- function(sheet_name, path, skip = 0) {
  x <- read_excel(path = path, sheet = sheet_name, skip = skip)
}
#
calc_z_scores <- function(value, mean, sd, playing_minutes) {
  if (playing_minutes >= 90) {
    ratio_minutes <- 1
  } else if (playing_minutes < 90) {
    ratio_minutes <- playing_minutes / 90
  } else {
    cat("Wrong Playing Minutes Input")
  }
  z_score <- ((value - (mean * ratio_minutes)) / sd)
  z_score <- round(z_score, digits = 2)
  z_score
}
#
create_empty_table <- function(num_rows, num_cols) {
  frame <- data.frame(matrix(NA, nrow = num_rows, ncol = num_cols))
  return(frame)
}
#
loadRData <- function(fileName) {
  # loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
#
