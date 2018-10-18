# knitr::stitch_rmd(script="./analysis/report-1/report-1.R", output="./analysis/report-1/report-1.md")
# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./analysis/scenario-1/scenario-1.R",
#   output = "./analysis/scenario-1/scenario-1.md"
# )
# this command is typically executed by the ./manipulation/governor.R


rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./SomethingSomething.R")
base::source("./scripts/graphing/graph-presets.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
library(dplyr)
# requireNamespace("dplyr")
# requireNamespace("tidyr") #For converting wide to long
# requireNamespace("RColorBrewer")
# requireNamespace("scales") #For formating values in graphs
# requireNamespace("mgcv) #For the Generalized Additive Model that smooths the longitudinal graphs.
# requireNamespace("TabularManifest") # devtools::install_github("Melinae/TabularManifest")

# ---- declare-globals ---------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values

# capture the link to the source file as a string object for future use
path_input <- "./data-public/raw/scenario-2/example-data.rds"

# ---- load-data ---------------------------------------------------------------
# load an example data data from `ihacru/suppress-for-release` project
ls0         <- readRDS(path_input)

ds_raw        <- ls0$raw_wide
ds_redacted   <- ls0$redacted_long
bc_health_map <- ls0$bc_health_map

# ---- inspect-data ------------------------------------------------------------
# base::source("./analysis/scenario-2/suppression-functions.R")

# ---- tweak-data --------------------------------------------------------------


# ---- exercise-1 --------------------------------------------------------------

# reconstruct the following objects, incrementally increasing their complexity
ds_redacted %>% glimpse()

g1 <- ds_redacted %>% 
  ggplot2::ggplot(aes(x = sex, y = label_hsda))+
  # geom_tile(aes( fill = censor1_small_cell))+
  # geom_tile(aes( fill = censor2_recalc_triplet))+
  geom_tile(aes( fill = censor3_single_suppression))+
  geom_text(aes(label = value))+
  facet_grid(. ~ agg_level)
g1

ggplot2::ggsave(
  filename = "./analysis/scenario-2/prints/graph-1.jpeg",
  plot = g1,
  device = "jpeg",
  units = "mm",
  width = 216,
  height = 140
)


# ---- define-graphing-settings ---------------------------------------------------------------

# ---- define-graphing-functions ---------------------------------------------------------------



base::source('./analysis/scenario-2/graph-presets.R')
# ----- print-graphs ---------------------

# show created file
list.files("./analysis/scenario-1/prints/", full.names = TRUE)


# Size           Width x Height (mm) Width x Height (in)  Aspect Ratio
# Half Letter      140 x 216           5.5 x  8.5          1: 1.55
# Letter           216 x 279           8.5 x 11.0          1: 1.29
# Legal            216 x 356           8.5 x 14.0          1: 1.65
# Junior Legal     127 x 203           5.0 x  8.0          1: 1.60
# Ledger/Tabloid   279 x 432          11.0 x 17.0          1: 1.55


# ---- publish ---------------------------------------
# This chunk will publish the summative report
path_publish_report_1 <- "./analysis/scenario-2/scenario-2.Rmd"
# path_publish_report_1 <- "./reports/*/report_2.Rmd"
allReports <- c(
  path_publish_report_1
) # add more reports, in necessary
pathFilesToBuild <- c(allReports) # line up report(s) to render
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build (render) the reports
for( pathFile in pathFilesToBuild ) {
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document"   
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" 
                    ),
                    clean=TRUE)
}


