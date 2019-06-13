#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
library(magrittr)          # enables piping : %>% 
library(dplyr)             # data wrangling
library(knitr)             # tables
library(ggplot2)           # graphs
requireNamespace("readr")  # for data input
requireNamespace("tidyr")  # for data manipulation

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines external functions needed in this report. 
# Ideally, no real operations are performed in these scripts.
base::source("./scripts/common-functions.R") # used in multiple reports
base::source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 

# ---- declare-globals -------------------------
# location of the data sources
path_input  <- "./data-public/raw/scenario-3/PHAC_Infobase_CCDSS_-8586421808967913043.csv" # mental illness
# path_input   <- "./data-public/raw/scenario-3/PHAC_Infobase_CCDSS_-8586421810172823624.csv" # anxiety & mood
output_format = "pandoc"

# ---- rmd-specific ----------------------------
# for demontrating custom function principle:
print_distinct <- function(
  d
  ,group_by_variables
){
  # values needed for testing and development inside the function:
  # d <- ds0
  # group_by_variables <- "area"
  # group_by_variables <- c("area","age_group")
  
  d_out <- d %>%
    dplyr::group_by(.dots = c(group_by_variables) ) %>%
    dplyr::count()

  return(d_out)
}
# How to use
ds1 %>% print_distinct(group_by_variables = "area")
# ds1 %>% print_distinct(group_by_variables = "condition")
# ds1 %>% print_distinct(group_by_variables = "age_group")
# ds1 %>% print_distinct(group_by_variables = c("area","age_group") )

# to print a graph to disk
quick_save <- function(g,name){
  ggplot2::ggsave(
    filename  = paste0(name,".png"), 
    plot      = g,
    device    = png,
    path      = where_to_store_graphs, 
    width     = 960,
    height    = 400,
    res       = 200,
    limitsize = FALSE
  )
}
# How to use
# g1 %>% quick_save(names = "g1")

# ---- load-data -------------------------------
# see ./data-unshared/contents.md for origin of the data
ds0 <- path_input %>% readr::read_csv(skip = 3) %>% tibble::as_tibble()

# see data dictionary at http://infobase.phac-aspc.gc.ca/cubes/ccdss-eng.html
ds0 %>% dplyr::glimpse(80)

# ---- tweak-data ------------------------------
# to trace the tweaks
ds1 <- ds0 # start with the identical copy
# correct variable names
names(ds1) <- gsub(" ","_", names(ds1)) # to make into a single word (all word chars)
names(ds1) <- gsub("%","", names(ds1))  # to remove special characters
names(ds1) <- tolower(names(ds1))       # to standardize spellining

ds1 <- ds1 %>% dplyr::select(-x12) # to remove an empty column

ds1 <- ds1 %>% 
  dplyr::mutate(
    age_group = gsub("'","",age_group) # to remove extra set of quotes
  ) %>% 
  dplyr::filter(!is.na(condition)) # to remove notes at the end of the spreadsheet

ds1 %>% dplyr::glimpse()

# ---- inspect-data-1 ----------------------------


# ---- graph-1 --------------------------
# let us sketch the most basic graph in 3 internal dimensions
# notice that we isolate a single value on all the rest dimensions
g1 <- ds1 %>% 
  dplyr::filter(area       ==  "British Columbia" ) %>%
  dplyr::filter(age_group  ==  "20-34" ) %>%
  dplyr::filter(sex       %in% c("Males","Females") ) %>%
  ggplot(aes(
    x      = year
    ,y     = rate
    ,color = sex
  ))+
  geom_point()+
  geom_line( aes(group = sex) )+
  theme_minimal()+
  labs( title = "Crude prevalence of MH service utilization in BC among 20-34 year olds")
g1

g1 %>% quick_save(name  = "g1") # will produce error, debug

# ---- graph-1a --------------------------
# modify graph g1 to improve the appearance 


