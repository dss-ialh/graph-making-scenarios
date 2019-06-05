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
requireNamespace("testit") # for asserting conditions meet expected patterns
requireNamespace("car")    # for its `recode()` function

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

ds1 %>% dplyr::glimpse(80)

# ---- inspect-data-1 ----------------------------
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

# ---- declare-components ----------------------
# to help with looping and wrangling
categorical_variables <- c("area","condition", "age_group", "sex","year")
continuous_variables  <- c(
  "rate", "rate_cv", "rate_95_ci_lower","rate_95_ci_upper", "number","population"
)
# to help with targeted application 
varnames_rate     <- c("rate", "rate_cv","rate_95_ci_lower","rate_95_ci_upper")
varnames_measure  <- c(varnames_rate,"number","population" )
varnames_time     <- c("year")
varnames_design   <- c("area","age_group","sex", "condition")

# ---- inspect-data-2 --------------------------
# to explore the scles of the CATEGORICAL variables
for(i in categorical_variables){
  ds1 %>% 
    print_distinct( group_by_variables = i ) %>%
    neat() %>% # to apply custom style for html canvas
    print()    # because inside the loop
}

# ---- inspect-data-3 --------------------------
# to explore the scales of CONTINUOUS variables
ds1 %>% 
  dplyr::select_(.dots = continuous_variables) %>% 
  explore::describe() %>% neat()


# ---- phase-1-graph -----------------------
# now let us find ways to look at/in/with data
# the above analysis helps us to conceptualize available variables as:

#  I - DATA space

## MEASURE - rate       # crude rate (not age-adjusted)   # includes (cv, ci_upper, ci_lower) 
## MEASURE - number     # count of cases of the condition
## MEASURE - population # total alive
## TIME    - year       # fiscal
## DESIGN  - area       ( 12 provinces + 1 total )
## DESIGN  - age_group  ( 6 gropus + 1 total )
## DESIGN  - sex        ( 2 gropus + 1 total )
## DESIGN  - condition  ( 1 )

#  II - VISUALIZATION space 

## INNER - horizontal - TIME    - (year) 
## INNER - vertical   - MEASURE - (rate)  
## INNER - color      - DESIGN  - (sex)
## OUTER - horizontal - DESIGN  - (area)
## OUTER - vertial    - DESIGN  - (age_group)


# ---- phase-1-graph-1 --------------------------


# ---- phase-2-make_plot --------------------------
# suppose, we have settled on the graphical form `g2d` (immediately above)
# now let us re-express this plot as a custom function

# ---- phase-2-make_plot-2 --------------------------
# We need our function to offer us a convenient way to:
# 1. Control the order of the columns (and which are displayed)
# 2. Control the order of the rows    (and which are displayed)
# 3. Control the order and aesthetics of the color dimention


# ---- phase-3-prep_data-1 ------------------------------

#
# ---- phase-4-print_plot ---------------------------------


# ---- dimensions ----------------------
# canvas size guide ( portrait orientation )

# Size           Width x Height (mm) Width x Height (in)  Aspect Ratio
# Half Letter      140 x 216           5.5 x  8.5          1: 1.55
# Letter           216 x 279           8.5 x 11.0          1: 1.29
# Legal            216 x 356           8.5 x 14.0          1: 1.65
# Junior Legal     127 x 203           5.0 x  8.0          1: 1.60
# Ledger/Tabloid   279 x 432          11.0 x 17.0          1: 1.55


# ---- phase-5-serialize ---------------------------------
# it often makes sense to genrate a series of plot to be explored manually

# ---- phase-6-place_plot ---------------------------------

# ---- publish ---------------------------------------
# This chunk will publish the summative report
path_publish_report_1 <- "./analysis/scenario-3/scenario-3-ccdss.Rmd"
# path_publish_report_2 <- "./reports/*/report_2.Rmd"
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




# ---- save-to-disk --------------------- 
# save the created and groomed dataset for further use by subsequent reports
readr::write_csv(ds, path_save)
ds %>% saveRDS( gsub(".csv$",".rds",path_save) )



# phase 0 - build the plot
# phase 1 - build the function
# phase 2 - isolate prep step
# phase 3 - isolate print step
# phase 4 - Serialize
# phase 6 - place into the canvas
  


