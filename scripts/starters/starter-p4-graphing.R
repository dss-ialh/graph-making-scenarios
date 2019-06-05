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
path_input  <- "2*.csv" # source 1
path_input  <- "1*.csv" # source 2

# ---- rmd-specific ----------------------------

# ---- load-data -------------------------------
# see ./data-unshared/contents.md for origin of the data
ds0 <- path_input %>% readr::read_csv() #%>% tibble::as_tibble()

# see data dictionary at http://infobase.phac-aspc.gc.ca/cubes/ccdss-eng.html
ds0 %>% dplyr::glimpse(80)

# ---- tweak-data ------------------------------


# ---- inspect-data-1 ----------------------------


# ---- declare-components ----------------------


# ---- inspect-data-2 --------------------------

# ---- inspect-data-3 --------------------------


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

## INTERNAL - horizontal - TIME    - (year) 
## INTERNAL - vertical   - MEASURE - (rate)  
## INTERNAL - color      - DESIGN  - (sex)
## EXTERNAL - horizontal - DESIGN  - (area)
## EXTERNAL - vertial    - DESIGN  - (age_group)


# ---- phase-1-graph-1 --------------------------
# let us sketch the most basic graph in 3 internal dimensions
# notice that we isolate a single value on all the rest dimensions

# to add a few bells and whistles that aid quick evaluation:

# ---- phase-1-graph-2 --------------------------
# let us introduce external dimensions through faceting
# we will keep one of the external dimension constant and facet_wrap on the other


# ---- phase-1-graph-3 --------------------------
# now let facet_grid on both demension

# ---- phase-2-make_plot --------------------------
# suppose, we have settled on the graphical form `...` (immediately above), repeat below

# ---- phase-2-make_plot-1 --------------------------
# now let us re-express this plot as a custom function
make_plot_1_basic <- function(
  d
  ,measure = "rate" # continuous
){
  g_out <- d %>% 
    ggplot(aes_string(
       x      = "year" # continuous
       ,y     = measure
       ,color = "sex" # binary/categorical
    ))+
    geom_point()+
    geom_line( aes_string(group = "sex") )+
    facet_grid(area ~ age_group)+ # categorical ~ categorical
    theme_minimal()+
    labs( title = "")
  return(g_out)  
}
# how to use:
ds1 %>% 
  dplyr::filter(sex       %in% c("Males","Females") ) %>% # Excludes "Total"
  # notice that we keep operations on the data outside of the function definition
  make_plot_1_basic(measure = "rate")

# ---- phase-2-make_plot-2 --------------------------
# We need our function to offer us a convenient way to:
# 1. Control the order of the columns (and which are displayed)
# 2. Control the order of the rows    (and which are displayed)
# 3. Control the order and aesthetics of the color dimention

# if we were to pack everything into a single function we would get something like:
make_plot_1_packed <- function(
  d
  ,measure
){
  d1 <- d
  # d1 <- ds1 # for testing and development
  # create support objects
  order_of_age_group <- d1 %>% 
    dplyr::arrange() %>% 
    dplyr::distinct(var_outer_1) %>% 
    as.list() %>% unlist() %>% as.character()

  order_of_areas <- d1 %>% 
    dplyr::arrange() %>% 
    dplyr::distinct(var_outer_1) %>% 
    as.list() %>% unlist() %>% as.character()
    
  # to customize the order of levels of the color dimension
  levels_sex <- c("Females", "Males","Both sexes")
  # 
  d1 <- d #%>% # add more here
  # to create custom pallets:
  
  # descriptive tag              # green     # red      # blue
  palette_sex_dark         <- c("#1b9e77", "#d95f02", "#7570b3") #duller than below
  # palette_sex_dark         <- c("#66c2a5", "#fc8d62", "#8da0cb") #brighter than above
  # taken from http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
  pallete_sex_light        <- adjustcolor(palette_sex_dark, alpha.f = .2)
  names(palette_sex_dark)  <- c("Both sexes", "Females", "Males")
  names(pallete_sex_light) <- names(pallete_sex_light)
  
  g_out <- d %>% 
    ggplot(aes_string(
      x      = "year" # continuous
      ,y     = measure
      ,color = "sex" # binary/categorical
    ))+
    geom_point()+
    geom_line( aes_string(group = "sex") )+
    facet_grid(area ~ age_group)+ # categorical ~ categorical
    theme_minimal()+
    labs( title = "")
  
  return(g_out)  
}
# how to use
ds1 %>% 
  # to limit the view while in development
  dplyr::filter(age_group %in% c("1-19", "20-34", "80+","1+")) %>%
  dplyr::filter(area %in% c("Canada", "Alberta", "British Columbia")) %>%
  dplyr::filter(sex %in% c("Males","Females")) %>%
  make_plot_1_packed(measure = "rate")

# as you notice, the function got bulkier due to operations needed
# to construct a reference vector for factor levels 
# these and other operations are typically best sourced out 
# to the `prep_data` function

# ---- phase-3-prep_data-1 ------------------------------

# let us construct a new `prep_data` function that would
# isolate the preparatory operations from the `make_plot` function
prep_data_plot_1 <- function(
  d_input
  ,set_area      #= c("Canada")
  ,set_age_group #= c("20-34")
  ,set_sex       #= c("Males","Females")
){
  d1 <- d_input  # for within-function use
  
  d2 <- d1 %>% 
    dplyr::filter(area      %in% set_area      ) %>%
    dplyr::filter(age_group %in% set_age_group ) %>%
    dplyr::filter(sex       %in% set_sex       ) %>% 
    dplyr::mutate(
      # to create a shorter label
      years_since_2000 = year - 2000
      # to enforce the chosen order of the levels:
      ,area      = factor(area,      levels = set_area)
      ,age_group = factor(age_group, levels = set_age_group)
      ,sex       = factor(sex,       levels = set_sex )
    )
  # to store objects for passing to the `make_plot` function
  l_support <- list()
  l_support[["data"]] <- d2
  l_support[["set"]] <- list() # in case the first element is single
  l_support[["set"]][["sex"]]       <- set_sex
  l_support[["set"]][["area"]]      <- set_area
  l_support[["set"]][["age_group"]] <- set_age_group
  lapply(l_support, class) # view contents
  # the make_plot funtion will rely on the structure and values in l_support

  return(l_support)
}
# how to use
l_support <- ds1 %>% 
  prep_data_plot_1(
    set_area      = c("Canada")
    # set_area      = c("Canada", "Alberta", "British Columbia") 
    # ,set_sex       = c("Females", "Males")
    ,set_sex       = c("Both sexes")
    # ,set_age_group = c("1-19", "20-34", "80+","1+")
    ,set_age_group = c("1+")
  )
l_support %>% print()

# now we can pass this curated object `l_support` to graphing function
# note that we need to adjust the function to accomodate a new input object
make_plot_1 <- function(
  l_support
  ,measure 
){
  d <- l_support$data
  # to customize the color 
  # descriptive tag              # green     # red      # blue
  palette_sex_dark         <- c("#1b9e77", "#d95f02", "#7570b3") #duller than below
  # palette_sex_dark         <- c("#66c2a5", "#fc8d62", "#8da0cb") #brighter than above
  # taken from http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
  pallete_sex_light        <- adjustcolor(palette_sex_dark, alpha.f = .2)
  names(palette_sex_dark)  <- c("Both sexes", "Females", "Males")
  names(pallete_sex_light) <- names(pallete_sex_light)
  
  g_out <- d %>% 
    ggplot(aes_string(
      x      = "years_since_2000"
      ,y     = measure
      ,color = "sex"
    ))+
    geom_point()+
    geom_line( aes_string(group = "sex") )+
    facet_grid(area ~ age_group)+
    scale_color_manual(values = palette_sex_dark)+
    # scale_color_manual(values = pallete_sex_light)+
    theme_minimal()+
    labs( title = "Crude prevalence of MH service utilization")
  l_support[["graph"]]   <- g_out
  l_support[["measure"]] <- measure
  return(l_support)  
}
# how to use
l_support <- ds1 %>% 
  prep_data_plot_1(
    # set_area      = c("Canada")
    set_area      = c("Canada", "Alberta", "British Columbia")
    ,set_sex       = c("Females", "Males")
    # ,set_sex       = c("Both sexes")
    # ,set_age_group = c("1-19", "20-34", "80+","1+")
    ,set_age_group = c("1+")
  ) %>% 
  make_plot_1(measure = "rate")
l_support$graph %>% print()

# ---- phase-4-print_plot ---------------------------------

print_plot_1 <- function(
  l_support
  ,path_output_folder
  ,prefex     = NA
  ,graph_name = "auto"
  ,...
){
  if( graph_name == "auto" ){
    graph_name <- paste0(
      # should be replaced with features appropriate for analysis
      l_support$measure
      ,"-("
      ,l_support$set$sex %>% paste0(collapse = "-")
      ,")-("
      ,l_support$set$area %>% paste0(collapse = "-")
      ,")-("
      ,l_support$set$age_group %>% paste0(collapse = "-")
      ,")"
      ,collapse = "-"
    )
  }else{
    graph_name <- paste0(l_support$measure,"-", graph_name)
  }
  # add a label to distinguish a particular graph (last element in the file name)
  if( !is.na(prefex) ){ # inserts a PREFEX before the graph name
    (path_save_plot <- paste0(path_output_folder, prefex,"-",graph_name) )
  }else{
    ( path_save_plot <- paste0(path_output_folder, graph_name) )
  }

  # if folder does not exist yet, create it
  if( !dir.exists(path_output_folder) ){
    dir.create(path_output_folder)
  }
  # print the graphical object using jpeg device
  path_printed_plot <- paste0(path_save_plot, ".jpg")
  jpeg(
    filename = path_printed_plot
    ,...
  )
  l_support$graph %>% print() # reach into the custom object we made for graphing
  dev.off() # close the device
  l_support[["path_plot"]] <- path_printed_plot
  return(l_support)
}
# how to use
l_support <- ds1 %>% 
  prep_data_plot_1(
    set_sex        = c("Females", "Males") 
    # set_sex        = c("Females", "Males", "Both sexes") 
    ,set_area      = c("Canada", "Alberta", "British Columbia") 
    ,set_age_group = c("1-19", "20-34", "80+","1+")
  ) %>% 
  make_plot_1(
    measure = "rate"
  ) %>% 
  print_plot_1(
    path_output_folder = "./analysis/scenario-3/prints/demo-1/"
    # ,prefex            = "attempt1"
    # ,graph_name        = "take1" # `auto` by default
# options added through `...` into the jpeg() function   
    ,width   = 1700
    ,height  = 500
    ,units   = "px"
    ,quality = 100
    ,res     = 200
  )
# ---- phase-4-print_plot-1 ---------------------------------
# notice that if I print the GRAPH by reaching into the `l_support` object
# if will be displayed according to the `fig.width`, `fig.height`, and `out.width`
# parameters specified in the chuck options (in the .Rmd file)
l_support$graph %>% print()

# ---- phase-4-print_plot-2 ---------------------------------
# if, however, we reach into the disk, we will recover the image generated
# with the dimensions and specs defined in the `print_plot` function
l_support$path_plot %>% jpeg::readJPEG() %>% grid::grid.raster()



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

# GRAPH SERIES 1
path_target <- "./analysis/scenario-3/prints/series_1/"
provinces_to_pair     <- c("British Columbia", "Alberta", "Quebec")
age_groups_to_display <- c("1-19", "20-34", "35-49", "50-64","65-79","80+","+1")
# for each selected province create a comparison with Canada
ls_plot_series <- list()
for(province_i in provinces_to_pair){
  ls_plot_series[[province_i]]  <- ds1 %>%
    prep_data_plot_1(
      set_sex        = c("Females", "Males")
      ,set_area      = c("Canada", province_i)
      ,set_age_group = age_groups_to_display
    ) %>%
    make_plot_1(
      measure = "rate"
    ) %>%
    print_plot_1(
      path_output_folder = path_target
      # options added through `...` into the jpeg() function
      ,width   = 1700
      ,height  = 500
      ,units   = "px"
      ,quality = 100
      ,res     = 200
    )
}
saveRDS(ls_plot_series, paste0(path_target,"ls_plots.rds") )

# GRAPH SERIES 2
path_target <- "./analysis/scenario-3/prints/series_2/"
provinces_to_pair    <- c("British Columbia", "Alberta", "Quebec")
age_groups_to_display <- c( "+1")
# for each selected province create a comparison with Canada
ls_plot_series <- list()
for(province_i in provinces_to_pair){
  ls_plot_series[[province_i]]  <- ds1 %>%
    prep_data_plot_1(
      set_sex        = c("Males","Females")
      ,set_area      = c("Canada", province_i)
      ,set_age_group = c("20-34", "+1")
    ) %>%
    make_plot_1(
      measure = "rate"
    ) %>%
    print_plot_1(
      path_output_folder = path_target
      # options added through `...` into the jpeg() function
      ,width   = 900
      ,height  = 500
      ,units   = "px"
      ,quality = 100
      ,res     = 200
    )
}
saveRDS(ls_plot_series, paste0(path_target,"ls_plots.rds") )


# ---- phase-6-place_plot ---------------------------------
l_support <- readRDS("./analysis/scenario-3/prints/series_1/ls_plots.rds")
l_support[["British Columbia"]]$path_plot
l_support[["British Columbia"]]$path_plot %>% jpeg::readJPEG() %>% grid::grid.raster()

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
  


