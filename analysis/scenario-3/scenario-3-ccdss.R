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

## INTERNAL - horizontal - TIME    - (year) 
## INTERNAL - vertical   - MEASURE - (rate)  
## INTERNAL - color      - DESIGN  - (sex)
## EXTERNAL - horizontal - DESIGN  - (area)
## EXTERNAL - vertial    - DESIGN  - (age_group)


# ---- phase-1-graph-1 --------------------------
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

# to add a few bells and whistles that aid quick evaluation
g1a <- ds1 %>% 
  dplyr::filter(area       ==  "British Columbia" ) %>% 
  dplyr::filter(age_group  ==  "20-34" ) %>%
  dplyr::filter(sex       %in% c("Males","Females") ) %>% 
  ggplot(aes(
    x      = year
    ,y     = rate
    ,color = sex
  ))+
  geom_point( shape = 21, fill = NA, size = 3 )+ # new
  geom_line( aes(group = sex), alpha = .6 )+                 # new
  geom_smooth( method = "lm", se = F, size=.2 )+              # new
  theme_minimal()+
  labs( title = "Crude prevalence of MH service utilization in BC among 20-34 year olds")
g1a

# ---- phase-1-graph-2 --------------------------
# to introduce external dimensions
g2a <- ds1 %>% 
  # dplyr::filter(area       ==  "British Columbia" ) %>% 
  dplyr::filter(age_group  ==  "20-34" ) %>%
  dplyr::filter(sex       %in% c("Males","Females") ) %>% 
  ggplot(aes(
    x      = year
    ,y     = rate
    ,color = sex
  ))+
  geom_point()+
  geom_line( aes(group = sex) )+
  facet_wrap("area")+ # new
  theme_minimal()+
  labs( title = "Crude prevalence of MH service utilization in Canada among 20-34 year olds")
g2a


g2b <- ds1 %>% 
  dplyr::filter(area       ==  "British Columbia" ) %>%
  # dplyr::filter(age_group  ==  "20-34" ) %>%
  dplyr::filter(sex       %in% c("Males","Females") ) %>% 
  ggplot(aes(
    x      = year
    ,y     = rate
    ,color = sex
  ))+
  geom_point()+
  geom_line( aes(group = sex) )+
  facet_wrap("age_group")+ # new
  theme_minimal()+
  labs( title = "Crude prevalence of MH service utilization in British Columbia")
g2b

g2c <- ds1 %>% 
  dplyr::mutate(
    years_since_2000 = year - 2000
  ) %>% 
  # dplyr::filter(area       ==  "British Columbia" ) %>%
  # dplyr::filter(age_group  ==  "20-34" ) %>%
  dplyr::filter(sex       %in% c("Males","Females") ) %>% 
  ggplot(aes(
    x      = years_since_2000 # new
    ,y     = rate
    ,color = sex
  ))+
  geom_point()+
  geom_line( aes(group = sex) )+
  # facet_grid(area ~ age_group)+ # new
  facet_grid(age_group ~ area)+ # new
  theme_minimal()+
  labs( title = "Crude prevalence of MH service utilization in Canada")
g2c

# ---- phase-1-graph-3 --------------------------
g2d <- ds1 %>% 
  dplyr::mutate(
    years_since_2000 = year - 2000
  ) %>% 
  # dplyr::filter(area       ==  "British Columbia" ) %>%
  # dplyr::filter(age_group  ==  "20-34" ) %>%
  dplyr::filter(sex       %in% c("Males","Females") ) %>% 
  ggplot(aes(
    x      = years_since_2000 # new
    ,y     = rate
    ,color = sex
  ))+
  geom_point()+
  geom_line( aes(group = sex) )+
  facet_grid(area ~ age_group)+ # new
  # facet_grid(age_group ~ area)+ # new
  theme_minimal()+
  labs( title = "Crude prevalence of MH service utilization in Canada")
g2d

# ---- phase-2-make_plot --------------------------
# suppose, we have settled on the graphical form `g2d`
g2d <- ds1 %>% 
  dplyr::mutate(
    years_since_2000 = year - 2000
  ) %>% 
  dplyr::filter(sex       %in% c("Males","Females") ) %>% 
  ggplot(aes(
    x      = years_since_2000 # new
    ,y     = rate
    ,color = sex
  ))+
  geom_point()+
  geom_line( aes(group = sex) )+
  facet_grid(area ~ age_group)+ # new
  theme_minimal()+
  labs( title = "Crude prevalence of MH service utilization in Canada")

# now let us re-express this plot as a custom function
make_plot_1 <- function(
  d
){
  d1 <- d %>% 
    dplyr::mutate(
      years_since_2000 = year - 2000
    )
  
  g_out <- d1 %>% 
    ggplot(aes_string(
       x      = "year"
       ,y     = "rate"
       ,color = "sex"
    ))+
    geom_point()+
    geom_line( aes_string(group = "sex") )+
    facet_grid(area ~ age_group)+
    theme_minimal()+
    labs( title = "Crude prevalence of MH service utilization in BC among 20-34 year olds")
  return(g_out)  
}
# how to use:
ds1 %>% 
  dplyr::filter(sex       %in% c("Males","Females") ) %>% 
  # notice that we keep operations on the data outside of the function definition
  make_plot_1()

# We need our function to offer us a convinient way to:
# 1. Control the order of the columns
# 2. Control the order of the rows
# 3. Control the order and aesthetics of the color dimention

# if we were to pack everything into a single function we would get something like:
make_plot_1_packed <- function(
  d
){
  # create support objects
  order_of_age_groups <- d1 %>% 
    dplyr::arrange() %>% 
    dplyr::distinct(age_group) %>% 
    as.list() %>% unlist() %>% as.character()
  # make total value to be at the end of the vector
  order_of_age_groups <- c(setdiff(order_of_age_groups,"1+"),"1+")
  
  order_of_areas <- d1 %>% 
    dplyr::distinct(area) %>% 
    dplyr::arrange(area) %>% 
    as.list() %>% unlist() %>% as.character()
  # make total value to be at the beginning of the vector
  order_of_areas <- c("Canada", setdiff(order_of_areas, "Canada")  )
  # 
  d1 <- d %>% 
    dplyr::mutate(
      # to create a shorter label
      years_since_2000 = year - 2000
      # to enforce the chosen order of the levels:
      ,area = factor(area, levels = order_of_areas)
      ,age_group = factor(age_group, levels = order_of_age_groups)
    )
  # to customize the color 
  palette_sex_dark         <- c("#af6ca8", "#5a8fc1") #duller than below. http://colrd.com/image-dna/42282/ & http://colrd.com/image-dna/42275/
  palette_sex_dark         <- c("#f25091", "#6718f4") #brighter than above. http://colrd.com/palette/42278/
  pallete_sex_light        <- adjustcolor(palette_sex_dark, alpha.f = .2)
  names(palette_sex_dark)  <- c("Females", "Males")
  names(pallete_sex_light) <- names(pallete_sex_light)
  palette_sex <- c(
    "Females"       = "#d95f02" # pink
    ,"Males"        = "#7570b3" # blue
    ,"Both sexes"   = "#1b9e77" # green
    )
  # taken from http://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3
  g_out <- d1 %>% 
    ggplot(aes_string(
      x      = "year"
      ,y     = "rate"
      ,color = "sex"
    ))+
    geom_point()+
    geom_line( aes_string(group = "sex") )+
    facet_grid(area ~ age_group)+
    scale_color_manual(values = palette_sex_dark)+
    # scale_color_manual(values = pallete_sex_light)+
    theme_minimal()+
    labs( title = "Crude prevalence of MH service utilization in BC among 20-34 year olds")
  return(g_out)  
}
# how to use
ds1 %>% 
  # to limit the view while in development
  # dplyr::filter(age_group %in% c("1-19", "20-34", "80+","1+")) %>% 
  # dplyr::filter(area %in% c("Canada", "Alberta", "British Columbia")) %>% 
  dplyr::filter(sex %in% c("Males","Females")) %>%
  make_plot_1_packed()

# ---- prep_data_plot_1 ------------------------------

prep_data_plot_1 <- function(
  d_input
  # ,set_area      = c("Canada")
  # ,set_age_group = c("20-34")
  # ,set_sex       = c("Males","Females")
){
  d_input <- ds1 %>% 
    # dplyr::filter(area      %in% c(set_area)     ) %>% 
    # dplyr::filter(age_group %in% c(set_age_group))%>% 
    # dplyr::filter(sex       %in% c(set_sex)  )  
    
  d1 <- d_input
  
  return(l_support)
}
# how to use


# ---- print_plot_1 ---------------------------------

print_plot_1 <- function(
  l_support
  ,path_output_folder
  ,plot_to_print = 
  ,suffix = NA
){
  
  # add a label to distinguish a particular graph (last element in the file name)
  if(!is.na(suffix)){
    (path_save_plot <- paste0(path_output_folder,"class-",class_id_pretty,"-",suffix)) 
  }else{
    (path_save_plot <- paste0(path_output_folder,"class-",class_id_pretty)) 
  }
  
  
  # print the graphical object using jpeg device
  jpeg(
    filename = paste0(path_save_plot, ".jpg")
    ,...
    # ,width = 1700
    # ,height = 1100
    # ,units = "px"
    # ,pointsize =
    # ,quality = 100
    # ,res = 200
  )
  
  l_support$plots[[plot_to_print]] %>% print() # reach into the custom object we made for graphing
  dev.off() # close the device
}

# canvas size guide ( portrait orientation )

# Size           Width x Height (mm) Width x Height (in)  Aspect Ratio
# Half Letter      140 x 216           5.5 x  8.5          1: 1.55
# Letter           216 x 279           8.5 x 11.0          1: 1.29
# Legal            216 x 356           8.5 x 14.0          1: 1.65
# Junior Legal     127 x 203           5.0 x  8.0          1: 1.60
# Ledger/Tabloid   279 x 432          11.0 x 17.0          1: 1.55


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
  


