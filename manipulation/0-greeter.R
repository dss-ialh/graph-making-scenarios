# this script imports the raw data collected during quatitative phase 2 of Delphi study
# this script prepares a state of data used as a standard point of departure for any subsequent reproducible analytics

# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./stitched-output/manipulation/0-greeter.md"
# )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes


# ---- declare-globals ---------------------------------------------------------
path_input_micro <- "./data-unshared/raw/eDelphi-phase2.csv"
path_input_meta  <- "./data-public/raw/eDelphi-phase2-metadata.csv"
# path_input_meta  <- "./data-public/raw/eDelphi-phase2-metadata-edited.csv"

# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input_micro))
testit::assert("File does not exist", base::file.exists(path_input_meta))

# declare where you will store the product of this script
path_save <- "./data-unshared/derived/0-greeted.rds"

# See definitions of commonly  used objects in:
source("./manipulation/object-glossary.R")   # object definitions

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script
# for commonly used function see ./manipulation/function-support.R

# ---- load-data ---------------------------------------------------------------
# column names are very long, isolate to a separate object
col_names <- readr::read_csv(path_input_micro,n_max = 1, col_names = FALSE) %>% t()
# the main body of the survey, all the questions
ds0       <- readr::read_csv(path_input_micro,skip = 1,col_names = FALSE) %>% as.data.frame() 
# manually created object ()
ds_meta   <- readr::read_csv(path_input_meta)

ds0 %>% dplyr::glimpse()
ds_meta %>% dplyr::glimpse()


# ---- tweak-data --------------------------------------------------------------
# rename the first two columns 
ds1 <- ds0 %>% 
  dplyr::rename_( 
    "response_id"     = "X1",
    "respondent_role" = "X2"
  )
ds1 %>% dplyr::glimpse()

(variables_static  <- c("response_id","respondent_role")) 
(variables_dynamic <- setdiff(names(ds1), variables_static) )

ds2 <- ds1 %>% 
  tidyr::gather_("qid","response_value", variables_dynamic) %>% 
  dplyr::mutate(
    qid = as.integer(gsub("X","",qid)) - 2, # because of the first two variables
    response_value = as.integer(response_value)
  ) %>% 
dplyr::left_join(ds_meta, by = "qid") %>% 
  # dplyr::filter(!is.na(response_value)) %>% 
  dplyr::select(
    response_id, respondent_role, 
    qid, qlabel, response_value, dplyr::everything()
  ) 
ds2 %>% dplyr::glimpse(40)


# ---- inspect-data --------------------
# show groupings
ds_descriptives <- ds2 %>% 
  # dplyr::filter(qid == 1) %>% 
  # dplyr::filter(respondent_role == "Health System Impact Fellow") %>% 
  dplyr::group_by(qid, qlabel, respondent_role,target, subject, component) %>%
  # dplyr::group_by(qid, qlabel) %>% 
  dplyr::summarize(
    n_responses = n(),
    mean = mean(response_value, na.rm = T),
    sd   = sd(response_value,na.rm = T)
  )

# subset unique questions into a separate data frame
ds2 %>% dplyr::glimpse(50)

# ---- save-to-disk ----------------------------
readr::write_csv(ds2, "./data-unshared/derived/ds2.csv")
