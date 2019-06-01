# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(dplyr)
library(knitr)
library(ggplot2)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes 
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("testit")# For asserting conditions meet expected patterns.
# requireNamespace("car") # For it's `recode()` function.

# ---- declare-globals -------------------------

# path_input  <- "./data-public/raw/scenario-3/PHAC_Infobase_CCDSS_-8586421808967913043.csv" # mental illness
path_input  <- "./data-public/raw/scenario-3/PHAC_Infobase_CCDSS_-8586421810172823624.csv" # mental illness
# path_save <- "./data-unshared/derived/dto-ccdss.csv"

# ---- utility-functions -----------------------

# ---- load-data -------------------------------
# see ./data-unshared/contents.md for origin of the data
# ds0 <- readr::read_csv(path_input, skip = 3)
ds0 <- readr::read_csv(path_input, skip = 3)

# ---- inspect-data ----------------------------
# see data dictionary at http://infobase.phac-aspc.gc.ca/cubes/ccdss-eng.html
ds0 %>% glimpse()

ds0 %>% 
  dplyr::filter(!is.na(Condition)) %>% 
  dplyr::group_by(Area) %>% 
  dplyr::count() %>% 
  View()

# ---- tweak-data ------------------------------
# correct variable names
names(ds0) <- gsub(" ","_", names(ds0))
names(ds0) <- gsub("%","", names(ds0))
names(ds0) <- tolower(names(ds0))

ds0 <- ds0 %>% dplyr::select(-x12)

ds0 <- ds0 %>% 
  dplyr::mutate(
    age_group = gsub("'","",age_group)
  )

ds0 %>% dplyr::glimpse()

g1 <- ds0 %>% 
  dplyr::filter(area == "British Columbia") %>% 
  dplyr::filter(!age_group == "1+") %>% 
  dplyr::filter(!sex == "Both sexes") %>% 
  ggplot(aes(
    x = age_group
    ,y = number
    ,fill = sex
  ))+
  geom_bar( stat = "identity", position = "dodge")+
  facet_grid(year~.)+
  theme_minimal()
g1

g2 <- ds0 %>%
  dplyr::filter(area == "British Columbia") %>% 
  dplyr::filter(!age_group == "1+") %>% 
  # dplyr::filter(!sex == "Both sexes") %>% 
  ggplot(aes(
    x = year
    ,y = rate
    ,color = sex
  ))+
  geom_point()+
  geom_line(aes(group = sex))+
  theme_minimal()
g2


g3 <- ds0 %>%
  dplyr::filter(!area == "Canada") %>%
  # dplyr::filter(area == "British Columbia") %>% 
  # dplyr::filter(area == "Alberta") %>% 
  # dplyr::filter(area == "Quebec") %>% 
  # dplyr::filter(age_group == "1-19") %>% 
  dplyr::filter(age_group == "20-34") %>%
  dplyr::filter(!sex == "Both sexes") %>%
  ggplot(aes(
    x = year
    ,y = rate
    # ,y = number
    # ,y = population
    ,color = sex
  ))+
  geom_point(shape = 21, fill = NA, alpha = .6, size = 1)+
  geom_line(aes(group = sex), alpha = .4)+
  geom_smooth(method = "lm", se = F, size=.1)+
  facet_wrap("area")+
  theme_minimal()
g3

# phase 0 - build the plot
# phase 1 - build the function
# phase 2 - isolate prep step
# phase 3 - isolate print step
# phase 4 - Serialize
# phase 6 - place into the canvas
  

# 
# rename_stencil %>% kable()
# # structure for use
# ds <- ds_long %>% 
#   dplyr::left_join(rename_stencil, by = "variable") %>% 
#   dplyr::left_join(ds_aqp, by = "age") %>% 
#   dplyr::select_(.dots = c(
#     "disease","year","sex","age","quarter","period",
#     "condition_present",
#     "variable","index","location","view", "unit_count","value"
#   )) %>% 
#   dplyr::mutate(
#     value   = as.numeric(value),
#     quarter = factor(quarter, levels = c(1:4),labels = c("0 - 25","26-50","51-75","76-85+")),
#     period  = factor(period,  levels = c(1:5))
#   )
# ds %>% glimpse()
# 
# # final tweaks
# proper_order_of_age_groups <- ds_aqp[,"age"] %>% as.character()
# ds <- ds %>% 
#   dplyr::mutate(
#     age = factor(age, 
#                  levels = proper_order_of_age_groups,
#                  labels = proper_order_of_age_groups ),
#     sex = factor(sex, levels = c("M","F"), labels=c("Men","Women"))
#   )
# ds %>% glimpse()
# str(ds)

# ---- save-to-disk --------------------- 
# save the created and groomed dataset for further use by subsequent reports
readr::write_csv(ds, path_save)
ds %>% saveRDS( gsub(".csv$",".rds",path_save) )

# ---- basic-graph -----------------------------
d1 <- ds %>% 
  dplyr::filter(disease == "Mental_Illness") %>% 
  dplyr::filter(index   == "Hospitalization") %>% 
  dplyr::filter(unit_count == "person") %>% 
  dplyr::filter(condition_present)

d1 %>% dplyr::summarize_all(n_distinct) %>% t()

g1 <- d1 %>% 
  ggplot(aes(x=year,y=value)) +
  geom_line(aes(group = age, color = quarter), stat="identity", size =3)+
  geom_text(aes(label = period))+
  scale_y_continuous(labels = scales::comma)+
  scale_color_brewer(type="qual")+
  # facet_grid(. ~ sex)+
  facet_grid(sex ~ condition_present)+
  labs(color = "Age group")+
  theme_minimal()
g1

