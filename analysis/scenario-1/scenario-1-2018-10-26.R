# knitr::stitch_rmd(script="./analysis/report-1/report-1.R", output="./analysis/report-1/report-1.md")
# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./analysis/scenario-1/scenario-1-2018-10-26.R",
#   output = "./analysis/scenario-1/scenario-1-2018-10-26.md"
# )
# this command is typically executed by the ./manipulation/governor.R

# goal for today
# 1) reproduce this script without errors through tweak-data chunk
# 1) add improvements to the bargraph created on 2018-10-19
# 2) compose a function that re-generates this graph
# 3) compose a report narrating today's session 
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
# create a string object that store the path to the data file
path_input <- "./data-public/raw/scenario-1/cash.txt"

# ---- load-data ---------------------------------------------------------------
# an alterntative way to store small datasets in R
# useful to share reproducible examples on online forums
# use fundtion dput() to print an object in this way
# ds <- structure(list(X1 = c(21, 23, 19, 22, 22, 23, 21, 22, 20, 21, 
#                       19, 25, 30, 29, 26, 28, 27, 27, 26, 29, 27, 28, 27, 29, 25, 22, 
#                       23, 21, 22, 21, 23, 19, 20, 21, 20, 20),
#                X2 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
#                       2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
#                X3 = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L,
#                       1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
#                       1L, 2L, 2L, 2L, 2L, 2L, 2L), 
#                X4 = c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 
#                       6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L)), 
#           .Names = c("X1", "X2", "X3", "X4"), 
#           row.names = c(NA, -36L), 
#           class = c("tbl_df", "tbl", "data.frame"), 
#           spec = structure(
#             list(cols = structure(list(X1 = structure(list(), class = c("collector_double", "collector")
#                     ),X2 = structure(list(), class = c("collector_integer", "collector")), 
#                     X3 = structure(list(), class = c("collector_integer", "collector")), 
#                     X4 = structure(list(), class = c("collector_integer", "collector"))), 
#                     .Names = c("X1", "X2", "X3", "X4")), 
#                  default = structure(list(), class = c("collector_guess", "collector"))),
#             .Names = c("cols", "default"), class = "col_spec"))

# alternatively, read from stored file
ds <- readr::read_table2(path_input, col_names = FALSE) # 'ds' stands for 'datasets'

# ---- tweak-data --------------------------------------------------------------

names(ds) <- c("cash", "age_group", "sex", "group")
ds %>% glimpse()
ds %>% head()

ds %>% 
  dplyr::group_by(age_group) %>%
  dplyr::summarize(
    n = n()
    , average = mean(cash)
  )
ds %>% dplyr::glimpse()
# create factor levels
ds <- ds %>% 
  dplyr::mutate(
    age_group = factor(
      x        = age_group
      , levels = c(1,2,3)
      , labels = c("young","middle","elderly")
    )
    ,sex       = factor(x = sex, levels = c(1,2),   labels = c("female", "male"))
    ) %>% 
  dplyr::select( cash, age_group, sex)
ds %>% glimpse() 

# ---- define-graphing-functions ---------------------------------------------------------------

# ----- a1 - ---------------------------
ds %>% head()
# our previous take to graph this data resulted in the following graph:
g1 <- ds %>% 
  ggplot2::ggplot(aes(x = age_group, y = cash))
g1 <- g1 + geom_bar(
  mapping   = aes(fill = sex)
  ,stat     = "identity"
  ,position = position_dodge()
  ,color = "black"
  ,alpha = .2
)
g1 <- g1 + main_theme
g1 <- g1 + ggplot2::theme(
  panel.grid = element_blank()
)
g1
# there are multiple bars becasue we plot each observation as a bar
# instead we should plot each observation as a point,
# and plot group means as a bars in a separate graph

# if we insist on displaying both individual data points AND group means
# in the same graph, I would recommend resorting to boxplots
g2 <- ds %>% 
  ggplot2::ggplot(aes(x=age_group, y = cash, fill= sex))+
  geom_boxplot(alpha = .5, show.legend = T)+
  geom_jitter(height = 0, width = .2, shape = 21, size = 2, alpha = .5)+
  main_theme
g2

# ---- a2 --------------------------------------

ds %>% 
  dplyr::group_by(age_group, sex) %>% 
  dplyr::summarize(
    n = n()
    ,mean = mean(cash,na.rm=T)
  )

g1 <- ds %>% 
  dplyr::group_by(age_group, sex) %>% 
  dplyr::summarize(
    n = n()
    ,cash_mean = mean(cash,na.rm=T)
  ) %>% 
  ggplot(aes(x = age_group, y = cash_mean)) +
  geom_bar(
    aes(fill = sex)
    ,stat = "identity"
    ,position = position_dodge()
  )+
  main_theme
g1





# ---- boxplots ----------------------


g2a <- ds %>% 
  ggplot2::ggplot(aes(x=age_group, y = cash, fill= sex))+
  geom_violin(alpha = .5, show.legend = T)+
  geom_jitter(height = 0, width = .2, shape = 21, size = 2, alpha = .5)+
  main_theme
g2a

# ----- print-graphs ---------------------

# create a function that prints an alluvia plot with the label on top as a separate plot
print_plot_basic <- function(  
  g
  ,path_output_folder 
  ,graph_name 
  ,suffix = NA
  ,...
){
  # if folder does not exist yet, create it
  if(!dir.exists(path_output_folder)){
    dir.create(path_output_folder)
  }
  # browser()
  graph_name <- paste0(graph_name) # for automatic adjustment if needed
  
  # add a label to distinguish a particular graph (last element in the file name)
  if(!is.na(suffix)){
    (path_save_plot <- paste0(path_output_folder,graph_name,"-",suffix)) 
  }else{
    (path_save_plot <- paste0(path_output_folder,graph_name)) 
  }
  # implemet plot corrects for the complex display
  
  # jpeg device open
  jpeg(
    filename = paste0(path_save_plot, ".jpg")
    ,...
  )
  # g <- g + ggplot2::theme()
  g %>% print()
  # l_support$plots$alluvia2axes %>% ggpubr::get_legend() %>% ggpubr::as_ggplot() %>% print()
  # l_support$plots$alluvia2axes %>% ggpubr::get_legend() %>% ggpubr::as_ggplot() %>% print()
  dev.off() # close the device
  # jpeg device close
  
}
# how to use
g <- ds %>% 
  ggplot2::ggplot(aes(x=age_group, y = cash, fill= sex))+
  geom_boxplot(alpha = .5, show.legend = T)+
  geom_jitter(height = 0, width = .2, shape = 21, size = 2, alpha = .5)+
  main_theme 
g %>% 
  print_plot_basic(
    "./analysis/scenario-1/prints/"
    ,"example-1"
    ,width         = 216
    ,height        = 140
    ,units         = "mm"
    ,quality       = 100 # percent
    ,res           = 300 # dpi
    )

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
path_publish_report_1 <- "./analysis/scenario-1/scenario-1.Rmd"
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


