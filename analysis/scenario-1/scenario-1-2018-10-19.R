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

path_input <- "./data-public/raw/scenario-1/cash.txt"



# ---- load-data ---------------------------------------------------------------
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
    # ,female    = ifelse(sex == 1, TRUE, FALSE) # because I disagree how `sex` is encoded
  ) %>% 
  dplyr::select( cash, age_group, sex)
ds %>% glimpse()

# ---- define-graphing-functions ---------------------------------------------------------------
# 
# The two graphing functions are copied from https://github.com/Melinae/TabularManifest.
histogram_discrete <- function(
  d_observed,
  variable_name,
  levels_to_exclude   = character(0),
  main_title          = variable_name,
  x_title             = NULL,
  y_title             = "Number of Included Records",
  text_size_percentage= 6,
  bin_width           = 1L,
  font_base_size      = 12
) {
  
  # Ungroup, in case it comes in grouped.
  d_observed <-
    d_observed %>%
    dplyr::ungroup()
  
  if( !base::is.factor(d_observed[[variable_name]]) )
    d_observed[[variable_name]] <- base::factor(d_observed[[variable_name]])
  
  d_observed$iv <- base::ordered(d_observed[[variable_name]], levels=rev(levels(d_observed[[variable_name]])))
  
  d_count <- dplyr::count_(d_observed, vars ="iv" )
  # if( base::length(levels_to_exclude)>0 ) { }
  d_count <- d_count[!(d_count$iv %in% levels_to_exclude), ]
  
  d_summary <- d_count %>%
    dplyr::rename_(
      "count"    =  "n"
    ) %>%
    dplyr::mutate(
      proportion = count / sum(count)
    )
  d_summary$percentage <- base::paste0(base::round(d_summary$proportion*100), "%")
  
  y_title <- base::paste0(y_title, " (n=", scales::comma(base::sum(d_summary$count)), ")")
  
  g <-
    ggplot(d_summary, aes_string(x="iv", y="count", fill="iv", label="percentage")) +
    geom_bar(stat="identity") +
    geom_text(stat="identity", size=text_size_percentage, hjust=.8, na.rm=T) +
    scale_y_continuous(labels=scales::comma_format()) +
    labs(title=main_title, x=x_title, y=y_title) +
    coord_flip()
  
  theme  <-
    theme_light(base_size=font_base_size) +
    theme(legend.position       =  "none") +
    theme(panel.grid.major.y    =  element_blank()) +
    theme(panel.grid.minor.y    =  element_blank()) +
    theme(axis.text.y           =  element_text(size=font_base_size + 2L)) +
    theme(axis.text.x           =  element_text(colour="gray40")) +
    theme(axis.title.x          =  element_text(colour="gray40")) +
    theme(panel.border          =  element_rect(colour="gray80")) +
    theme(axis.ticks            =  element_blank())
  
  return( g + theme )
}
histogram_continuous <- function(
  d_observed,
  variable_name,
  bin_width               = NULL,
  main_title              = base::gsub("_", " ", variable_name, perl=TRUE),
  x_title                 = paste0(variable_name, "\n(each bin is ", scales::comma(bin_width), " units wide)"),
  y_title                 = "Frequency",
  rounded_digits          = 0L,
  font_base_size          = 12
) {
  
  if( !inherits(d_observed, "data.frame") )
    stop("`d_observed` should inherit from the data.frame class.")
  
  d_observed <- d_observed[!base::is.na(d_observed[[variable_name]]), ]
  
  ds_mid_points <- base::data.frame(label=c("italic(X)[50]", "bar(italic(X))"), stringsAsFactors=FALSE)
  ds_mid_points$value <- c(stats::median(d_observed[[variable_name]]), base::mean(d_observed[[variable_name]]))
  ds_mid_points$value_rounded <- base::round(ds_mid_points$value, rounded_digits)
  
  if( ds_mid_points$value[1] < ds_mid_points$value[2] ) {
    h_just <- c(1, 0)
  } else {
    h_just <- c(0, 1)
  }
  
  g <- ggplot2::ggplot(d_observed, ggplot2::aes_string(x=variable_name))
  g <- g + ggplot2::geom_histogram(binwidth=bin_width, position=ggplot2::position_identity(), fill="gray70", color="gray90", alpha=.7)
  g <- g + ggplot2::geom_vline(xintercept=ds_mid_points$value, color="gray30")
  g <- g + ggplot2::geom_text(data=ds_mid_points, ggplot2::aes_string(x="value", y=0, label="value_rounded"), color="tomato", hjust=h_just, vjust=.5, na.rm=T)
  g <- g + ggplot2::scale_x_continuous(labels=scales::comma_format())
  g <- g + ggplot2::scale_y_continuous(labels=scales::comma_format())
  g <- g + ggplot2::labs(title=main_title, x=x_title, y=y_title)
  
  g <-
    g + ggplot2::theme_light(base_size = font_base_size) +
    ggplot2::theme(axis.ticks             = ggplot2::element_blank())
  
  ds_mid_points$top <- stats::quantile(ggplot2::ggplot_build(g)$layout$panel_ranges[[1]]$y.range, .8)
  g <- g + ggplot2::geom_text(data=ds_mid_points, ggplot2::aes_string(x="value", y="top", label="label"), color="tomato", hjust=h_just, parse=TRUE, na.rm=T)
  return( g )
}

# ---- marginals -------------------
# custom function we have defined ourselves
ds %>% histogram_continuous("cash", bin_width = 1)
# the version we stole the design from:
ds %>% TabularManifest::histogram_continuous("cash",bin_width = 1)



# custom function we have defined ourselves
ds %>% histogram_discrete("sex")
# the version we stole the design from:
ds %>% TabularManifest::histogram_discrete("age_group")


# ----- bargraphs ---------------------------
ds %>% glimpse()

g1 <- ds %>% 
  ggplot2::ggplot(aes(x = age_group, y = cash))
g1 <- g1 + geom_bar(
  mapping   = aes(fill = sex)
  ,stat     = "identity"
  ,position = position_dodge()
  ,color = "black"
  ,alpha = .5
  )
# g1 <- g1 + geom_boxplot(aes(fill = sex))
# g1 <- g1 + geom_jitter()
# g1 <- g1 + ggplot2::theme_bw()
g1 <- g1 + main_theme
g1 <- g1 + ggplot2::theme(
  panel.grid = element_blank()
  # panel.grid   = element_line(size = 2)
)
# g1 <- g1 + scale_fill_manual(values = c("female" = "black", "male" = "white" ))
# g1 <- g1 + coord_flip()
g1


g1 <- ds %>% 
  ggplot2::ggplot(aes(x=age_group, y = cash))+
  geom_bar(stat = "identity",aes(fill=sex), position = position_dodge())+
  main_theme
g1

# ---- boxplots ----------------------
g2 <- ds %>% 
  ggplot2::ggplot(aes(x=age_group, y = cash, fill= sex))+
  geom_boxplot(alpha = .5, show.legend = T)+
  geom_jitter(height = 0, width = .2, shape = 21, size = 2, alpha = .5)+
  main_theme
g2

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


