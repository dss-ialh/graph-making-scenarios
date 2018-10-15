# knitr::stitch_rmd(script="./analysis/report-1/report-1.R", output="./analysis/report-1/report-1.md")
# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./analysis/report-1/report-1.R",
#   output = "./stitched-output/analysis/report-1.md"
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
requireNamespace("dplyr")
# requireNamespace("tidyr") #For converting wide to long
# requireNamespace("RColorBrewer")
# requireNamespace("scales") #For formating values in graphs
# requireNamespace("mgcv) #For the Generalized Additive Model that smooths the longitudinal graphs.
# requireNamespace("TabularManifest") # devtools::install_github("Melinae/TabularManifest")

# ---- declare-globals ---------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values

path_input <- "./data-unshared/derived/ds2.csv"

# ---- function-copy -------------------------
# The two graphing functions are copied from https://github.com/Melinae/TabularManifest.
# histogram_discrete <- function(
#   d_observed,
#   variable_name,
#   levels_to_exclude   = character(0),
#   main_title          = variable_name,
#   x_title             = NULL,
#   y_title             = "Number of Included Records",
#   text_size_percentage= 6,
#   bin_width           = 1L,
#   font_base_size      = 12
# ) {
# 
#   # Ungroup, in case it comes in grouped.
#   d_observed <-
#     d_observed %>%
#     dplyr::ungroup()
# 
#   if( !base::is.factor(d_observed[[variable_name]]) )
#     d_observed[[variable_name]] <- base::factor(d_observed[[variable_name]])
# 
#   d_observed$iv <- base::ordered(d_observed[[variable_name]], levels=rev(levels(d_observed[[variable_name]])))
# 
#   d_count <- dplyr::count_(d_observed, vars ="iv" )
#   # if( base::length(levels_to_exclude)>0 ) { }
#   d_count <- d_count[!(d_count$iv %in% levels_to_exclude), ]
# 
#   d_summary <- d_count %>%
#     dplyr::rename_(
#       "count"    =  "n"
#     ) %>%
#     dplyr::mutate(
#       proportion = count / sum(count)
#     )
#   d_summary$percentage <- base::paste0(base::round(d_summary$proportion*100), "%")
# 
#   y_title <- base::paste0(y_title, " (n=", scales::comma(base::sum(d_summary$count)), ")")
# 
#   g <-
#     ggplot(d_summary, aes_string(x="iv", y="count", fill="iv", label="percentage")) +
#     geom_bar(stat="identity") +
#     geom_text(stat="identity", size=text_size_percentage, hjust=.8, na.rm=T) +
#     scale_y_continuous(labels=scales::comma_format()) +
#     labs(title=main_title, x=x_title, y=y_title) +
#     coord_flip()
# 
#   theme  <-
#     theme_light(base_size=font_base_size) +
#     theme(legend.position       =  "none") +
#     theme(panel.grid.major.y    =  element_blank()) +
#     theme(panel.grid.minor.y    =  element_blank()) +
#     theme(axis.text.y           =  element_text(size=font_base_size + 2L)) +
#     theme(axis.text.x           =  element_text(colour="gray40")) +
#     theme(axis.title.x          =  element_text(colour="gray40")) +
#     theme(panel.border          =  element_rect(colour="gray80")) +
#     theme(axis.ticks            =  element_blank())
# 
#   return( g + theme )
# }
# histogram_continuous <- function(
#   d_observed,
#   variable_name,
#   bin_width               = NULL,
#   main_title              = base::gsub("_", " ", variable_name, perl=TRUE),
#   x_title                 = paste0(variable_name, "\n(each bin is ", scales::comma(bin_width), " units wide)"),
#   y_title                 = "Frequency",
#   rounded_digits          = 0L,
#   font_base_size          = 12
# ) {
# 
#   if( !inherits(d_observed, "data.frame") )
#     stop("`d_observed` should inherit from the data.frame class.")
# 
#   d_observed <- d_observed[!base::is.na(d_observed[[variable_name]]), ]
# 
#   ds_mid_points <- base::data.frame(label=c("italic(X)[50]", "bar(italic(X))"), stringsAsFactors=FALSE)
#   ds_mid_points$value <- c(stats::median(d_observed[[variable_name]]), base::mean(d_observed[[variable_name]]))
#   ds_mid_points$value_rounded <- base::round(ds_mid_points$value, rounded_digits)
# 
#   if( ds_mid_points$value[1] < ds_mid_points$value[2] ) {
#     h_just <- c(1, 0)
#   } else {
#     h_just <- c(0, 1)
#   }
# 
#   g <- ggplot2::ggplot(d_observed, ggplot2::aes_string(x=variable_name))
#   g <- g + ggplot2::geom_histogram(binwidth=bin_width, position=ggplot2::position_identity(), fill="gray70", color="gray90", alpha=.7)
#   g <- g + ggplot2::geom_vline(xintercept=ds_mid_points$value, color="gray30")
#   g <- g + ggplot2::geom_text(data=ds_mid_points, ggplot2::aes_string(x="value", y=0, label="value_rounded"), color="tomato", hjust=h_just, vjust=.5, na.rm=T)
#   g <- g + ggplot2::scale_x_continuous(labels=scales::comma_format())
#   g <- g + ggplot2::scale_y_continuous(labels=scales::comma_format())
#   g <- g + ggplot2::labs(title=main_title, x=x_title, y=y_title)
# 
#   g <-
#     g + ggplot2::theme_light(base_size = font_base_size) +
#     ggplot2::theme(axis.ticks             = ggplot2::element_blank())
# 
#   ds_mid_points$top <- stats::quantile(ggplot2::ggplot_build(g)$layout$panel_ranges[[1]]$y.range, .8)
#   g <- g + ggplot2::geom_text(data=ds_mid_points, ggplot2::aes_string(x="value", y="top", label="label"), color="tomato", hjust=h_just, parse=TRUE, na.rm=T)
#   return( g )
# }

# ---- load-data ---------------------------------------------------------------
ds <- readr::read_csv(path_input) # 'ds' stands for 'datasets'

# ---- tweak-data --------------------------------------------------------------

ds_common <- ds %>%
  # ds_unique <- ds %>% 
  dplyr::filter(!is.na(response_value)) %>% 
  # dplyr::filter(component == "Common") %>%
  # dplyr::filter(component == "Unique") %>%
  # dplyr::filter(subject == "Contribution") %>%
  # dplyr::filter(respondent_role == "Health System Impact Fellow") %>%
  dplyr::mutate(
    qpretty = paste0("[",qid,"] - ",qlabel),
    common_item_id    = paste0(qid_fellow,"-", qid_host,"-",qid_academic),
    common_item_label = paste0("[",common_item_id,"] - ", qlabel),
    # so that no "NA" is displayed in the value:
    common_item_id    = gsub("NA","",common_item_id),
    common_item_label = gsub("NA","",common_item_label)
    # wrap the text of the display label
    # ,common_item_label_new = wrap_text(common_item_label, 80)
    # ,common_item_label_new = break_into_lines(common_item_label, line_length = 80)
  )
ds_common %>% dplyr::glimpse()
# unique(ds_common$common_item_label)[25]
# unique(ds_common$common_item_label_new)[25]
# text_string = "[--48] - Revealing the strengths and weaknesses of the current academic training environment, and opportunities for curriculum enhancement within HSPR"
# wrap_text(text_string, width_value = 80)



# ---- define-graphing-functions ---------------------------------------------------------------
# 
# wrap_text <- function(text_string, width_value){
#   wrapped_text <- paste0(base::strwrap(text_string, width = width_value, simplify = T), collapse = " \n ")
#   return(wrapped_text)
# }
# # how to use:
# text_string = "[--48] - Revealing the strengths and weaknesses of the current academic training environment, and opportunities for curriculum enhancement within HSPR"
# wrap_text(text_string, width_value = 80)



library(ggplot2)

# define the graphing function to distribution in the matrix plot
histogram_continuous <- function (
  d_observed, 
  variable_name, 
  bin_width      = NULL, 
  main_title     = base::gsub("_", " ", variable_name, perl = TRUE), 
  sub_title      = NULL, 
  caption        = paste0("each bin is ", scales::comma(bin_width), " units wide"), 
  x_title        = variable_name, 
  y_title        = "Frequency", 
  x_axis_format  = scales::comma_format(), 
  rounded_digits = 0L, 
  font_base_size = 12
) 
{
  # browser()
  # d_observed     = ds_common %>% dplyr::filter(common_item_id == "3-22-42") %>% dplyr::filter(target == "Host")
  # # d_observed     = ds_common %>% dplyr::filter(common_item_id == "9") #%>% dplyr::filter(target == "Host")
  # variable_name  = "response_value"
  # bin_width      = 1
  # main_title     = base::gsub("_", " ", variable_name, perl = TRUE)
  # sub_title      = NULL
  # caption        = paste0("each bin is ", scales::comma(bin_width), " units wide")
  # x_title        = variable_name
  # y_title        = "Frequency"
  # x_axis_format  = scales::comma_format()
  # rounded_digits = 2L
  # font_base_size = 12

  
  
  
  if (!inherits(d_observed, "data.frame")) 
    stop("`d_observed` should inherit from the data.frame class.")
  d_observed <- d_observed[!base::is.na(d_observed[[variable_name]]), 
                           ]
  non_empty <- (nrow(d_observed) >= 1L)
  if (non_empty) {
    ds_mid_points <- base::data.frame(label = c("bar(italic(X))","italic(SD)"), stringsAsFactors = FALSE )
    ds_mid_points$value <- c(base::mean(d_observed[[variable_name]]),
                             stats::sd(d_observed[[variable_name]]))
    ds_mid_points$value_rounded <- sprintf("%.*f", rounded_digits, ds_mid_points$value)
    ds_mid_points$line_position <- ds_mid_points$value[1]
    # if (ds_mid_points$value[1] < ds_mid_points$value[2]) {
      h_just <- c(1.5, -0.2)
      # h_just <- c(1.4)
      # h_just <- c(-0.2)
    # }
    # else {
    #   h_just <- c(-0.1, 1.1)
    # }
  } else {
    main_title <- paste0("Empty: ", main_title)
    caption <- "The variable contains only missing values.\nThere is nothing to graph."
    ds_mid_points <- tibble::tribble(~label, ~value, ~value_rounded, 
                                     "italic(X)[50]", NA_real_, NA_character_, "bar(italic(X))", 
                                     NA_real_, NA_character_)
    h_just <- c(1.2, -0.2)
    # h_just <- c(1.1)
    # h_just <- c(0.2)
  }
  palette_midpoint <- c("#2274A5", "#32936F")
  palette_midpoint_line <- c("#2274A5", "#2274A5")
  # palette_midpoint <- c("#2274A5")
  # rename input for faceting
  if(!non_empty){
    g <- ggplot()+geom_blank(aes(1,1)) + cowplot::theme_nothing()
  }else{
  g <- ggplot2::ggplot(d_observed, ggplot2::aes_string(x = variable_name)) + 
    ggplot2::geom_histogram(binwidth = bin_width ,position = ggplot2::position_identity()
      ,fill = "gray92", color = "gray80", size = .5, alpha = 0.8) + 
    ggplot2::geom_vline(xintercept = ds_mid_points$line_position,color = palette_midpoint_line, na.rm = T) +
    # ggplot2::geom_vline(xintercept = rep(ds_mid_points$value[1],2),color = palette_midpoint_line, na.rm = T) +
    ggplot2::geom_text(data    = ds_mid_points,
                       mapping =  ggplot2::aes_string(x = "line_position", y = -Inf, label = "value_rounded"),
                       color   = palette_midpoint,
                       hjust   = h_just,
                       vjust   = -1.2,
                       na.rm   = T
                       ) +
    ggplot2::geom_text(data    = ds_mid_points,
                       mapping = ggplot2::aes_string(x = "line_position", y = Inf, label = "label"),
                       color   = palette_midpoint,
                       hjust   = h_just,
                       vjust   = 1.2,
                       parse   = TRUE,
                       na.rm   = T
                       ) +
    ggplot2::scale_x_continuous(labels = x_axis_format, breaks = 1:5, limits = c(.5,5.5)) + 
    # ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(-.5, 24)) + 
    ggplot2::scale_y_continuous(labels = scales::comma_format() ) + 
    ggplot2::coord_cartesian(ylim = c(0,22))
    ggplot2::labs(
      title = main_title, subtitle = sub_title, 
      caption = caption, x = x_title, y = y_title
    )
    
  
  g <- g + ggplot2::theme_light(base_size = font_base_size) + 
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) + 
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "gray90")) + 
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = "gray94")) + 
    ggplot2::theme(plot.caption = ggplot2::element_text(color = "gray60")) + 
    ggplot2::theme(axis.title.y = ggplot2::element_text(color = "gray60"))
  # g
  }
  return(g)
}
 
# g1 <- ds_common %>%
#   dplyr::filter(subject == "Contribution") %>%
#   dplyr::filter(common_item_id == "9--") %>%
#   dplyr::filter(target == "Fellow") %>%
#   histogram_continuous("response_value",bin_width = 1, rounded_digits = 2)
# g1

# define complext plot, matrix of simple views
matrix_plot <- function(
  d, # ds_long
  # patterns
  # ,
  facet_x = "target",
  # facet_y = "common_item_label"
  facet_y = "qlabel"
  ,base_plus = 0
  ,plot_title
){
  # values for testing
  # d <- ds_common %>% dplyr::filter(subject == "Contribution")
  # facet_x = "target"
  # facet_y = "common_item_label"
  # facet_y = "common_item_id"

  # create a list of plots to facet with ggmatrix
  (facet_y_values <- unique(d[,facet_y])%>% as.list() %>% unlist() %>% as.character())# 
  (facet_x_values <- unique(d[,facet_x]) %>% as.list() %>% unlist() %>% as.character() )  #"Fellow"   "Host"     "Academic"
  ls <- list()
  for(i_item in (facet_y_values)){
    for(i_role in (facet_x_values)){
      
    ls[[paste0(i_item,"-",i_role)]] <- d %>% 
      # dplyr::mutate(
      #   common_item_label = break_into_lines(common_item_label, line_length = 80)
      # ) %>% 
      # dplyr::filter(common_item_id == i_item) %>%
      # dplyr::filter(common_item_label == i_item) %>%
      # see Non Standard Evaluations (NSE) with dplyr::filter article on StackOverflow                  
      # https://stackoverflow.com/questions/31760134/using-filter-in-dplyr-where-both-field-and-value-are-in-variables
      dplyr::filter_(      #    value/field     variable/column
        lazyeval::interp( ~v == i_item, v = as.name(facet_y) )
      ) %>% 
      dplyr::filter(target == i_role) %>% 
      histogram_continuous(variable_name = "response_value", bin_width = 1,
                           rounded_digits = 2,
                           main_title = paste0(i_item, " - ", i_role))
    } 
  } 
  # ls$`[1-20-40] - Research/analytic skills and expertise-Host`
  # ls$`[1-20-40] - Research/analytic skills and expertise-Fellow`
  # place the plots into a single ggmatrix
  mplot <- GGally::ggmatrix(
    ls
    ,ncol = length(facet_x_values)
    ,nrow = length(facet_y_values)
    # title = "Observed MMSE scores for three types of response patterns",
    # yAxisLabels = patterns,
    ,yAxisLabels = facet_y_values
    ,xAxisLabels = facet_x_values
    ,title = plot_title
    # xlab = "Years since baseline", ylab = "Mini Mental State Exam (MMSE) Score"
 
    # legend = 1
  ) + theme(
    legend.position = "right"
    ,strip.text.x = element_text(size=baseSize+2)
    ,strip.text.y = element_text(size=baseSize+base_plus, angle = 0, hjust = 0)
    ,panel.grid.minor.y = element_blank()
    
  )
  mplot
}
# usage demo:
# ds_common %>% dplyr::filter(subject == "Contribution") %>% matrix_plot(facet_y = "common_item_label")
# ds_common %>% dplyr::filter(subject == "Contribution") %>% matrix_plot(facet_y = "qlabel")
# ds_common %>% dplyr::filter(subject == "Criterion") %>% matrix_plot()


# create a function that prints an alluvia plot with the label on top as a separate plot
print_plot_basic_item_fq <- function(  
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
  g <- g + ggplot2::theme()
  g %>% print()
  # l_support$plots$alluvia2axes %>% ggpubr::get_legend() %>% ggpubr::as_ggplot() %>% print()
  # l_support$plots$alluvia2axes %>% ggpubr::get_legend() %>% ggpubr::as_ggplot() %>% print()
  dev.off() # close the device
  # jpeg device close
  
}
# how to use
# ds_common %>% 
#   dplyr::filter(subject == "Contribution") %>% 
#   matrix_plot(facet_y = "qlabel") %>% 
#   print_plot_basic_item_fq(
#     "./analysis/report-1/prints/"
#     ,"contribution1"
#     ,width         = 279
#     ,height        = 216
#     ,units         = "mm"
#     ,quality       = 100 # percent
#     ,res           = 600 # dpi
#     )


# ----- print-graphs ---------------------
one_bar_height <- 22
header_height <- 8
local_title = "Please rate your agreement with the use of each of the following CRITERIA (identified in Round 1) 
to evaluate the SUCCESS of your Health System Impact Fellowship. 
(Responses include: 1-Strongly disagree, 2-Disagree, 3-Neutral, 4-Agree, 5-Strongly agree )"

i_subject   <- "Criterion"
i_component <- "Common"
ds_common %>% 
  dplyr::filter(subject == i_subject) %>% 
  dplyr::filter(component == i_component) %>% 
  matrix_plot(facet_y = "qlabel", base_plus = 2, plot_title = local_title ) %>% 
  print_plot_basic_item_fq(
    "./analysis/report-1/prints/"
    ,graph_name    = i_subject
    ,suffix        = i_component
    ,width         = 276
    ,height        = (one_bar_height*7)+header_height
    ,units         = "mm"
    ,quality       = 100 # percent
    ,res           = 600 # dpi
  )

i_subject   <- "Criterion"
i_component <- "Unique"
ds_common %>% 
  dplyr::filter(subject == i_subject) %>% 
  dplyr::filter(component == i_component) %>% 
  matrix_plot(facet_y = "qlabel", base_plus = 2, plot_title = local_title ) %>% 
  print_plot_basic_item_fq(
    "./analysis/report-1/prints/"
    ,graph_name    = i_subject
    ,suffix        = i_component
    ,width         = 276
    ,height        = (one_bar_height*8)+header_height
    ,units         = "mm"
    ,quality       = 100 # percent
    ,res           = 600 # dpi
  )

i_subject   <- "Criterion"
# i_component <- "Common"
ds_common %>% 
  dplyr::filter(subject == i_subject) %>% 
  # dplyr::filter(component == i_component) %>% 
  matrix_plot(facet_y = "qlabel", base_plus = 0, plot_title = local_title ) %>% 
  print_plot_basic_item_fq(
    "./analysis/report-1/prints/"
    ,graph_name    = i_subject
    # ,suffix        = i_component
    # ,
    ,width         = 230
    ,height        = 356
    ,units         = "mm"
    ,quality       = 100 # percent
    ,res           = 600 # dpi
  )

# Size           Width x Height (mm) Width x Height (in)  Aspect Ratio
# Half Letter      140 x 216           5.5 x  8.5          1: 1.55
# Letter           216 x 279           8.5 x 11.0          1: 1.29
# Legal            216 x 356           8.5 x 14.0          1: 1.65
# Junior Legal     127 x 203           5.0 x  8.0          1: 1.60
# Ledger/Tabloid   279 x 432          11.0 x 17.0          1: 1.55


one_bar_height <- 22
header_height <- 8
local_title = "Please rate your agreement with each of the following CONTRIBUTION (identified in Round 1) 
that Health System Impact FELLOWS have made to your health system organization
(Responses include: 1-Strongly disagree, 2-Disagree, 3-Neutral, 4-Agree, 5-Strongly agree )"

i_subject   <- "Contribution"
i_component <- "Common"
ds_common %>% 
  dplyr::filter(subject == i_subject) %>% 
  dplyr::filter(component == i_component) %>% 
  matrix_plot(facet_y = "qlabel", base_plus = 2, plot_title = local_title ) %>% 
  print_plot_basic_item_fq(
    "./analysis/report-1/prints/"
    ,graph_name    = i_subject
    ,suffix        = i_component
    ,width         = 276
    ,height        = (one_bar_height*8)+header_height
    ,units         = "mm"
    ,quality       = 100 # percent
    ,res           = 600 # dpi
  )

i_subject   <- "Contribution"
i_component <- "Unique"
ds_common %>% 
  dplyr::filter(subject == i_subject) %>% 
  dplyr::filter(component == i_component) %>% 
  matrix_plot(facet_y = "qlabel", base_plus = 2, plot_title = local_title ) %>% 
  print_plot_basic_item_fq(
    "./analysis/report-1/prints/"
    ,graph_name    = i_subject
    ,suffix        = i_component
    ,width         = 276
    ,height        = (one_bar_height*5)+header_height
    ,units         = "mm"
    ,quality       = 100 # percent
    ,res           = 600 # dpi
  )

i_subject   <- "Contribution"
# i_component <- "Common"
ds_common %>% 
  dplyr::filter(subject == i_subject) %>% 
  # dplyr::filter(component == i_component) %>% 
  matrix_plot(facet_y = "qlabel", base_plus = 0, plot_title = local_title ) %>% 
  print_plot_basic_item_fq(
    "./analysis/report-1/prints/"
    ,graph_name    = i_subject
    # ,suffix        = i_component
    # ,
    ,width         = 230
    ,height        = 320
    ,units         = "mm"
    ,quality       = 100 # percent
    ,res           = 600 # dpi
  )




# ---- create-table-of-results ---------------------
ds_descriptives <- ds_common %>% 
  # dplyr::filter(qid == 1) %>% 
  # dplyr::filter(respondent_role == "Health System Impact Fellow") %>% 
  dplyr::group_by(qid, qlabel, respondent_role,target, subject, component) %>%
  # dplyr::group_by(qid, qlabel) %>% 
  dplyr::summarize(
    n_responses = n(),
    mean = mean(response_value, na.rm = T),
    sd   = sd(response_value,na.rm = T)
  )

readr::write_csv(ds_descriptives, "./analysis/report-1/products/ds_descriptives.csv" )



