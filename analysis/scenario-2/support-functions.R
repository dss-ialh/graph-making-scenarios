
quick_save <- function(g, name, width=200, height=130, dpi=200){
  ggplot2::ggsave(
    # filename   = paste0(name,".png"),
    filename   = paste0(name,".jpeg"),
    # filename   = paste0(name,".pdf"),
    plot       = g,
    # device     = "png",
    device     = "jpeg",
    # device     = "pdf",
    path       = "./reports/cohort-class-panorama/prints/",
    width      = width,  # letter size  = 220 x 280 mm
    height     = height, # widescreen = 900 x 1600 
    units      = "in",   # cm, mm, in
    dpi        = dpi,    #
    limitsize  = FALSE
  )
  
  # Size           Width x Height (mm) Width x Height (in)  Aspect Ratio
  # Half Letter      140 x 216           5.5 x  8.5          1: 1.55
  # Letter           216 x 279           8.5 x 11.0          1: 1.29
  # Legal            216 x 356           8.5 x 14.0          1: 1.65
  # Junior Legal     127 x 203           5.0 x  8.0          1: 1.60
  # Ledger/Tabloid   279 x 432          11.0 x 17.0          1: 1.55
}