##################################################
## Project: ALVPH Color Style                   ##
## Script purpose: ALVPH Color Style            ##
## Date: 22.12.21                               ##
## Author: Guy Schnidrig BLV/VPHI               ##
##################################################

#
usethis::use_package("ggplot2") # Default is "Imports"

# Define spectrum colors ####
spektrum_col<- c(
  `blue_1`   = "#748cb2",
  `green_1`  = "#9cc677",
  `yellow_1` = "#eacf5e",
  `orange_1` = "#f9ad79",
  `rosa_1`   = "#d16a7c",
  `purple_1` = "#8873a2",
  `teal_1`   = "#3a95b3",
  `green_2`  = "#b5d949",
  `yellow_2` = "#fdd46c",
  `orange_2` = "#f9ac79",
  `rosa_2`   = "#a65084",
  `blue_2`   = "#0064b1",
  `green_3`  = "#48ab6f",
  `orange_3` = "#de8168",
  `red_1`    = "#f28366",
  `purple_2` = "#b0517f",
  `blue_3`   = "#4c4887")

spektrum_colors <- as.data.frame(t(spektrum_col))

#' Function for hex codes ####
#' @param ... Character names of spektrum
spektrum <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (spektrum_col)
  
  spektrum_col[cols]
}

#' Function to list all spektrum colors with hex code ####
#' @keywords spektrum color
#' @export
list_spektrum_colors <- function(...){
  return (spektrum_col)
}


# Make palettes ####
spektrum_palettes <- list(
  
  `main`     = spektrum("blue_1",
                        "green_1",
                        "yellow_1",
                        "orange_1",
                        "rosa_1",
                        "purple_1",
                        "teal_1",
                        "green_2",
                        "yellow_2",
                        "orange_2",
                        "rosa_2",
                        "blue_2",
                        "green_3",
                        "orange_3",
                        "red_1",
                        "purple_2",
                        "blue_3"),
  
  `two`      = spektrum("blue_1",
                        "green_1"),
  
  `four`     = spektrum("blue_1", 
                        "green_1",
                        "yellow_1",
                        "orange_1"),
  
  `krit_ws`  = spektrum("yellow_1",
                        "orange_2",
                        "orange_3"),
  
  `wsk10`    = spektrum("blue_1",
                        "green_1",
                        "rosa_1",
                        "purple_1",
                        "teal_1",
                        "green_2",
                        "rosa_2",
                        "yellow_1",
                        "orange_2",
                        "orange_3"),
  
  `wsk11`    = spektrum("blue_1",
                        "green_1",
                        "rosa_1",
                        "purple_1", 
                        "teal_1",
                        "green_2",
                        "rosa_2",
                        "blue_2",
                        "yellow_1",
                        "orange_2",
                        "orange_3"),
  
  `wsk12`    = spektrum("blue_1",
                        "green_1",
                        "rosa_1",
                        "purple_1",
                        "teal_1", 
                        "green_2",
                        "rosa_2",
                        "blue_2",
                        "green_3",
                        "yellow_1",
                        "orange_2",
                        "orange_3"))

#' Function to list all spektrum palettes####
#' @keywords spektrum palette
#' @export
list_spektrum_palettes <- function(...){
  return (spektrum_palettes)
}


#'  Palette function####
#'  @param palette Character name of palette in spektrum_palettes
#'  @param reverse Boolean indicating whether the palette should be reversed
#'  @param ... Additional arguments to pass to colorRampPalette()
spektrum_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- spektrum_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

#' Functions for Scales in ggplot2 ####

#' color scale constructor for spektrum
#' @param palette Character name of palette in spektrum_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale or
#' scale_color_gradientn, used respectively when discrete is TRUE or FALSE
#' @export
scale_color_spektrum <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- spektrum_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("spektrum_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for spektrum
#' @param palette Character name of palette in spektrum_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale or
#' scale_fill_gradientn, used respectively when discrete is TRUE or FALSE
#' @export
scale_fill_spektrum <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- spektrum_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("spektrum_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# imports the replace function from ggplot, gave me trouble without

`%+replace%` <- ggplot2::'%+replace%' # nolint

#' Makes custom ggplot theme
#' @export
theme_spektrum <- function(){ 
  font <- "sans"
  ggplot2::theme_minimal() %+replace%      
  ggplot2::theme(
    
      plot.title = ggplot2::element_text(family = font, 
                                         size = 20,
                                         face = 'bold',
                                         hjust = 0,
                                         vjust = 2),                
      
      plot.subtitle = ggplot2::element_text(family = font,
                                            face = 'bold',
                                            size = 14),                
      
      plot.caption = ggplot2::element_text(family = font,
                                           size = 12,
                                           hjust = 1),               
      
      legend.title= ggplot2::element_text(size=14,
                                          face = 'bold'),
      
      axis.title = ggplot2::element_text(family = font,
                                         size = 14),               
      
      axis.title.x = ggplot2::element_blank(),
      
      axis.title.y = ggplot2::element_text(face = 'bold',
                                           angle = 90,
                                           margin=ggplot2::margin(0, 15, 0, 0)),
      
      axis.text = ggplot2::element_text(family = font,
                                        size = 12)
        )
}

#Test####
# Fill by discrete variable with different palette + remove legend (guide)
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(manufacturer, fill = manufacturer)) +
  ggplot2::geom_bar() +
  scale_fill_spektrum(palette = "wsk12")+
  theme_spektrum()

