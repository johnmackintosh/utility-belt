#' Arrange facet
#'
#' internal function, using arguments ncols and facet_scales.
#' These are used within the call to facet_wrap
#'
#' If the council area is "Highland", and 9 columns are specified, then
#' the facet labels are wrapped at 10 characters
#'
#' The same applies for "Argyll and Bute" with 4 columns
#'
#' Otherwise facet as specified and leave the facet labels as is
#'
#' @param plot   existing ggplot object to modify
#' @param councilval  Highland or Argyll and Bute
#' @param ncols number of columns in the faceted plot
#' @param facet_scales usually "fixed"
#'
#' @return returns the modified ggplot object
#' @keywords {internal}
#'
#' @examples 
#' 
#'  p <- arrange_facets(
#'  plot = p, 
#'  councilval = councilval, 
#'  ncols = ncols, 
#'  facet_scales = facet_scales)
#' 
arrange_facets <- function(plot = p,
                           councilval,
                           ncols,
                           facet_scales) {

  condition1 <- councilval == "Highland" & ncols == 9
  condition2 <- councilval == "Argyll and Bute" & ncols == 4

  # wrap the facet labels if plotting all on one row - Highland

  if (condition1 | condition2) {

      p <- plot + ggplot2::facet_wrap(. ~ areaname,
                                   ncol = ncols,
                                   scales = facet_scales,
                                   labeller = labeller(areaname = ggplot2::label_wrap_gen(10)))
  } else {

    p <- plot + ggplot2::facet_wrap(. ~ areaname,
                                    ncol = ncols,
                                    scales = facet_scales)
    }



}

