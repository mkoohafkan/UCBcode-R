# isn't this included in R?
plotCol = function(col, nrow=1, ncol=ceiling(length(col) / nrow),
                    txt.col="black") {
    stopifnot(nrow >= 1, ncol >= 1)
    if(length(col) > nrow*ncol)
        warning("some colors will not be shown")
    require(grid)
    grid.newpage()
    gl <- grid.layout(nrow, ncol)
    pushViewport(viewport(layout=gl))
    ic <- 1
    for(i in 1:nrow) {
        for(j in 1:ncol) {
            pushViewport(viewport(layout.pos.row=i, layout.pos.col=j))
            grid.rect(gp= gpar(fill=col[ic]))
            grid.text(col[ic], gp=gpar(col=txt.col))
            upViewport()
            ic <- ic+1
        }
    }
    upViewport()
    invisible(gl)
}

gradient_from_palette = function(pname, type='color', n=20, showpalette=TRUE){
require(ggplot2)
require(RColorBrewer)
# converts a named palette (i.e. a palette accessible by 
# RColorBrewer::scale_color_brewer, or R standard color maps such as 
# topo.colors or terrain.colors) to a ggplot2 color gradient.
# ARGUMENTS
#   pname = the name of the palette. If a single string is passed,
#           it is assumed to be the name of a palette in RColorBrewer.
#           If it is a vector of strings, it is assumed to be a list of
#           valid colors (either named or hex).
#   type = the ggplot scale function, can be 'color' or 'fill'.
#   n = the number of color points listed. Can be somewhat arbitrary, 
#       but larger numbers give more control over the gradient path.
#   showpalette = optionally plot the colors in the palette.
# OUTPUT
# a ggplot color scale aesthetic.
#
# # define palette
  if(class(pname) == 'function')
    colorlist = pname(n)
  else if(length(pname) < 2)
    colorlist = suppressWarnings(brewer.pal(n, pname))
  else
    colorlist = pname
  # show the colors
  if(showpalette)
    plotCol(colorlist)
  # define the aesthetic
  if(type == 'color' | type == 'colour'){
    s = scale_color_gradientn(colours=colorlist)
  } else if(type == 'fill'){
    s = scale_fill_gradientn(colours=colorlist)
  } else {
    warning("value of argument 'type' not recognized. Defaulting to 'color'.")
	s = scale_color_gradientn(colours=colorlist)
  }
  return(s)
}

cbgradient = function(...){
# creates a gradient from the color blind palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# ARGUMENTS
#   ... are arguments to pass to gradient_from_palette
# OUTPUT
#   a ggplot color scale aesthetic. 
#
# # define the palette
  cbbPalette <- c("#000000", "#D55E00", "#E69F00", "#F0E442", 
                  "#009E73", "#56B4E9", "#0072B2", "#CC79A7")
  # get the gradient aesthetic
  return(gradient_from_palette(cbbPalette, ...))
}