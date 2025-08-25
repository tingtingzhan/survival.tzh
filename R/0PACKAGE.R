

#' @import patchwork
# @importFrom patchwork ggplot_add.ggplot
'_PACKAGE'
# ?patchwork:::ggplot_add.ggplot # not exported



# name clash
# ?patchwork::area
# ?MASS::area


if (FALSE) {
  
  library(ggplot2)
  library(patchwork)
  methods(class = 'ggplot')
  methods(class = 'gg')
  
  ggplot2:::`+.gg`
  ggplot2:::add_ggplot
  ggplot2::ggplot_add
  
  patchwork:::ggplot_add.ggplot # not exported
  patchwork:::get_patches
  patchwork:::add_patches
  
  methods(generic.function = 'ggplot_add') |>
    attr(which = 'info', exact = TRUE)
  
}