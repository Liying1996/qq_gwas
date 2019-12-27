#' @title Draw QQ-plot for multiple-group GWAS data.
#' @description  This package use ggplot2 to do multiple GWAS Q-Q plots in one figure.
#' @author Ying Li
#' @param data
#' required. a dataframe. The first column is p.value, and second column is goup.
#' @param col1
#' optional. difference group colors
#' @param col2
#' optional. color of y=x
#' @return a ggplot
#' @examples
#' ggwas(data, col1 = c('#086FA1', '#00B64F'), col2 = 'black')
#' @export qq_gwas
#'

library(ggplot2)

new_func <- function(p){
  return(sort(-log10(p)))
}

qq_gwas <- function(data, col1 = NULL, col2='black') {
  p <- ggplot(data) + stat_qq(aes(sample= -log10(data[,1]),color=data[,2]),cex=0.8,quantiles = NULL,distribution = new_func) +
    labs(x = "Expected -log10(p)", y = "Observed -log10(p)") + coord_cartesian(ylim = c(0, max(-log10(data[,1])))) +
    theme(plot.title = element_text(hjust = 0.5))  + geom_abline(slope=1, intercept=0) +
    guides(color = guide_legend(title = NULL, override.aes = list(size=3,linetype=0))) +
    theme(aspect.ratio = 1) + theme_bw()
  if (!is.null(col1)){
    p <- p + scale_colour_manual(values = col1)
  }

  return(p)
}
