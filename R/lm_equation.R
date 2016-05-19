#' Function to get least squares regression equation as a string. 
#' 
#' \code{lm_equation} is used for creating a string which can be plotted with
#' \strong{ggplot2}. 
#' 
#' @author Ramnath Vaidyanathan
#' 
#' @param df Data frame. 
#' @param y \emph{y}-variable.
#' @param x \emph{x}-variable.
#' 
#' @seealso \href{http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph?answertab=votes#tab-top}{stackoverflow.com}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Use annotation with ggplot
#' ggplot(data_my1, aes(nox, bc)) + geom_point() + stat_smooth(method = lm) + 
#' annotate("text", x = 200, y = 45, label = lm_equation(data_my1, "bc", "nox"),
#'          parse = TRUE)
#' 
#' }
#' 
#' @export
lm_equation <- function(df, y, x) {
  
  formula <- stringr::str_c(y, " ~ ", x)
  formula <- as.formula(formula)
  
  m <- lm(formula, data = df)
  
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  
  as.character(as.expression(eq))
  
}
