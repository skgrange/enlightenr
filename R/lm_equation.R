#' Function to get least squares regression equation as a string. 
#' 
#' \code{lm_equation} is used for creating a string which can be plotted with
#' \strong{ggplot2}. 
#' 
#' @author Ramnath Vaidyanathan with some enhancement by Stuart K. Grange
#' 
#' @param df Data frame. 
#' 
#' @param y \emph{y}-variable.
#' 
#' @param x \emph{x}-variable.
#' 
#' @param labels Should the equation show variable names rather than a generic
#' \emph{y} and \emph{x}? Default is \code{TRUE}. 
#' 
#' @param digits Number of digits to display for the coefficents in the 
#' equation. Default is \code{3}. 
#' 
#' @seealso \href{http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph?answertab=votes#tab-top}{stackoverflow}
#' 
#' @import stringr
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
lm_equation <- function(df, y, x, labels = TRUE, digits = 3) {
  
  formula <- stringr::str_c(y, " ~ ", x)
  formula <- as.formula(formula)
  
  m <- lm(formula, data = df)
  
  eq <- substitute(
    italic(y) == italic(x) %.% b + a * "," ~ ~ italic(R) ^ 2 ~ "=" ~ r2, 
    list(a = format(coef(m)[1], digits = digits), 
         b = format(coef(m)[2], digits = digits), 
         r2 = format(summary(m)$r.squared, digits = digits)))
  # italic(y) == a + b %.% italic(x) * "," ~ ~ italic(R) ^ 2 ~ "=" ~ r2, 
  
  # To character
  eq <- as.character(as.expression(eq))
  
  # A switch for when there is a negative intercept
  eq <- str_replace(eq, ' \\+ \"-', ' - \"')
  
  # Parsing vectors to expression is hard, therefore do some replacing
  if (labels) {
    
    # Get vector
    labels <- c(y, x)
    
    # Do some formatting
    labels <- ifelse(labels == "pm10", "PM[10]", labels)
    labels <- ifelse(labels %in% c("pm25", "pm2.5"), "PM[2.5]", labels)
    labels <- ifelse(labels %in% c("bc"), "BC", labels)
    
    # Replace y and x
    eq <- str_replace(eq, "italic\\(y\\)", labels[1])
    eq <- str_replace(eq, "italic\\(x\\)", labels[2])
    # eq <- str_replace(eq, "italic\\(y\\)", str_c("italic(", labels[1], ")"))
    # eq <- str_replace(eq, "italic\\(x\\)", str_c("italic(", labels[2], ")"))
    
  }
  
  # Return
  eq
  
}
