% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm_equation.R
\name{lm_equation}
\alias{lm_equation}
\title{Function to get least-squares regression equation as a string.}
\usage{
lm_equation(df, y, x, labels = TRUE, zero_intercept = FALSE, digits = 3)
}
\arguments{
\item{df}{Data frame.}

\item{y}{\emph{y}-variable.}

\item{x}{\emph{x}-variable.}

\item{labels}{Should the equation show variable names rather than a generic
\emph{y} and \emph{x}? Default is \code{TRUE}.}

\item{digits}{Number of digits to display for the coefficents in the 
equation. Default is \code{3}.}
}
\description{
\code{lm_equation} is used for creating a string which can be plotted with
\strong{ggplot2}.
}
\examples{
\dontrun{

# Use annotation with ggplot
ggplot(data_my1, aes(nox, bc)) + geom_point() + stat_smooth(method = lm) + 
annotate("text", x = 200, y = 45, label = lm_equation(data_my1, "bc", "nox"),
         parse = TRUE)

}

}
\seealso{
\href{http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph?answertab=votes#tab-top}{stackoverflow}
}
\author{
Ramnath Vaidyanathan with some enhancement by Stuart K. Grange
}
