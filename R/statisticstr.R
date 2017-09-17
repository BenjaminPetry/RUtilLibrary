#' Provides a string representation of a p-value
#'
#' < 0.1 = ., < 0.05 = *, < 0.01 = **, < 0.001 = ***
#'
#' @param p.value p-value
#' @return string representation of the p-value
#' @examples
#' p.value.string(0.03)
#' @export
p.value.string <- function(p.value)
{
  if (is.nan(p.value))
  {
    return("NaN")
  }
  else if (is.na(p.value))
  {
    return("NA")
  }
  else if (p.value < 0.001)
  {
    return("***")
  }
  else if (p.value < 0.01)
  {
    return("**")
  }
  else if (p.value < 0.05)
  {
    return("*")
  }
  else if (p.value < 0.1)
  {
    return(".")
  }
  return("")
}


#' Classifies the p.value into one of 4 significance classes
#'
#' ns, p < 0.05, p < 0.01, p < 0.001
#'
#' @param p.value p-value
#' @return string representation of the p-value class
#' @examples
#' p.value.level(0.03)
#' @export
p.value.level <- function(p.value)
{
  if (is.nan(p.value))
  {
    return("NaN")
  }
  else if (is.na(p.value))
  {
    return("NA")
  }
  else if (p.value < 0.001)
  {
    return("p < 0.001")
  }
  else if (p.value < 0.01)
  {
    return("p < 0.01")
  }
  else if (p.value < 0.05)
  {
    return("p < 0.05")
  }
  return("ns")
}

#' Provides a string representation of a effect size
#'
#' < 0.1 = no effect, < 0.3 = small effect, < 0.5 = medium effect, otherwise = large effect
#'
#' @param effect.size the calculated effect size, e.g. r
#' @return string representation of the effect size
#' @examples
#' effect.size.string(0.35)
#' @export
effect.size.string <- function(effect.size)
{
  if (is.nan(effect.size))
  {
    return("NaN")
  }
  else if (is.na(effect.size))
  {
    return("NA")
  }
  r <- abs(effect.size)
  if (r < 0.1)
  {
    return("no effect")
  }
  else if (r < 0.3)
  {
    return("small effect")
  }
  else if (r < 0.5)
  {
    return("medium effect")
  }
  return("large effect")
}

#' Creates the statistic string for a Kruskal-Wallis test result
#'
#' Format of the string is: H(df) = X^2, p < 0.05
#'
#' @param kruskal.result the kruskal-wallis test result
#' @return string representation of the result
#' @examples
#' kruskal.wallis.string(kruskal.test(wt ~ carb,data=mtcars))
#' @export
kruskal.wallis.string <- function(kruskal.result)
{
  p.value.str <- p.value.level(kruskal.result$p.value)
  return(paste("H(",kruskal.result$parameter,") = ",kruskal.result$statistic,", ",p.value.str,sep=""))
}

#' Converts the wilcox test's p.value into a z-value
#'
#' qnorm(wilcox.result$p.value / 2), based on Andy Field's formular
#'
#' @param wilcox.result the wilcoxon test result
#' @return z-value of the test result
#' @examples
#' wilcox.z(wilcox.test(wt ~ am, data=head(mtcars)))
#' @export
#' @import stats
wilcox.z <- function(wilcox.result)
{
  return(c(z=stats::qnorm(wilcox.result$p.value / 2)))
}

#' Provides the effect size for a wilcoxon test
#'
#' r = wilcoxon.z / sqrt(sample.size)
#'
#' @param wilcox.result the wilcoxon test result
#' @param sample.size the sample's size
#' @return r-value of the test result
#' @examples
#' wilcox.effect.size(wilcox.test(wt ~ am, data=head(mtcars)),nrow(head(mtcars)))
#' @export
wilcox.effect.size <- function(wilcox.result, sample.size)
{
  z <- wilcox.z(wilcox.result)
  r <- z / sqrt(sample.size)
  return(c(r=r))
}

#' Creates the statistic string for a Wilcoxon-Mann-Whitney test result
#'
#' Format of the string is: U = x, p < 0.05, z = -10
#'
#' @param mann.result the Wilcoxon-Mann-Whitney test result
#' @param sample.size the sample's size. If provided, the effect-size (r) will be provided rather than z
#' @return string representation of the result
#' @examples
#' mann.whitney.string(wilcox.test(wt ~ am, data=head(mtcars)))
#' @export
mann.whitney.string <- function(mann.result, sample.size=NULL)
{
  p.value.str <- p.value.level(mann.result$p.value)

  z.str = ", z = "
  z <- wilcox.z(mann.result)
  if (!is.null(sample.size))
  {
    z.str = ", r = "
    z <- wilcox.effect.size(mann.result, sample.size)
  }

  return(paste("U = ", mann.result$statistic,", ",p.value.str, z.str,z,sep=""))
}

