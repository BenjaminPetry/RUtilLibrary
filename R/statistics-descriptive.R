#' Calculates the descriptive statistic of parametric data
#'
#' Returns a data.frame. The data.frame data depends on the type of data.
#'
#' @param data the data as a data.frame
#' @param column.dependent the name of the column that contains the dependent variable | will be ignored when data.type == 'frequency'
#' @param columns.independent a vector of columns that contain the independent variables
#' @param data.type the data-type. Allowed values are 'parametric', 'nonparametric', and 'frequency'
#' @param conf.interval the confidence interval desired (default 95\%) | only used when data.type == 'parametric'
#' @param conf.accurate TRUE to use the accurate confidence interval. This means, that the CI.factor is calculated using the sample size - 1. Otherwise 10.000 is assumed as sample size.  | only used when data.type == 'parametric'
#' @return a data frame containing basic statistic values
#' @seealso \code{\link{statistics.descriptive.parametric}}
#' @seealso \code{\link{statistics.descriptive.nonparametric}}
#' @seealso \code{\link{statistics.descriptive.frequency}}
#' @examples
#' statistics.descriptive(mtcars, "carb", c("cyl"), data.type = 'nonparametric')
#' @import plyr
#' @export
statistics.descriptive <- function(data, column.dependent, columns.independent, data.type = "parametric", conf.interval = .95, conf.accurate = TRUE)
{
  if (data.type == "parametric")
  {
    return(statistics.descriptive.parametric(data, column.dependent, columns.independent, conf.interval, conf.accurate))
  }
  else if (data.type == "nonparametric")
  {
    return(statistics.descriptive.parametric(data, column.dependent, columns.independent))
  }
  else if (data.type == "frequency")
  {
    return(statistics.descriptive.frequency(data, columns.independent))
  }
  stop("Data type '",data.type,"' is not supported!")
}

#' Calculates the descriptive statistic of frequency data
#'
#' Returns a data.frame with the following values
#' \itemize{
#' \item{N - the frequency}
#' }
#'
#' @param data the data as a data.frame
#' @param columns.independent a vector of columns that contain the independent variables
#' @examples
#' statistics.descriptive.frequency(mtcars, c("cyl","carb"))
#' @export
statistics.descriptive.frequency <- function(data, columns.independent)
{
  fun <- function(xx)
  {
    n <- dim(xx)[1]
    return(c(N = n))
  }

  # Apply function to each sub set. The sub sets are defined by the independent columns
  result <- plyr::ddply(data, columns.independent, .drop=TRUE, .fun = fun)
  return (result)
}

#' Calculates the descriptive statistic of non-parametric data
#'
#' Returns a data.frame with the following values
#' \itemize{
#' \item{N - the sample size}
#' \item{Sum - the sample size}
#' \item{Percentiles.25 - 25th percentiles}
#' \item{Percentiles.50 - 50th (median) percentiles}
#' \item{Percentiles.50 - 75th percentiles}
#' \item{Min - the sample's minimum}
#' \item{Max - the sample's maximum}
#' }
#'
#' @param data the data as a data.frame
#' @param column.dependent the name of the column that contains the dependent variable
#' @param columns.independent a vector of columns that contain the independent variables
#' @examples
#' statistics.descriptive.nonparametric(mtcars, "carb", c("cyl"))
#' @export
statistics.descriptive.nonparametric <- function(data, column.dependent, columns.independent)
{
  fun <- function(xx, col)
  {
    values <- xx[[col]]
    n <- sum(!is.na(values))
    sumV <- sum(values, na.rm=FALSE)
    median = median(values, na.rm = FALSE)
    quantileV <- unname(stats::quantile(values, c(0.25, 0.75)))
    return(c(N = n, Sum = sumV, Percentiles.25 = quantileV[1], Percentiles.50 = median, Percentiles.75 = quantileV[2], Min = min(values), Max = max(values)))
  }

  # Apply function to each sub set. The sub sets are defined by the independent columns
  result <- plyr::ddply(data, columns.independent, .drop=TRUE,
                        .fun = fun,
                        column.dependent
  )
  return (result)
}


#' Calculates the descriptive statistic of parametric data
#'
#' Returns a data.frame with the following values
#' \itemize{
#' \item{N - the sample size}
#' \item{Sum - sum of all sample's values}
#' \item{Mean - the sample's mean}
#' \item{SS - the squared sum of errors}
#' \item{Var - the sample's variance}
#' \item{SD - the sample's standard deviation}
#' \item{SE - the estimated population standard error}
#' \item{CI.Lower - lower confidence interval}
#' \item{CI.Upper - upper confidence interval}
#' \item{CI.Factor - confidence interval factor}
#' \item{CI.Range - confidence interval range (CI.Factor * SE)}
#' \item{Min - the sample's minimum}
#' \item{Max - the sample's maximum}
#' }
#'
#' @param data the data as a data.frame
#' @param column.dependent the name of the column that contains the dependent variable
#' @param columns.independent a vector of columns that contain the independent variables
#' @param conf.interval the confidence interval desired (default 95\%)
#' @param conf.accurate TRUE to use the accurate confidence interval. This means, that the CI.factor is calculated using the sample size - 1. Otherwise 10.000 is assumed as sample size.
#' @return a data frame containing basic statistic values
#' @examples
#' statistics.descriptive.parametric(mtcars, "wt", c("cyl"),
#'                                   conf.interval = .95,
#'                                   conf.accurate = TRUE)
#' @import plyr
#' @export
statistics.descriptive.parametric <- function(data, column.dependent, columns.independent, conf.interval = .95, conf.accurate = TRUE)
{
  statisticFun <- function(xx, col)
  {
    values <- xx[[col]]
    n <- sum(!is.na(values))
    sumV <- sum(values, na.rm=FALSE)
    meanV <- mean(values, na.rm=FALSE)
    if (n == 1)
    {
      return(res <- c(N = n, Sum = sumV, Mean = meanV, SS = NA, Var = NA, SD = NA, SE = NA, CI.Lower = meanV, CI.Upper = meanV,CI.Factor = NA, CI.Range = 0, Min = min(values), Max = max(values)))
    }
    variance <- stats::var(values, na.rm = FALSE)
    standardDeviation <- stats::sd(values, na.rm=FALSE)
    standardError <- standardDeviation / sqrt(n)
    cifactorN <- ifelse(conf.accurate, n - 1, 10000)
    cifactor <- stats::qt(conf.interval / 2 + 0.5, cifactorN)
    confidenceInterval <- cifactor * standardError
    res <- c(N = n,
             Sum = sumV,
             Mean = meanV,
             SS = variance*(n - 1),
             Var = variance,
             SD = standardDeviation,
             SE = standardError,
             CI.Lower = meanV - confidenceInterval,
             CI.Upper = meanV + confidenceInterval,
             CI.Factor = cifactor,
             CI.Range = confidenceInterval,
             Min = min(values),
             Max = max(values)
    )
    return(res)
  }

  # Apply function to each sub set. The sub sets are defined by the independent columns
  result <- plyr::ddply(data, columns.independent, .drop=TRUE,
                        .fun = statisticFun,
                        column.dependent
  )
  return (result)
}

