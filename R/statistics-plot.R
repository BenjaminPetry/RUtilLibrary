#' Creates histograms of the data
#'
#' A list of histogram ggplot-plots
#'
#' @param data the data as a data.frame
#' @param column.dependent the name of the column that contains the dependent variable (x-axis)
#' @param columns.independent a vector of columns that contain the independent variables
#' @param column.fill column for a stacked histogram
#' @param bin.width width of each bin
#' @param bins how many bins you want to have (will be ignored if bin.width is set)
#' @param title true to get a title printed on each plot with the current condition
#' @return a data frame containing basic statistic values
#' @seealso \code{\link{statistics.descriptive.parametric}}
#' @seealso \code{\link{statistics.descriptive.nonparametric}}
#' @seealso \code{\link{statistics.descriptive.frequency}}
#' @examples
#' statistics.histogram(mtcars, "carb", c("cyl"), column.fill = "gear", bins = 5)
#' statistics.histogram(mtcars, "carb")
#' @import ggplot2
#' @import RColorBrewer
#' @export
statistics.histogram <- function(data, column.dependent, columns.independent=c(), column.fill = NULL, bin.width = NULL, bins = 30, title = TRUE)
{
  column.fill.factor <- "NULL"
  values <- c()
  if (!is.null(column.fill))
  {
    column.fill.factor <- paste("factor(",column.fill,")",sep="")
    values <- unique(as.factor(data[,column.fill]))
    colors <- RColorBrewer::brewer.pal(length(values), "Set1")
    names(colors) <- levels(values)
    colorScale <- ggplot2::scale_fill_manual(name = column.fill,values = colors)
  }
  fun <- function(xx, iv)
  {
    p <- ggplot2::ggplot(xx, aes_string(iv, fill = column.fill.factor)) + ggplot2::geom_histogram(bins = bins, binwidth = bin.width)
    if (!is.null(column.fill))
    {
      p <- p + colorScale
    }
    if (is.character(title))
    {
      p <- p + ggplot2::ggtitle(title)
    }
    else if (title && dim(xx)[1] > 0)
    {
        if (length(columns.independent)==0)
        {
          p <- p + ggplot2::ggtitle(paste("Distribution ",column.dependent,sep=""))
        }
        else
        {
          tmp <- paste(columns.independent,xx[1,columns.independent],sep=":")
          title.string <- paste(tmp,collapse=", ")
          p <- p + ggplot2::ggtitle(title.string)
        }
    }
    return(p)
  }
  if (length(columns.independent)==0)
  {
    return(fun(data, column.dependent))
  }
  # Apply function to each sub set. The sub sets are defined by the independent columns
  result <- plyr::dlply(data, columns.independent, .drop=TRUE,
                        .fun = fun,
                        column.dependent)

  return(result)
}



#' @keywords internal
statistics.plot.prepare <- function(data, column.dependent, columns.independent, xlab = NULL, ylab = NULL, legend.lab = NULL, xlim = NULL, ylim = NULL, title = NULL)
{
  column.fill = NULL
  column.xaxis = 1
  if (length(columns.independent) > 0)
  {
    column.xaxis <- paste("factor(",columns.independent[1],")",sep="")
    if (is.null(xlab))
    {
      xlab <- columns.independent[1]
    }
  }
  if (length(columns.independent) > 1)
  {
    column.fill <- paste("factor(",columns.independent[2],")",sep="")
    if (is.null(legend.lab))
    {
      legend.lab <- columns.independent[2]
    }
  }

  p <- ggplot2::ggplot(data, aes_string(x = column.xaxis, y = column.dependent, fill = column.fill))

  # title
  if (!is.null(title))
  {
    p <- p + ggplot2::ggtitle(title)
  }

  # limites
  if (!is.null(xlim))
  {
    p <- p + ggplot2::xlim(xlim)
  }
  if (!is.null(ylim))
  {
    p <- p + ggplot2::ylim(ylim)
  }
  # labels
  if (!is.null(xlab))
  {
    p <- p + ggplot2::xlab(xlab)
  }
  if (!is.null(legend.lab))
  {
    p <- p + ggplot2::labs(fill=legend.lab)
  }
  if (!is.null(ylab))
  {
    p <- p + ggplot2::ylab(ylab)
  }
  return(p)
}


#' Creates descriptive plot of parametric data
#'
#' Creates first a parametric descriptive statistic and plots it into a ggplot-plot
#'
#' @param data the data as a data.frame
#' @param column.dependent the name of the column that contains the dependent variable (x-axis)
#' @param columns.independent a vector of columns that contain the independent variables
#' @param data.type the data-type. Allowed values are 'parametric', 'nonparametric', and 'frequency'
#' @param xlab label for x-axis (columns.independent[1] if provided)
#' @param ylab label for y-axis (column.dependent)
#' @param legend.lab label for legend (columns.independent[2] if provided)
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param title title for the graph
#' @param position the arrangement of bars for parametric or frequency bar plot (dodge on default, use "stack" for stacked version)
#' @param conf.interval the confidence interval desired (default 95\%)
#' @param conf.accurate TRUE to use the accurate confidence interval. This means, that the CI.factor is calculated using the sample size - 1. Otherwise 10.000 is assumed as sample size.
#' @return a ggplot-plot
#' @examples
#' statistics.plot(mtcars, "wt",c("cyl","carb"))
#' @import ggplot2
#' @export
statistics.plot <- function(data, column.dependent, columns.independent, data.type = "parametric", xlab = NULL, ylab = NULL, legend.lab = NULL, xlim = NULL, ylim = NULL, title = NULL, position = position_dodge(), conf.interval = .95, conf.accurate = TRUE)
{
  if (data.type == "parametric")
  {
    return(statistics.plot.parametric(data, column.dependent, columns.independent, xlab, ylab, legend.lab, xlim, ylim, title, position=position, conf.interval, conf.accurate))
  }
  else if (data.type == "nonparametric")
  {
    return(statistics.plot.nonparametric(data, column.dependent, columns.independent, xlab, ylab, legend.lab, xlim, ylim, title))
  }
  else if (data.type == "frequency")
  {
    return(statistics.plot.frequency(data, columns.independent, FALSE, position=position))
  }
  stop("Data type '",data.type,"' is not supported!")
}

#' Creates descriptive plot of frequency data
#'
#' Creates a barchart as ggplot-plot
#'
#' @param data the data as a data.frame
#' @param columns.independent a vector of columns that contain the independent variables
#' @param drop should combinations of variables that do not appear in the columns.independent be preserved or dropped (default = FALSE)
#' @param xlab label for x-axis (columns.independent[1] if provided)
#' @param ylab label for y-axis (frequency)
#' @param legend.lab label for legend (columns.independent[2] if provided)
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param title title for the graph
#' @param position the arrangement of bars in the bar plot (dodge on default, use "stack" for stacked version)
#' @return a ggplot-plot
#' @examples
#' statistics.plot.frequency(mtcars, c("cyl","carb"))
#' @import ggplot2
#' @export
statistics.plot.frequency <- function(data, columns.independent, drop = FALSE, xlab = NULL, ylab = NULL, legend.lab = NULL, xlim = NULL, ylim = NULL, title = NULL, position=position_dodge())
{
  statistic <- statistics.descriptive.frequency(data, columns.independent, drop = drop)
  if (is.null(ylab))
  {
    ylab <- "frequency"
  }

  p <- statistics.plot.prepare(statistic, "N", columns.independent, xlab, ylab, legend.lab, xlim, ylim, title)
  p <- p + ggplot2::geom_bar(position=position, stat="identity")
  return(p)
}

#' Creates descriptive plot of parametric data
#'
#' Creates first a parametric descriptive statistic and plots it into a ggplot-plot
#'
#' @param data the data as a data.frame
#' @param column.dependent the name of the column that contains the dependent variable (x-axis)
#' @param columns.independent a vector of columns that contain the independent variables
#' @param xlab label for x-axis (columns.independent[1] if provided)
#' @param ylab label for y-axis (column.dependent)
#' @param legend.lab label for legend (columns.independent[2] if provided)
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param title title for the graph
#' @param position the arrangement of bars in the bar plot (dodge on default, use "stack" for stacked version)
#' @param conf.interval the confidence interval desired (default 95\%)
#' @param conf.accurate TRUE to use the accurate confidence interval. This means, that the CI.factor is calculated using the sample size - 1. Otherwise 10.000 is assumed as sample size.
#' @return a ggplot-plot
#' @examples
#' statistics.plot.parametric(mtcars, "wt",c("cyl","carb"))
#' @import ggplot2
#' @export
statistics.plot.parametric <- function(data, column.dependent, columns.independent, xlab = NULL, ylab = NULL, legend.lab = NULL, xlim = NULL, ylim = NULL, title = NULL, position = position_dodge(), conf.interval = .95, conf.accurate = TRUE)
{
  statistic <- statistics.descriptive.parametric(data, column.dependent, columns.independent, conf.interval, conf.accurate)
  if (is.null(ylab))
  {
    ylab <- column.dependent
  }

  p <- statistics.plot.prepare(statistic, "Mean", columns.independent, xlab, ylab, legend.lab, xlim, ylim, title)
  p <- p + ggplot2::geom_bar(position=position, stat="identity")
  p <- p + ggplot2::geom_errorbar(aes_string(ymin="CI.Lower", ymax="CI.Upper"), width=.2, position=ggplot2::position_dodge(.9))
  return(p)
}

#' Creates descriptive plot of non parametric data
#'
#' Creates a boxplot as ggplot-plot
#'
#' @param data the data as a data.frame
#' @param column.dependent the name of the column that contains the dependent variable (x-axis)
#' @param columns.independent a vector of columns that contain the independent variables
#' @param xlab label for x-axis (columns.independent[1] if provided)
#' @param ylab label for y-axis (column.dependent)
#' @param legend.lab label for legend (columns.independent[2] if provided)
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param title title for the graph
#' @return a ggplot-plot
#' @examples
#' statistics.plot.nonparametric(mtcars, "mpg",c("cyl","carb"))
#' @import ggplot2
#' @export
statistics.plot.nonparametric <- function(data, column.dependent, columns.independent, xlab = NULL, ylab = NULL, legend.lab = NULL, xlim = NULL, ylim = NULL, title = NULL)
{
  p <- statistics.plot.prepare(data, column.dependent, columns.independent, xlab, ylab, legend.lab, xlim, ylim, title)
  p <- p + ggplot2::stat_boxplot(geom = "errorbar")
  p <- p + ggplot2::geom_boxplot()
  return(p)
}

