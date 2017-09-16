#' Provides a string representation of a p-value
#'
#' < 0.1 = ., < 0.05 = *, < 0.01 = **, < 0.001 = ***
#'
#' @param p.value p-value
#' @return string representation of the p-value
#' @examples
#' p.value.string(0.95)
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

#' Stores a ggplot
#'
#' Creates a DinA4 document (on default) and places the plot on the whole document.
#'
#' @param p the ggplot-plot or a list of ggplots. Filename will be adjusted with increasing numbers, except for pdf files. These will contain one page per plot.
#' @param filename filename of the file WITH extension, which describes the format. Currently supported are "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg"
#' @param dpi the resultion of the picture (important for raster graphics)
#' @param paper default paper - only important for pdf (see pdf(paper=?))
#' @param width width of the ploting area in inch (width * ppi defines the size of the picture formats)
#' @param height height of the ploting area in inch (as for width)
#' @param print.default.graphicdevice when true, the plot will be plotted on the default graphics device as well
#' @return void
#' @examples
#' save.plot(statistics.plot.frequency(mtcars, c("cyl","carb")),"test.pdf")
#' @import grDevices
#' @import ggplot2
#' @import graphics
#' @export
# For saving a ggplot into a pdf-file and also provide output on the default graphic device.
# Default size: A4
save.plot <- function(p, filename, dpi = 300, paper="a4r", width=11.69, height = 8.27, print.default.graphicdevice = TRUE)
{
  # extract file extension
  filename.split <- strsplit(filename,"\\.")[[1]]
  format <- filename.split[-1]
  filename <- paste(filename.split[-length(filename.split)],collapse=".")
  if (!(format %in% c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg")))
  {
    stop("Not supported format ",format)
  }

  # make plot a list if it is not
  plot.list <- p
  if ("ggplot" %in% class(p))
  {
    plot.list <- list(p)
  }

  fname <- paste(filename,".",format,sep="")
  is.series <- (length(plot.list) > 1)
  if (format == "pdf")
  {
    grDevices::pdf(fname, paper=paper, width=width, height = height)
    fname <- ""
    is.series <- FALSE
  }

  fun <- function(index)
  {
    pl <- plot.list[[index]]
    if (is.series)
    {
      fname <- paste(filename,"-",index,".",format,sep="")
      print(fname)
    }
    save.plot.single(pl, fname, format, dpi, paper, width, height)
  }
  result <- lapply(1:length(plot.list), FUN = fun)

  if (format == "pdf")
  {
    grDevices::dev.off()
  }

  if(print.default.graphicdevice)
  {
    fun.console <- function(p)
    {
      save.plot.single(p, "", "", dpi, paper, width, height)
    }
    result <- lapply(plot.list, FUN = fun.console)
  }
}

#' @keywords internal
#' @import graphics
save.plot.single <- function(p, filename="", format="", dpi = 300, paper="a4r", width=11.69, height = 8.27)
{
  if (filename == "")
  {
    graphics::plot(p)
  }
  else if (format == "pdf")
  {
    grDevices::pdf(filename, paper=paper, width=width, height = height)
    graphics::plot(p)
    grDevices::dev.off()
  }
  else
  {
    ggplot2::ggsave(filename = filename, p, width = width, height = height, dpi = dpi, units = "in", device=format)
  }
}

#' Closes all open sinks and graphic devices
#'
#' Closes the sinks that are still open and closes all graphic devices
#'
#' @return void
#' @examples
#' reset.output()
#' @import grDevices
#' @export
reset.output <- function()
{
  while (grDevices::dev.cur() > 1)
  {
    grDevices::dev.off()
  }
  while (sink.number() > 0)
  {
    sink()
  }
}


#' Copies content from columns of one data.frame to another data.frame based column matching (conditional copy)
#'
#' Creates or replaces column content of the destination data frame (x) with data from (y) where the rows have matching content in column (by)
#'
#' @param x destination data frame
#' @param y source data frame
#' @param columns.copy the columns from y that should be copied to x.
#' @param columns.copy.x the names of the new columns in x (default: columns.copy)
#' @param columns.copy.y the names of the columns in y (default: columns.copy)
#' @param by that matching column of x and y
#' @param by.x the name of the matching column in x (default: by)
#' @param by.y the name of the matching column in y (default: by)
#' @return the data frame x with content from y in the columns (columns.copy) where by.x = by.y
#' @examples
#' test = data.frame(carb=c(1,2,3,4,5),desc=c('a','b','c','d','e'))
#' copy.conditional.column(mtcars, test, columns.copy = "desc", by = "carb")
#' @export
copy.conditional.column <- function(x, y, columns.copy = names(y), columns.copy.x = columns.copy, columns.copy.y = columns.copy, by = intersect(names(x), names(y))[1], by.x = by, by.y = by)
{
  if (length(columns.copy.x) != length(columns.copy.y))
  {
    stop("length of columns.copy x[",length(columns.copy.x),"] and y[",length(columns.copy.y),"] must be equal.")
  }
  if (length(by.x) != 1 || length(by.y) != 1)
  {
    stop("length of by.x[",length(by.x),"] and by.y[",length(by.x),"] must be 1.")
  }

  merge.fun = function(xx, colY) (y[y[by.y] == xx[by.x],][1,colY])

  for (i in 1:length(columns.copy.y))
  {
    col.x <- columns.copy.x[i]
    col.y <- columns.copy.y[i]
    x[col.x] <- apply(x, 1, FUN = merge.fun, col.y)
  }
  return(x)
}
