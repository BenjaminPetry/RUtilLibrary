env <- new.env(parent = emptyenv())
env$outputDir <- ""

#' Prepares output functions and opens a file
#'
#' Sets the output dir for images and opens a file to write strings into
#'
#' @param outputDir destination dir where all files should go to
#' @param filename file to write data into
#' @param reset set to TRUE to close all open sinks and graphic devices (default TRUE)
#' @param replace set to TRUE to replace an existing file or FALSE to append to it (default TRUE)
#' @return void
#' @examples
#' output.start("./","file.csv")
#' @export
output.start <- function(outputDir, filename, reset=TRUE, replace=TRUE)
{
  if (!endsWith(outputDir, "/"))
  {
    outputDir <- paste(outputDir,"/",sep="")
  }
  env$outputDir <- outputDir
  if (reset)
  {
    output.reset()
  }
  filename <- paste(env$outputDir, filename, sep="")
  if (file.exists(filename))
  {
    file.remove(filename)
  }
  sink(filename, append = !replace, split = TRUE)
}

#' Stops writing into files and in dir
#'
#' Closes the sink and all graphic devices
#'
#' @return void
#' @examples
#' output.stop()
#' @export
output.stop <- function()
{
  sink()
  output.reset
  env$outputDir <- ""
}

#' Writes a header into the output file
#'
#' Writes a "### [HEADER TEXT]" into the output file
#'
#' @param text text to write as header
#' @return void
#' @examples
#' output.start("./","file.csv")
#' output.header("Header Text")
#' output.stop()
#' @export
output.header <- function(text)
{
  cat("### ")
  output.writeLine(text)
}

#' Writes a line into the output file
#'
#' Writes a "[Text]" into the output file
#'
#' @param text text to write inside the file
#' @return void
#' @examples
#' output.start("./","file.csv")
#' output.writeLine("text")
#' output.stop()
#' @export
output.writeLine <- function(text)
{
  cat(text)
  cat("\n\n")
}

#' Writes a table as csv into the current file
#'
#' The table will be formated in csv format (',' as separator) and written into the current output file.
#'
#' @param data data to write as csv
#' @return void
#' @examples
#' output.start("./","file.csv")
#' output.csv(mtcars)
#' output.stop()
#' @export
output.csv <- function(data)
{
  write.csv(data)
  flush.console()
}


#' Closes all open sinks and graphic devices
#'
#' Closes the sinks that are still open and closes all graphic devices
#'
#' @return void
#' @examples
#' output.reset()
#' @import grDevices
#' @export
output.reset <- function()
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


#' Stores a ggplot
#'
#' Creates a DinA4 document (on default) and places the plot on the whole document.
#'
#' @param p the ggplot-plot or a list of ggplots. Filename will be adjusted with increasing numbers, except for pdf files. These will contain one page per plot.
#' @param filename filename of the file WITH extension, which describes the format. Currently supported are "eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg". Files are put into the output directory
#' @param dpi the resultion of the picture (important for raster graphics)
#' @param paper default paper (currently specified by width and height) - only important for pdfs (see pdf(paper=?))
#' @param width width of the ploting area in inch (width * ppi defines the size of the picture formats)
#' @param height height of the ploting area in inch (as for width)
#' @param print.default.graphicdevice when true, the plot will be plotted on the default graphics device as well
#' @return void
#' @examples
#' output.plot(statistics.plot.frequency(mtcars, c("cyl","carb")),"test.pdf")
#' @import grDevices
#' @import ggplot2
#' @import graphics
#' @export
# For saving a ggplot into a pdf-file and also provide output on the default graphic device.
# Default size: A4
output.plot <- function(p, filename, dpi = 300, paper="special", width=11.69, height = 8.27, print.default.graphicdevice = TRUE)
{
  filename <- paste(env$outputDir, filename, sep="")

  # extract file extension
  filename.split <- strsplit(filename,"\\.")[[1]]
  format <- filename.split[length(filename.split)]
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
    output.plot.single(pl, fname, format, dpi, paper, width, height)
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
      output.plot.single(p, "", "", dpi, paper, width, height)
    }
    result <- lapply(plot.list, FUN = fun.console)
  }
}

#' @keywords internal
#' @import graphics
output.plot.single <- function(p, filename="", format="", dpi = 300, paper="a4r", width=11.69, height = 8.27)
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
