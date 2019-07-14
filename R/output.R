#' Setting up an environment to store the output information
#' @keywords internal
envOutput <- new.env(parent = emptyenv())
envOutput$outputDir <- ""

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

#' Prepares output functions and sets a directory where all files will be relative to
#'
#' Sets the output dir for images
#'
#' @param outputDir destination dir where all files should go to
#' @param reset set to TRUE to close all open sinks and graphic devices (default TRUE)
#' @return void
#' @examples
#' output.init("./results/")
#' @export
output.init <- function(outputDir, reset=TRUE)
{
  if (!endsWith(outputDir, "/"))
  {
    outputDir <- paste(outputDir,"/",sep="")
  }
  envOutput$outputDir <- outputDir
  if (reset)
  {
    output.reset()
  }
}

#' Closes all sinks and graphic devices and resets the output directory
#'
#' Closes the sink and all graphic devices
#'
#' @return void
#' @examples
#' output.init("./")
#' output.dispose()
#' @export
output.dispose <- function()
{
  output.reset()
  envOutput$outputDir <- ""
}

#' Opens a file for writing
#'
#' Open a file to write strings into, relativ to the output directory
#'
#' @param filename file to write data into
#' @param replace set to TRUE to replace an existing file or FALSE to append to it (default TRUE)
#' @return void
#' @examples
#' output.init("./")
#' # Starting a CSV file
#' output.open("file.csv")
#' output.csv(mtcars)
#' output.close()
#' # Starting a Markdown file
#' output.open("file.md")
#' output.header("First header",1)
#' output.writeLine("Hello World")
#' output.close()
#' @export
output.open <- function(filename, replace=TRUE)
{
  filename <- paste(envOutput$outputDir, filename, sep="")
  if (file.exists(filename) && replace)
  {
    file.remove(filename)
  }
  sink(filename, append = !replace, split = TRUE)
}

#' Closes the open file
#'
#' Closes the open file.
#'
#' @return void
#' @examples
#' output.init("./")
#' output.open("file.md")
#' output.close()
#' @export
output.close <- function()
{
  flush.console()
  sink()
}

#' Writes a header into the output file
#'
#' Writes a "# Header" into the output file
#'
#' @param text text to write as header
#' @param level level of the header
#' @return void
#' @examples
#' output.init("./")
#' output.open("file.md")
#' output.header("First header",1)
#' output.writeLine("Hello World")
#' output.close()
#' @export
output.header <- function(text,level=1)
{
  cat(paste(replicate(level, "#"), collapse = ""))
  cat(" ")
  cat(text)
  cat("\n\n")
}

#' Writes a line into the output file
#'
#' Writes a text into the output file
#'
#' @param text text to write inside the file
#' @return void
#' @examples
#' output.init("./")
#' output.open("file.md")
#' output.header("First header",1)
#' output.writeLine("Hello World")
#' output.close()
#' @export
output.writeLine <- function(text)
{
  cat(text)
  cat("\n")
}

#' Writes a table as csv into the current file (or a new file, if a filename is given)
#'
#' The table will be formated in csv format (',' as separator) and written into the current output file.
#'
#' @param data data to write as csv
#' @param filename if given, the csv data will be written into a new file (default NULL)
#' @return void
#' @examples
#' output.init("./")
#' output.csv(mtcars, "file.csv")
#' @export
output.csv <- function(data, filename=NULL)
{
  if (!is.null(filename))
  {
    output.open(filename)
  }
  write.csv(data)
  flush.console()
  if (!is.null(filename))
  {
    output.close()
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
#' @param attachToMarkdown if a file is open the name of filename is written into the file in markdown style.
#' @return void
#' @examples
#' output.init("./")
#' output.plot(statistics.plot.frequency(mtcars, c("cyl","carb")),"test.pdf")
#' @import grDevices
#' @import ggplot2
#' @import graphics
#' @export
# For saving a ggplot into a pdf-file and also provide output on the default graphic device.
# Default size: A4
output.plot <- function(p, filename, dpi = 300, paper="special", width=11.69, height = 8.27, print.default.graphicdevice = TRUE, attachToMarkdown=FALSE)
{
  filename <- paste(envOutput$outputDir, filename, sep="")

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
output.plot.single <- function(p, filename="", format="", dpi = 300, paper="a4r", width=11.69, height = 8.27, attachToMarkdown=FALSE)
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
  if (sink.number() > 0 && attachToMarkdown)
  {
    output.writeLine(paste("![",filename,"](",filename,")"))
  }
}
