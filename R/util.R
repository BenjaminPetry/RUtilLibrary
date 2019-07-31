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

#' Removes columns by name from a data frame
#'
#' Removes specified columns by their name from a data frame
#'
#' @param data data frame
#' @param columns names of columns to remove
#' @return the data frame without the specified columns
#' @examples
#' test = remove.columns(mtcars, c("cyl","carb"))
#' @export
remove.columns <- function(data, columns)
{
  return(data[,!(names(data) %in% columns)])
}

#' Selects columns by name from a data frame
#'
#' Selects specified columns by their name from a data frame
#'
#' @param data data frame
#' @param columns names of columns to select
#' @return the data frame with only the specified columns
#' @examples
#' test = select.columns(mtcars, c("cyl","carb"))
#' @export
select.columns <- function(data, columns)
{
  return(data[,(names(data) %in% columns)])
}
