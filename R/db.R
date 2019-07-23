#' Setting up an environment to store the database login information
#' @keywords internal
env <- new.env(parent = emptyenv())
env$dbLoginData.host <- ""
env$dbLoginData.user <- ""
env$dbLoginData.password <- ""
env$dbLoginData.dbname <- ""

#' Initializes the database
#'
#' Stores login information for later database connections.
#'
#' @param host the IP address of the database
#' @param user the username
#' @param password the user's password
#' @param dbname the database's name you want to connect to
#' @return void
#' @examples
#' \dontrun{
#' db.init(host = '127.0.0.1',
#'         user = 'myUsername',
#'         password = 'myPassword',
#'         dbname='myDatabaseName')
#' }
#' @import RMySQL
#' @export
db.init <- function(host, user, password, dbname)
{
  env$dbLoginData.user <- user
  env$dbLoginData.password <- password
  env$dbLoginData.host <- host
  env$dbLoginData.dbname <- dbname
}

#' Executes a SELECT-query
#'
#' Executes a SELECT-query and returns the result set as a data.frame.
#'
#' @section Important: db.init has to be called first!
#' @param queryString the SELECT-query string
#' @param autoConvertTimestamps if TRUE, fields like "...timestamp...", "...start..." and "...end..." will be automatically converted into POSIXct timestamps
#' @return the result set as data.frame
#' @seealso \code{\link{db.init}}
#' @examples
#' \dontrun{
#' participants = db.query("SELECT * FROM participants")
#' }
#' @import RMySQL
#' @export
db.query <- function(queryString,autoConvertTimestamps=TRUE)
{
  con <- RMySQL::dbConnect(RMySQL::MySQL(),
                   user = env$dbLoginData.user,
                   password = env$dbLoginData.password,
                   host = env$dbLoginData.host,
                   dbname = env$dbLoginData.dbname)
  query <- RMySQL::dbSendQuery(con, queryString)
  data <- RMySQL::fetch(query, n=-1)
  RMySQL::dbClearResult(RMySQL::dbListResults(con)[[1]])
  RMySQL::dbDisconnect(con)
  if (autoConvertTimestamps)
  {
    data.names = names(data)

    for (name in data.names)
    {
      name.check <- tolower(name)
      if (grepl("(^|[^a-zA-Z0-9])end($|[^a-zA-Z0-9])", name.check) || grepl("(^|[^a-zA-Z0-9])start($|[^a-zA-Z0-9])", name.check) || grepl("(^|[^a-zA-Z0-9])timestamp($|[^a-zA-Z0-9])", name.check))
      {
        data[[name]] = db.as.timestamp(data[[name]])
      }
    }
  }
  return(data);
}

#' Converts a list or column of values to POSIXct timestamp
#'
#' Converts character-values of format like "2019-07-15 10:05:13.445098" to a POSIXct timestamp
#'
#' @param values the values to convert
#' @return the values as POSIXct timestamp
#' @examples
#' db.as.timestamp(c("2019-07-15 10:05:13.445098"));
#' @export
db.as.timestamp <- function(values)
{
  return (as.POSIXct(strptime(values, "%Y-%m-%d %H:%M:%OS")))
}
