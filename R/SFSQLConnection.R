

#' @importFrom sf read_sf st_layers
#' @importFrom tibble tibble
NULL



#' Class SFSQLConnection (and methods)
#'
#' SFSQLConnection objects are created by passing [SFSQL()] as first
#' argument to [DBI::dbConnect()].
#' They are a superclass of the [DBIConnection-class] class.
#' The "Usage" section lists the class methods overridden by \pkg{lazysf}.
#'
#' @seealso
#' The corresponding generic functions
#' [DBI::dbSendQuery()], [DBI::dbDisconnect()],
#' [DBI::dbReadTable()],
#' [DBI::dbExistsTable()], [DBI::dbListTables()].
#'
#' @keywords internal
#' @export
setClass("SFSQLConnection",
         contains = "DBIConnection",
         slots = list(
           DSN = "character",
           readonly = "logical",
           sf_read_args = "list")
)


## do we need?
# The corresponding generic functions
# [DBI::dbSendQuery()], [DBI::dbGetQuery()],
# [DBI::dbSendStatement()], [DBI::dbExecute()],
# [DBI::dbExistsTable()], [DBI::dbListTables()], [DBI::dbListFields()],
# [DBI::dbRemoveTable()], and [DBI::sqlData()].


#' @rdname SFSQLConnection-class
#' @export
setMethod("show", "SFSQLConnection", function(object) {
  cat("<SFSQLConnection>\n")
  tables <- DBI::dbListTables(object)
  dsn <- object@DSN
  if (grepl("pass", dsn, ignore.case = TRUE) || grepl("user", dsn, ignore.case = TRUE)) {

    dsn <- paste0(strsplit(dsn, "\\s")[[1L]][1L], "...")
  }
  cat("   DSN: ", dsn, "\n", sep = "")
  cat("tables: ", paste(tables, collapse = ", "), "\n", sep = "")
})







#' @rdname SFSQLConnection-class
#' @export
setMethod("dbSendQuery", "SFSQLConnection",
          function(conn, statement, ...) {
            ## quiet and fake layer because we aren't using layer  = (it's in the query)
            args <- conn@sf_read_args
            args$dsn <- DSN <- conn@DSN
            args$layer <- "<this is unused but keeps sf quiet>"
            args$query <- statement           ## user can't do this (warn?)
            layer_data <- do.call(sf::st_read, args)

            #layer_data <- sf::read_sf(DSN, layer = "<this is unused>", query = statement, quiet = TRUE)
            if (inherits(layer_data, "try-error")) {
              message("executing SQL failed:")
              writeLines(statement)
              if (length(gregexpr("SELECT", statement, ignore.case = TRUE)[[1]]) > 1) {
                stop("perhaps driver in use does not support sub-queries?")
              } else {
                stop("")
              }
            }
            new("SFSQLResult",
                layer_data = layer_data)

          })






#' @rdname SFSQLConnection-class
#' @export
setMethod("dbReadTable", c(conn = "SFSQLConnection", name = "character"),
          function(conn, name, ...){
            x <- dbSendQuery(conn, sprintf("SELECT * FROM %s", name))
            dbFetch(x)
          })

#' @rdname SFSQLConnection-class
#' @export
setMethod("dbListTables", c(conn = "SFSQLConnection"),
          function(conn, ...){
            layers <- sf::st_layers(conn@DSN, ...)
            layers$name
          })

#' @rdname SFSQLConnection-class
#' @export
setMethod("dbExistsTable", c(conn = "SFSQLConnection"),
          function(conn, name, ...){
            name %in% dbListTables(conn, ...)
          })








