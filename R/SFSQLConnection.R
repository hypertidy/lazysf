

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
           readonly = "logical")
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
            args <- list(...)
            args$dsource <- conn@DSN
            args$layer <- 0
            args$sql <- statement           ## user can't do this (warn?)
            ##args$extent <- "placeholder"
            #args$quiet <- TRUE; args$as_tibble <- TRUE ## hardcoded
            #browser()
            qu <- as.character(args$sql)

            if (grepl("AS.*q", qu) && grepl("WHERE \\(0 = 1)", qu)) {
              ## workaround for non-DB sources
              args$sql <- dbplyr::sql(gsub("WHERE \\(0 = 1)", "LIMIT 0", qu))
            }
op <- options(warn = -1)
on.exit(options(op), add = TRUE)
    #       layer_data <- do.call(sf::st_read, args)
   geom_name <- vapour::vapour_geom_name(args$dsource, args$layer, sql = args$sql)
   info <- vapour::vapour_layer_info(args$dsource, args$layer, sql = args$sql, count = FALSE)

layer_data <- tibble::as_tibble(c(do.call(vapour::vapour_read_fields, args),
                                setNames(list(wk::wkb(do.call(vapour::vapour_read_geometry, args), crs = info$projection$Wkt)), geom_name)))

           if (getOption("lazysf.query.debug")) {
             message(sprintf("-------------\nlazysf debug ....\nSQL:\n%s\nnrows read:\n%i",
                             statement), nrow(layer_data))
           }
           if (inherits(layer_data, "try-error")) {
             if (length(gregexpr("SELECT", statement, ignore.case = TRUE)[[1]]) > 1) {
               stop(sprintf("executing SQL failed: \n%s\n\nperhaps driver in use does not support sub-queries?",
                            statement))
             } else {
               stop("executing SQL failed")
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

            layers <- vapour::vapour_layer_names(conn@DSN, ...)

          })

#' @rdname SFSQLConnection-class
#' @export
setMethod("dbExistsTable", c(conn = "SFSQLConnection"),
          function(conn, name, ...){
            name %in% dbListTables(conn, ...)
          })








