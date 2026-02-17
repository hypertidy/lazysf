#' @include GDALVectorResult.R
#' @importClassesFrom DBI DBIConnection
#' @importMethodsFrom DBI dbSendQuery dbReadTable dbListTables dbExistsTable dbGetInfo dbIsValid dbDisconnect
NULL

#' @importFrom gdalraster GDALVector ogr_ds_layer_names
NULL

#' Class GDALVectorConnection (and methods)
#'
#' GDALVectorConnection objects are created by passing [GDALSQL()] as first
#' argument to [DBI::dbConnect()].
#' They are a superclass of the [DBI::DBIConnection-class] class.
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
setClass("GDALVectorConnection",
         contains = "DBIConnection",
         slots = list(
           DSN = "character",
           readonly = "logical",
           geom_format = "character",
           dialect = "character")
)


#' @rdname GDALVectorConnection-class
#' @export
setMethod("show", "GDALVectorConnection", function(object) {
  cat("<GDALVectorConnection>\n")
  dsn <- object@DSN
  if (grepl("pass", dsn, ignore.case = TRUE) || grepl("user", dsn, ignore.case = TRUE)) {
    dsn <- paste0(strsplit(dsn, "\\s")[[1L]][1L], "...")
  }
  cat("      DSN: ", dsn, "\n", sep = "")
  cat("  dialect: ", if (nzchar(object@dialect)) object@dialect else "(GDAL default)", "\n", sep = "")
  cat(" geometry: ", object@geom_format, "\n", sep = "")
  tables <- tryCatch(DBI::dbListTables(object), error = function(e) "(unavailable)")
  cat("   tables: ", paste(tables, collapse = ", "), "\n", sep = "")
})

#' @rdname GDALVectorConnection-class
#' @export
setMethod("dbIsValid", "GDALVectorConnection", function(dbObj, ...) {
  nzchar(dbObj@DSN)
})

#' @rdname GDALVectorConnection-class
#' @export
setMethod("dbGetInfo", "GDALVectorConnection",
          function(dbObj, ...) {
            list(
              db.version = gdalraster::gdal_version()[1L],
              dbname = dbObj@DSN,
              username = "",
              host = "",
              port = ""
            )
          })

#' @rdname GDALVectorConnection-class
#' @export
setMethod("dbSendQuery", "GDALVectorConnection",
          function(conn, statement, ...) {
            sql <- as.character(statement)

            ## dbplyr generates WHERE (0 = 1) for field discovery
            ## but OGRSQL on non-database sources doesn't handle it
            if (grepl("AS.*q\\b", sql) && grepl("WHERE \\(0 = 1\\)", sql)) {
              sql <- gsub("WHERE \\(0 = 1\\)", "LIMIT 0", sql)
            }

            ## GDALVector constructor: dsn, layer, read_only, open_options,
            ## spatial_filter, dialect
            lyr <- new(GDALVector, conn@DSN, sql, TRUE,
                       character(0), "", conn@dialect)
            lyr$returnGeomAs <- conn@geom_format
            lyr$quiet <- TRUE

            layer_data <- tryCatch(
              lyr$fetch(-1),
              error = function(e) {
                lyr$close()
                if (length(gregexpr("SELECT", statement,
                                    ignore.case = TRUE)[[1]]) > 1) {
                  stop(sprintf(
                    paste0("executing SQL failed: \n%s\n\n",
                           "perhaps driver in use does not support sub-queries?"),
                    statement), call. = FALSE)
                } else {
                  stop(sprintf("executing SQL failed: \n%s",
                               conditionMessage(e)), call. = FALSE)
                }
              }
            )
            lyr$close()

            if (getOption("lazysf.query.debug", FALSE)) {
              message(sprintf(
                "-------------\nlazysf debug ....\nSQL:\n%s\nnrows read:\n%i",
                statement, nrow(layer_data)))
            }

            new("GDALVectorResult", layer_data = layer_data)
          })


#' @rdname GDALVectorConnection-class
#' @export
setMethod("dbReadTable", c(conn = "GDALVectorConnection", name = "character"),
          function(conn, name, ...) {
            x <- dbSendQuery(conn, sprintf("SELECT * FROM %s", name))
            dbFetch(x)
          })

#' @rdname GDALVectorConnection-class
#' @export
setMethod("dbListTables", c(conn = "GDALVectorConnection"),
          function(conn, ...) {
            ogr_ds_layer_names(conn@DSN)
          })

#' @rdname GDALVectorConnection-class
#' @export
setMethod("dbExistsTable", c(conn = "GDALVectorConnection"),
          function(conn, name, ...) {
            name %in% dbListTables(conn, ...)
          })
