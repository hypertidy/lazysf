#' @include GDALVectorResult.R
#' @importClassesFrom DBI DBIConnection
#' @importMethodsFrom DBI dbSendQuery dbReadTable dbListTables dbListFields dbExistsTable dbGetInfo dbIsValid dbDisconnect
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
#' [DBI::dbReadTable()], [DBI::dbListFields()],
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

            ## dbplyr field/type discovery: intercept and use OGR metadata
            ## instead of executing SQL.  sql_query_fields() generates
            ## "SELECT * FROM <tbl> LIMIT 0"; older dbplyr used
            ## "WHERE (0 = 1)".  For named tables we can read the schema
            ## from $getLayerDefn() — no SQL execution at all.
            if (grepl("LIMIT 0\\s*$", sql) ||
                grepl("WHERE \\(0 = 1\\)", sql)) {
              tbl_name <- .extract_table_name(sql)
              if (!is.null(tbl_name)) {
                lyr <- new(GDALVector, conn@DSN, tbl_name, TRUE)
                lyr$returnGeomAs <- conn@geom_format
                lyr$quiet <- TRUE
                layer_data <- .schema_from_defn(lyr, conn@geom_format)
                lyr$close()
                if (getOption("lazysf.query.debug", FALSE)) {
                  message(sprintf(
                    "-------------\nlazysf debug ....\n%s\n%s",
                    "schema discovery via getLayerDefn for:", tbl_name))
                }
                return(new("GDALVectorResult", layer_data = layer_data))
              }
              ## subquery case: fall through to normal execution
              ## (SQLITE dialect handles WHERE (0 = 1) and LIMIT 0)
            }

            ## GDALVector constructor: dsn, layer, read_only, open_options,
            ## spatial_filter, dialect.
            ## Collapse to single line — GDALVector can't handle newlines
            ## in SQL, and dbplyr generates multiline SQL.
            sql <- gsub("\\s+", " ", sql)
            lyr <- new(GDALVector, conn@DSN, sql, TRUE,
                       character(0), "", conn@dialect)
            lyr$returnGeomAs <- conn@geom_format
            lyr$quiet <- TRUE
            geom_info <- .geom_info(lyr)

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
            ## GDAL prefixes column names when SQL uses table-qualified
            ## wildcards (e.g. SELECT "nc".* → nc.AREA, nc.NAME).
            ## Strip the prefix so names match what dbplyr expects.
            names(layer_data) <- sub("^[^.]+\\.", "", names(layer_data))
            layer_data <- .mark_geometry(layer_data,
                                         conn@geom_format, geom_info)
            ## Strip OGRFeatureSet class — its print method tries to
            ## re-process geometry that .mark_geometry() already converted
            ## to wk types. Plain data.frame is what dbplyr expects.
            class(layer_data) <- "data.frame"

            if (getOption("lazysf.query.debug", FALSE)) {
              message(sprintf(
                "-------------\nlazysf debug ....\nSQL:\n%s\nnrows read:\n%i",
                statement, nrow(layer_data)))
            }

            new("GDALVectorResult", layer_data = layer_data)
          })

## Get geometry column name and CRS from an open GDALVector layer.
.geom_info <- function(lyr) {
  nm <- lyr$getGeometryColumn()
  if (!nzchar(nm)) nm <- lyr$defaultGeomColName
  crs <- lyr$getSpatialRef()
  if (!nzchar(crs)) crs <- NULL
  list(col = nm, crs = crs)
}

## Apply wk type marking to geometry columns after fetch.
## WKB      -> wk::wkb(crs = ...)
## WKT      -> wk::wkt(crs = ...)
## BBOX     -> wk::rct(crs = ...)
## NONE     -> no-op
.mark_geometry <- function(df, geom_format, geom_info) {
  geom_col <- geom_info$col
  crs <- geom_info$crs
  if (geom_format == "NONE" || !geom_col %in% names(df)) return(df)
  col <- df[[geom_col]]
  if (geom_format %in% c("WKB", "WKB_ISO")) {
    df[[geom_col]] <- wk::wkb(col, crs = crs)
  } else if (geom_format %in% c("WKT", "WKT_ISO")) {
    df[[geom_col]] <- wk::wkt(col, crs = crs)
  } else if (geom_format == "BBOX") {
    df[[geom_col]] <- .bbox_as_rct(col, crs = crs)
  }
  df
}

## Convert GDAL BBOX list (list of numeric(4)) to wk::rct.
.bbox_as_rct <- function(x, crs = NULL) {
  if (length(x) == 0L) {
    return(wk::rct(crs = crs))
  }
  vals <- do.call(rbind, x)
  wk::rct(xmin = vals[, 1L], ymin = vals[, 2L],
          xmax = vals[, 3L], ymax = vals[, 4L], crs = crs)
}

## Extract a simple table name from dbplyr field-discovery SQL.
## Returns NULL for subqueries (first FROM followed by parenthesis).
## Handles: FROM "quoted_name", FROM `backtick`, FROM bare_name
.extract_table_name <- function(sql) {
  from_match <- regexpr("FROM\\s+", sql, ignore.case = TRUE)
  if (from_match < 0L) return(NULL)
  after_from <- substring(sql, from_match + attr(from_match, "match.length"))
  ## subquery: first FROM is followed by (
  if (grepl("^\\s*\\(", after_from)) return(NULL)
  m <- regmatches(after_from, regexec('^"([^"]+)"', after_from))[[1L]]
  if (length(m) >= 2L) return(m[2L])
  m <- regmatches(after_from, regexec("^`([^`]+)`", after_from))[[1L]]
  if (length(m) >= 2L) return(m[2L])
  m <- regmatches(after_from, regexec("^([A-Za-z_][A-Za-z0-9_.]*)", after_from))[[1L]]
  if (length(m) >= 2L) return(m[2L])
  NULL
}


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
setMethod("dbListFields", c(conn = "GDALVectorConnection", name = "character"),
          function(conn, name, ...) {
            lyr <- new(GDALVector, conn@DSN, name, TRUE)
            defn <- lyr$getLayerDefn()
            lyr$close()
            names(defn)
          })

#' @rdname GDALVectorConnection-class
#' @export
setMethod("dbExistsTable", c(conn = "GDALVectorConnection"),
          function(conn, name, ...) {
            name %in% dbListTables(conn, ...)
          })
