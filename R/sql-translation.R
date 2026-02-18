# SQL translation for GDAL vector connections
#
# Inherits SQLite base translations from dbplyr and adds SpatiaLite
# spatial functions when available. Without SpatiaLite, unknown
# function calls pass through as-is (standard dbplyr behaviour),
# so `mutate(a = ST_Area(geom))` still works in raw SQL form.
#
# The spatial translations allow idiomatic R usage:
#
#   lazysf("countries.gpkg") |>
#     mutate(area = st_area(geom)) |>
#     filter(st_within(geom, !!aoi))
#
# TODO: test with SpatiaLite-enabled GDAL build
# TODO: confirm which SpatiaLite version introduced each function
# TODO: consider st_read/st_write for geometry format conversion in SQL

#' @importFrom dbplyr sql_translation
#' @export
sql_translation.GDALVectorConnection <- function(con) {
  scalar_trans <- dbplyr::sql_translator(.parent = dbplyr::base_scalar,
                                         ## -- type casts (SQLite-specific) --
                                         as.numeric  = dbplyr::sql_cast("REAL"),
                                         as.double   = dbplyr::sql_cast("REAL"),
                                         as.integer  = dbplyr::sql_cast("INTEGER"),
                                         as.character = dbplyr::sql_cast("TEXT")
  )
  agg_trans <- dbplyr::sql_translator(.parent = dbplyr::base_agg)

  ## SpatiaLite functions only available under SQLITE dialect
  ## (and only when SpatiaLite is linked — errors deferred to execution)
  if (identical(toupper(con@dialect), "SQLITE") || !nzchar(con@dialect)) {
    scalar_trans <- dbplyr::sql_translator(.parent = scalar_trans,
                                           ## -- geometry constructors --
                                           st_point       = dbplyr::sql_prefix("MakePoint", 2),
                                           st_geomfromtext = dbplyr::sql_prefix("GeomFromText"),
                                           st_geomfromwkb  = dbplyr::sql_prefix("GeomFromWKB"),

                                           ## -- spatial predicates (return 0/1) --
                                           st_intersects  = dbplyr::sql_prefix("ST_Intersects", 2),
                                           st_within      = dbplyr::sql_prefix("ST_Within", 2),
                                           st_contains    = dbplyr::sql_prefix("ST_Contains", 2),
                                           st_touches     = dbplyr::sql_prefix("ST_Touches", 2),
                                           st_crosses     = dbplyr::sql_prefix("ST_Crosses", 2),
                                           st_overlaps    = dbplyr::sql_prefix("ST_Overlaps", 2),
                                           st_disjoint    = dbplyr::sql_prefix("ST_Disjoint", 2),
                                           st_equals      = dbplyr::sql_prefix("ST_Equals", 2),
                                           st_covers      = dbplyr::sql_prefix("ST_Covers", 2),
                                           st_coveredby   = dbplyr::sql_prefix("ST_CoveredBy", 2),

                                           ## -- measurements --
                                           st_area        = dbplyr::sql_prefix("ST_Area", 1),
                                           st_length      = dbplyr::sql_prefix("ST_Length", 1),
                                           st_perimeter   = dbplyr::sql_prefix("ST_Perimeter", 1),
                                           st_distance    = dbplyr::sql_prefix("ST_Distance", 2),

                                           ## -- geometry operations --
                                           st_buffer      = dbplyr::sql_prefix("ST_Buffer", 2),
                                           st_centroid    = dbplyr::sql_prefix("ST_Centroid", 1),
                                           st_union       = dbplyr::sql_prefix("ST_Union", 2),
                                           st_intersection = dbplyr::sql_prefix("ST_Intersection", 2),
                                           st_difference  = dbplyr::sql_prefix("ST_Difference", 2),
                                           st_convexhull  = dbplyr::sql_prefix("ST_ConvexHull", 1),
                                           st_simplify    = dbplyr::sql_prefix("ST_Simplify", 2),
                                           st_envelope    = dbplyr::sql_prefix("ST_Envelope", 1),
                                           st_boundary    = dbplyr::sql_prefix("ST_Boundary", 1),

                                           ## -- accessors --
                                           st_x           = dbplyr::sql_prefix("ST_X", 1),
                                           st_y           = dbplyr::sql_prefix("ST_Y", 1),
                                           st_srid        = dbplyr::sql_prefix("ST_SRID", 1),
                                           st_geometrytype = dbplyr::sql_prefix("ST_GeometryType", 1),
                                           st_numgeometries = dbplyr::sql_prefix("ST_NumGeometries", 1),
                                           st_numpoints   = dbplyr::sql_prefix("ST_NumPoints", 1),
                                           st_isvalid     = dbplyr::sql_prefix("ST_IsValid", 1),
                                           st_isempty     = dbplyr::sql_prefix("ST_IsEmpty", 1),
                                           st_issimple    = dbplyr::sql_prefix("ST_IsSimple", 1),
                                           st_astext      = dbplyr::sql_prefix("ST_AsText", 1),
                                           st_asbinary    = dbplyr::sql_prefix("ST_AsBinary", 1),

                                           ## -- coordinate transforms --
                                           st_transform   = dbplyr::sql_prefix("ST_Transform", 2)
    )
    agg_trans <- dbplyr::sql_translator(.parent = agg_trans,
                                        st_collect     = dbplyr::sql_aggregate("ST_Collect"),
                                        st_extent      = dbplyr::sql_aggregate("ST_Extent")
    )
  }

  win_trans <- dbplyr::sql_translator(.parent = dbplyr::base_no_win,
                                      ## Mark spatial aggregates as unsupported in window context
                                      ## (SpatiaLite doesn't support them as window functions)
                                      st_collect = dbplyr::sql_not_supported("st_collect"),
                                      st_extent  = dbplyr::sql_not_supported("st_extent")
  )

  dbplyr::sql_variant(scalar_trans, agg_trans, win_trans)
}

#' @importFrom dbplyr supports_window_clause
#' @export
supports_window_clause.GDALVectorConnection <- function(con) {
  TRUE
}

#' @importFrom dbplyr sql_escape_logical
#' @export
sql_escape_logical.GDALVectorConnection <- function(con, x) {
  ## SQLite uses 0/1 for logical
  y <- as.character(as.integer(x))
  y[is.na(x)] <- "NULL"
  y
}
