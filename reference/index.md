# Package index

## Lazy tables from GDAL vector sources

Create a lazy table from any GDAL-readable vector data source and pull
results into memory with collect(). Standard dplyr verbs are translated
to SQL and executed by GDAL.

- [`lazysf()`](https://hypertidy.github.io/lazysf/reference/lazysf.md) :
  Delayed (lazy) read for GDAL vector
- [`collect`](https://hypertidy.github.io/lazysf/reference/collect.md) :
  Collect a lazy GDAL query into memory
- [`lazysf-package`](https://hypertidy.github.io/lazysf/reference/lazysf-package.md)
  : lazysf: Delayed Read for 'GDAL' Vector Data Sources

## DBI interface

Connect directly with DBI for finer control over dialect, geometry
format, and the Arrow stream interface.

- [`GDALSQL()`](https://hypertidy.github.io/lazysf/reference/GDALSQL.md)
  : GDALSQL
- [`dbConnect(`*`<GDALVectorDriver>`*`)`](https://hypertidy.github.io/lazysf/reference/dbConnect-GDALVectorDriver-method.md)
  : dbConnect

## DBI classes and methods

S4 classes for the GDAL vector driver, connection, and result, with
their DBI methods (dbListTables, dbListFields, dbSendQuery, dbFetch, and
friends).

- [`dbDataType(`*`<GDALVectorDriver>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorDriver-class.md)
  [`dbIsValid(`*`<GDALVectorDriver>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorDriver-class.md)
  [`dbUnloadDriver(`*`<GDALVectorDriver>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorDriver-class.md)
  [`dbGetInfo(`*`<GDALVectorDriver>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorDriver-class.md)
  : Class GDALVectorDriver
- [`show(`*`<GDALVectorConnection>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  [`dbIsValid(`*`<GDALVectorConnection>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  [`dbGetInfo(`*`<GDALVectorConnection>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  [`dbSendQuery(`*`<GDALVectorConnection>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  [`dbReadTable(`*`<GDALVectorConnection>`*`,`*`<character>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  [`dbListTables(`*`<GDALVectorConnection>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  [`dbListFields(`*`<GDALVectorConnection>`*`,`*`<character>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  [`dbExistsTable(`*`<GDALVectorConnection>`*`,`*`<ANY>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  [`dbDisconnect(`*`<GDALVectorConnection>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorConnection-class.md)
  : Class GDALVectorConnection (and methods)
- [`show(`*`<GDALVectorResult>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorResult-class.md)
  [`dbFetch(`*`<GDALVectorResult>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorResult-class.md)
  [`dbClearResult(`*`<GDALVectorResult>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorResult-class.md)
  [`dbHasCompleted(`*`<GDALVectorResult>`*`)`](https://hypertidy.github.io/lazysf/reference/GDALVectorResult-class.md)
  : Class GDALVectorResult (and methods)
