# Class GDALVectorConnection (and methods)

GDALVectorConnection objects are created by passing
[`GDALSQL()`](https://hypertidy.github.io/lazysf/reference/GDALSQL.md)
as first argument to
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
They are a superclass of the
[DBI::DBIConnection](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
class. The "Usage" section lists the class methods overridden by lazysf.

## Usage

``` r
# S4 method for class 'GDALVectorConnection'
show(object)

# S4 method for class 'GDALVectorConnection'
dbIsValid(dbObj, ...)

# S4 method for class 'GDALVectorConnection'
dbGetInfo(dbObj, ...)

# S4 method for class 'GDALVectorConnection'
dbSendQuery(conn, statement, ...)

# S4 method for class 'GDALVectorConnection,character'
dbReadTable(conn, name, ...)

# S4 method for class 'GDALVectorConnection'
dbListTables(conn, ...)

# S4 method for class 'GDALVectorConnection,character'
dbListFields(conn, name, ...)

# S4 method for class 'GDALVectorConnection,ANY'
dbExistsTable(conn, name, ...)

# S4 method for class 'GDALVectorConnection'
dbDisconnect(conn, ...)
```

## See also

The corresponding generic functions
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html),
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html),
[`DBI::dbReadTable()`](https://dbi.r-dbi.org/reference/dbReadTable.html),
[`DBI::dbListFields()`](https://dbi.r-dbi.org/reference/dbListFields.html),
[`DBI::dbExistsTable()`](https://dbi.r-dbi.org/reference/dbExistsTable.html),
[`DBI::dbListTables()`](https://dbi.r-dbi.org/reference/dbListTables.html).
