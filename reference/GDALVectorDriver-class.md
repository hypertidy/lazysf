# Class GDALVectorDriver

GDALVectorDriver objects are created by
[`GDALSQL()`](https://hypertidy.github.io/lazysf/reference/GDALSQL.md)
and used to select the correct method in
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
They are a superclass of the
[DBI::DBIDriver](https://dbi.r-dbi.org/reference/DBIDriver-class.html)
class, and used purely for dispatch.

## Usage

``` r
# S4 method for class 'GDALVectorDriver'
dbDataType(dbObj, obj, ...)

# S4 method for class 'GDALVectorDriver'
dbIsValid(dbObj, ...)

# S4 method for class 'GDALVectorDriver'
dbUnloadDriver(drv, ...)

# S4 method for class 'GDALVectorDriver'
dbGetInfo(dbObj, ...)
```

## Details

The "Usage" section lists the class methods overridden by lazysf. The
[`DBI::dbUnloadDriver()`](https://dbi.r-dbi.org/reference/dbDriver.html)
method is a null-op.
