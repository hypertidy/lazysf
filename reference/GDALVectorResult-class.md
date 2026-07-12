# Class GDALVectorResult (and methods)

GDALVectorResult objects are created by
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html),
and encapsulate the result of an SQL statement. They are a superclass of
the
[DBI::DBIResult](https://dbi.r-dbi.org/reference/DBIResult-class.html)
class. The "Usage" section lists the class methods overridden by lazysf.

## Usage

``` r
# S4 method for class 'GDALVectorResult'
show(object)

# S4 method for class 'GDALVectorResult'
dbFetch(res, n = -1, ...)

# S4 method for class 'GDALVectorResult'
dbClearResult(res, ...)

# S4 method for class 'GDALVectorResult'
dbHasCompleted(res, ...)
```

## See also

The corresponding generic functions
[`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html),
[`DBI::dbClearResult()`](https://dbi.r-dbi.org/reference/dbClearResult.html),
and
[`DBI::dbHasCompleted()`](https://dbi.r-dbi.org/reference/dbHasCompleted.html).
