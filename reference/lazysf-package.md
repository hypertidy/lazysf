# lazysf: Delayed Read for 'GDAL' Vector Data Sources

Lazy read for drawings. A 'dplyr' back end for data sources supported by
'GDAL' vector drivers, that allows working with local or remote sources
as if they are in-memory data frames. Basic features work with any
drawing format ('GDAL vector data source') supported by the 'gdalraster'
package.

## Package Options

There is a debug option `options(lazysf.query.debug = TRUE)` which if
set will cause the generated SQL statement to be printed before every
call. In addition it will print the number of rows actually read.

Default geometry format can be set with
`options(lazysf.geom_format = "WKB")`. Default SQL dialect can be set
with `options(lazysf.dialect = "SQLITE")`.

## See also

Useful links:

- <https://github.com/hypertidy/lazysf>

- <https://hypertidy.github.io/lazysf/>

- Report bugs at <https://github.com/hypertidy/lazysf/issues>

## Author

**Maintainer**: Michael Sumner <mdsumner@gmail.com>
([ORCID](https://orcid.org/0000-0002-2471-7511))

Authors:

- Michael Sumner <mdsumner@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-2471-7511))
