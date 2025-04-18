% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SFSQLDriver.R
\docType{class}
\name{SFSQLDriver-class}
\alias{SFSQLDriver-class}
\alias{dbDataType,SFSQLDriver,ANY-method}
\alias{dbIsValid,SFSQLDriver-method}
\alias{dbUnloadDriver,SFSQLDriver-method}
\alias{dbGetInfo,SFSQLDriver-method}
\title{Class SFSQLDriver.}
\usage{
\S4method{dbDataType}{SFSQLDriver,ANY}(dbObj, obj, ...)

\S4method{dbIsValid}{SFSQLDriver}(dbObj, ...)

\S4method{dbUnloadDriver}{SFSQLDriver}(drv, ...)

\S4method{dbGetInfo}{SFSQLDriver}(dbObj, ...)
}
\description{
SFSQLDriver objects are created by \code{\link[=SFSQL]{SFSQL()}} and used to select the correct
method in \code{\link[DBI:dbConnect]{DBI::dbConnect()}}.
They are a superclass of the \link[DBI:DBIDriver-class]{DBI::DBIDriver} class, and used purely for dispatch.
}
\details{
The "Usage" section lists the class methods overridden by \pkg{lazysf}.
The \code{\link[DBI:dbDriver]{DBI::dbUnloadDriver()}} method is a null-op.
}
\keyword{internal}
