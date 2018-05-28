\name{prices}
\alias{prices}
\title{Return the xts quote of the asset}
\usage{
prices(symbol)
}
\description{
The argument symbol is of the type character that use the quantmod package to search the asset quote and return the xts database of it, with the properly name of columns for the candlesticks functions.
}
\examples{
prices("PETR4.SA")
prices("AAPL")
prices("^BVSP")
}
