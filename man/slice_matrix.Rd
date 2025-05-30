% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slice_matrix.R
\name{slice_matrix}
\alias{slice_matrix}
\title{Extract part of a matrix}
\usage{
slice_matrix(
  mtrx,
  x_dim,
  y_dim,
  x,
  y,
  depth,
  z,
  transect,
  mtrx_zct = NULL,
  mtrx_surf = NULL,
  mtrx_bott = NULL,
  add_depth_to_output = T,
  profile_interval = NULL
)
}
\arguments{
\item{mtrx}{matrix; matrix of the variable to extract. Result of ncdf4::ncvar_get(...)}

\item{x_dim, y_dim}{numeric; vector of the values of the 'x' and 'y' dimensions}

\item{x, y}{numeric; x and y coordinates. If NULL, extracts all.
These are the actual values, not the index.}

\item{depth}{numeric; depth below surface, should be negative. If NULL, extracts all.
'depth' and 'z' cannot be provided both.}

\item{z}{numeric; layer number (1 at bottom). If NULL, extracts all.
Note: NOT in Python-counting (so first one is "1"). 'depth' and 'z' cannot be provided both.}

\item{transect}{data.table; headers 'x' and 'y', providing a set of cells to extract.
Can be used in combination with depth/z or not. 'If NULL, 'x' and 'y' args are
ignored of 'transect' is provided. If NULL, will not extract transect.}

\item{mtrx_zct}{matrix: matrix of 'zct'. Result of ncdf4::ncvar_get(...)}

\item{mtrx_surf, mtrx_bott}{matrix; similar to mtrx_zct, but based on a matrix
of the interfaces ('zft') and given for the uppermost and lowermost layer, respectively}

\item{add_depth_to_output}{logical; if true and 'depth' is not provided, then
still 'depth' is calculated for each value of 'z'}

\item{profile_interval}{numeric; single value, calculates output depths for a profile with
this depth interval}
}
\description{
Extract part of a matrix
}
\details{
Takes a matrix (as gotten from ncvar_get() on a PyGETM output file) and
  extracts part of it, based on the input arguments. 
  The dimensions of the matrix, in order, are 'x', 'y', 'z', 'time'
  At least for now, if specified, x, y, z, and depth must be single numbers:
  ranges are not possible. This function is for internal use in
  read_pygetm_output.R.
}
\author{
Jorrit Mesman
}
