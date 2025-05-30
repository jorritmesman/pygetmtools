% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_pygetm_output.R
\name{read_pygetm_output}
\alias{read_pygetm_output}
\title{Read a PyGETM output file (3D)}
\usage{
read_pygetm_output(
  ncdf,
  var,
  x = NULL,
  y = NULL,
  depth = NULL,
  z = NULL,
  transect = NULL,
  save_everything = F,
  round_depth = NULL,
  round_val = NULL,
  profile_interval = NULL
)
}
\arguments{
\item{ncdf}{character; name of the output nc file}

\item{var}{character; name of the variable in the nc file}

\item{x, y}{numeric; x and y coordinates. If NULL, extracts all.}

\item{depth}{numeric; depth below surface, should be negative. If NULL, extracts all.
'depth' and 'z' cannot be provided both.}

\item{z}{numeric; layer number (0 at bottom). If NULL, extracts all.
These are the actual values, not the index (or: index in Python-counting starting at 0).
'depth' and 'z' cannot be provided both.}

\item{transect}{data.table; headers 'x' and 'y', providing a set of cells to extract.
Can be used in combination with depth/z or not. 'If NULL, 'x' and 'y' args are
ignored of 'transect' is provided. If NULL, will not extract transect.}

\item{save_everything}{logical; if TRUE, exports entire output, without formatting. Defaults to FALSE}

\item{round_depth, round_val}{integer; Round depth and variable value to this many digits. No rounding if NULL.}

\item{profile_interval}{numeric; single value, calculates output depths for a profile with
this depth interval}
}
\description{
Read a PyGETM output file (3D)
}
\details{
Read a PyGETM 3D output file into R, with options for extracting certain cells.
  Several options for extracting output. Either everything ('save_everything' is TRUE),
  2D from top ('x' and 'y' are NULL and specifying 'depth' or 'z'), a 1D depth profile
  (specifying 'x' and 'y' and both 'depth' and 'z' are NULL), a single point ('x', 'y',
  and 'depth/z'), or 1D/2D from the "side" (provided 'transect' and optionally 'depth/z').
}
\examples{
 \dontrun{
 read_pygetm_output(ncdf = "pygetm_output_3d.nc",
                    var = "temp",
                    x = NULL,
                    y = NULL,
                    depth = NULL,
                    z = 19,
                    transect = NULL,
                    save_everything = FALSE,
                    round_depth = 2L,
                    round_val = 3L)
 }
}
\author{
Jorrit Mesman
}
