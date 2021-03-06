% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palette-projects.R
\name{project_pal}
\alias{project_pal}
\title{Return function to interpolate a project color palette}
\usage{
project_pal(palette = "main", reverse = FALSE, ...)
}
\arguments{
\item{palette}{Character name of palette in project_palettes, or a vector of colour names
(NDev, MemP, NCP, MoBa, Loci, MemC, ACon, S2C)}

\item{reverse}{Boolean indicating whether the palette should be reversed}

\item{...}{Additional arguments to pass to colorRampPalette()}
}
\description{
Return function to interpolate a project color palette
}
