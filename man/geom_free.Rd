% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3_functions.R
\name{geom_free}
\alias{geom_free}
\title{Create free mesh object with given vertices and faces (GeomFreeMesh)}
\usage{
geom_free(position, mesh_name, vertices, faces, mesh_info = mesh_name,
  layer = 1, hover_enabled = F, clippers = NULL,
  clip_intersect = F, ...)
}
\arguments{
\item{position}{vector of 3, geom coordinates, for example c(0,0,0) stands for origin}

\item{mesh_name}{character, unique identifier/name,}

\item{vertices}{mesh vertices, a "n x 3" position matrix (n is number of vertices)}

\item{faces}{mesh faces, a "m x 3" face matrix (m is number of triangles)}

\item{mesh_info}{default is mesh_name, html info to show when object is clicked}

\item{layer}{geom layer (visibility), see ?threejs_scene}

\item{hover_enabled}{when mouse hover over, show helpers}

\item{clippers}{mesh names of planes with "is_clipper=TRUE"}

\item{clip_intersect}{clip mode}

\item{...}{other params for GeomFreeMesh$new(...)}
}
\description{
Create free mesh object with given vertices and faces (GeomFreeMesh)
}
