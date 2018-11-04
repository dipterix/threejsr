# Stores S3 functions to create geoms

#' Util function to map data to color
#' @param name color scheme name, choices are "rainbow", "grey", "terrain", "heat", "cm", or "customized"
#' @param nlevels number of distinct colors
#' @param plot visualize color mappings
#' @param customize vector of colors, see grDevices::colorRampPalette (valid only when name="customized")
#' @param bias passed to grDevices::colorRampPalette, see grDevices::colorRampPalette
#' @param space passed to grDevices::colorRampPalette, see grDevices::colorRampPalette
#' @param interpolate passed to grDevices::colorRampPalette, see grDevices::colorRampPalette
#' @param data any vector, matrix or array to calculate color mappings (can be blank)
#' @param center center alignmnet. For example, if you want color center standing for data value 0, set center=0. Default is the mean of range(data)
#' @param reversed reverse color map
#' @export
threejsr_palettes <- function(name, nlevels = 64, plot = F, customize, data, center,
                              bias = 1, space = 'rgb', interpolate = 'linear', reversed = FALSE){
  if(!missing(name) && length(name) == 1){
    switch (
      name,
      'rainbow' = {
        p = grDevices::rainbow(nlevels)
      },
      'grey' = {
        p = grDevices::gray.colors(nlevels, start = 0.1, end = 0.9)
      },
      'terrain' = {
        p = grDevices::terrain.colors(nlevels)
      },
      'heat' = {
        p = grDevices::heat.colors(nlevels)
      },
      'topo' = {
        p = grDevices::topo.colors(nlevels)
      },
      'cm' = {
        p = grDevices::cm.colors(nlevels)
      },
      {
        name = NULL
      }
    )
  }else{
    name = NULL
  }
  if(is.null(name)){
    p = grDevices::colorRampPalette(colors = customize, bias = bias, space = space, interpolate = interpolate, alpha = F)(nlevels)
  }
  if(reversed){
    p = rev(p)
  }
  # convert p to rgb
  p = t(grDevices::col2rgb(p, alpha = F) / 255)


  if(!missing(data)){
    # map data to color
    if(!is.numeric(data)){
      # factor data
      f = as.factor(data)
      # center is ignored and spread nlevels
      l = levels(f)
      if(length(l) == 1){
        s = 0
        col = p[1,,drop = F]
      }else{
        s = floor((nlevels - 1) / (length(l) - 1))
        col = p[s * (seq_along(l) - 1) + 1, ]
      }

      re = data.frame(
        red = col[,1],
        green = col[,2],
        blue = col[,3],
        key = l,
        hex = grDevices::rgb(col[,1], col[,2], col[,3], maxColorValue = 1),
        stringsAsFactors = F
      )
      attr(re, 'color_transform') = list(
        type = 'discrete',
        scale = s,
        transform = '(as.numeric(x) - 1) * scale + 1'
      )
      if(plot){
        with(re, {
          plot(factor(key), col = hex, yaxt = 'n')
        })
      }
    }else{
      # this is continuous data
      range = range(data)
      if(missing(center)){
        center = mean(range)
      }
      range = range - center
      if(range[1] == range[2]){
        # only one color,
        s = 0

        re = data.frame(
          red = p[,1],
          green = p[,2],
          blue = p[,3],
          key = range[1] + center,
          hex = grDevices::rgb(p[,1], p[,2], p[,3], maxColorValue = 1),
          stringsAsFactors = F
        )

        if(plot){
          message('Only one value in your data, not able to visualize (set plot=FALSE will remove this message)')
        }

      }else{
        s = (nlevels - 2) / max(abs(range)) / 2

        re = data.frame(
          red = p[,1],
          green = p[,2],
          blue = p[,3],
          key = (seq_len(nlevels) - nlevels/2 -1)/ s  + center,
          hex = grDevices::rgb(p[,1], p[,2], p[,3], maxColorValue = 1),
          stringsAsFactors = F
        )

        if(plot){
          graphics::image(x = re$key, z = matrix(seq_len(nlevels), ncol = 1), col = re$hex, ylim = c(-1,1),
                yaxt='n', xlim = range+center)
          graphics::rect(range[1] + center, -0.8, range[2] + center, 0.8, lty = 2, lwd = 3)
        }

      }

      attr(re, 'color_transform') = list(
        type = 'continuous',
        scale = s,
        center = center,
        nlevels = nlevels,
        transform = 'round((x - center) * scale + (nlevels / 2)) + 1'
      )

    }
  }else{
    re = data.frame(
      red = p[,1],
      green = p[,2],
      blue = p[,3],
      hex = grDevices::rgb(p[,1], p[,2], p[,3], maxColorValue = 1),
      stringsAsFactors = F
    )
    if(plot){
      x = seq_len(nlevels)
      graphics::image(x = x, z = matrix(x, ncol = 1), col = re$hex, ylim = c(-1,1), xaxt='n', yaxt='n', xlim = range(x))
    }
  }
  if(plot){
    return(invisible(re))
  }else{
    return(re)
  }
}

#' Create sphere object (GeomSphere)
#' @param position vector of 3, geom coordinates, for example c(0,0,0) stands for origin
#' @param mesh_name character, unique identifier/name,
#' @param mesh_info default is mesh_name, html info to show when object is clicked
#' @param layer geom layer (visibility), see ?threejs_scene
#' @param hover_enabled when mouse hover over, show helpers
#' @param radius number, positive
#' @param ... other params for GeomSphere$new(...)
#' @export
geom_sphere <- function(position, mesh_name, radius, mesh_info = mesh_name,
                        layer = 1, hover_enabled = T, ...){
  g = GeomSphere$new(position = position, mesh_name = mesh_name, mesh_info = mesh_info,
                     radius = radius, layer = layer, hover_enabled = hover_enabled, is_clipper = F, ...)
  g
}

#' Create plane object (GeomPlane)
#' @param position vector of 3, geom coordinates, for example c(0,0,0) stands for origin
#' @param mesh_name character, unique identifier/name,
#' @param mesh_info default is mesh_name, html info to show when object is clicked
#' @param layer geom layer (visibility), see ?threejs_scene
#' @param hover_enabled when mouse hover over, show helpers
#' @param width plane width
#' @param height plane height
#' @param is_clipper can clip objects?
#' @param ... other params for GeomPlane$new(...)
#' @export
geom_plane <- function(position, mesh_name, width, height, mesh_info = mesh_name, layer = 1,
                       hover_enabled = T, is_clipper = F, ...){
  g = GeomPlane$new(position = position, mesh_name = mesh_name, mesh_info = mesh_info,width = width,height = height,
                    layer = layer, hover_enabled = hover_enabled, is_clipper = is_clipper, ...)
  g
}


#' Create free mesh object with given vertices and faces (GeomFreeMesh)
#' @param position vector of 3, geom coordinates, for example c(0,0,0) stands for origin
#' @param mesh_name character, unique identifier/name,
#' @param mesh_info default is mesh_name, html info to show when object is clicked
#' @param layer geom layer (visibility), see ?threejs_scene
#' @param hover_enabled when mouse hover over, show helpers
#' @param vertices mesh vertices, a "n x 3" position matrix (n is number of vertices)
#' @param faces mesh faces, a "m x 3" face matrix (m is number of triangles)
#' @param clippers mesh names of planes with "is_clipper=TRUE"
#' @param clip_intersect clip mode
#' @param ... other params for GeomFreeMesh$new(...)
#' @export
geom_free <- function(position, mesh_name, vertices, faces, mesh_info = mesh_name, layer = 1,
                      hover_enabled = F, clippers = NULL, clip_intersect = F, ...){
  g = GeomFreeMesh$new(position = position, mesh_name = mesh_name, mesh_info = mesh_info,
                       vertices = vertices, faces = faces, layer = layer, hover_enabled = hover_enabled,
                       is_clipper = F, clippers = clippers, clip_intersect = clip_intersect, ...)
  g
}



