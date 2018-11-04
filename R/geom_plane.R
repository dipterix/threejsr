#' Plane Geom
#' @export
GeomPlane <- R6::R6Class(
  classname = 'plane',
  inherit = TGeom,

  private = list(
    width = 1,
    height = 1,
    widthSegments = 1,
    heightSegments = 1
  ),

  public = list(
    initialize = function(
      position,
      mesh_name,
      mesh_info = mesh_name,
      width = 1,
      height = 1,
      widthSegments = 1,
      heightSegments = 1,
      ...
    ) {
      super$initialize(position = position, mesh_name = mesh_name,
                       mesh_info = mesh_info, mesh_type = 'plane', ...)

      private$widthSegments = widthSegments
      private$heightSegments = heightSegments
      private$width = width
      private$height = height

      return(self)
    },

    print = function(x, quiet = F, ...){
      s = c(sprintf('threejsr geom [plane] - %s', private$mesh_name),
            sprintf('\tradius \t\t\t- %.4f', private$radius),
            sprintf('\twidth \t\t- %.4f', private$width),
            sprintf('\theight \t\t- %.4f', private$height),
            sprintf('\twidthSegments \t\t- %d', private$widthSegments),
            sprintf('\theightSegments \t\t- %d', private$heightSegments),
            ''
      )
      ss = super$print(x, quiet = T)
      cat(s, ss, sep = '\n')
      return(invisible(self))
    },

    to_list = function(){
      re = super$to_list()
      re$geom_args = list(
        widthSegments = private$widthSegments,
        heightSegments = private$heightSegments,
        width = private$width,
        height = private$height
      )
      re
    }

  )
)
