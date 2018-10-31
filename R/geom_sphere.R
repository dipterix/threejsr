#' Sphere geom
#' @export
GeomSphere <- R6::R6Class(
  classname = 'Sphere',
  inherit = TGeom,

  private = list(
    radius = 10,
    widthSegments = 10,
    heightSegments = 6,
    phiStart = 0,
    phiLength = 2 * pi,
    thetaStart = 0,
    thetaLength = pi
  ),

  public = list(
    initialize = function(
      position,
      mesh_name,
      mesh_info = mesh_name,
      radius = 10,
      widthSegments = 10,
      heightSegments = 6,
      phiStart = 0,
      phiLength = 2 * pi,
      thetaStart = 0,
      thetaLength = pi,
      ...
    ) {
      super$initialize(position = position, mesh_name = mesh_name, mesh_info = mesh_info, mesh_type = 'sphere', ...)

      private$radius = radius
      private$widthSegments = widthSegments
      private$heightSegments = heightSegments
      private$phiStart = phiStart
      private$phiLength = phiLength
      private$thetaStart = thetaStart
      private$thetaLength = thetaLength

      return(self)
    },

    set_radius = function(radius){
      assertthat::assert_that(length(radius) == 1 && is.numeric(radius) && radius > 0, msg = 'radius must be positive number')
      radius = max(radius, 0)
      private$radius = radius
    },

    to_list = function(){
      re = super$to_list()
      re$geom_args = list(
        radius = private$radius,
        widthSegments = private$widthSegments,
        heightSegments = private$heightSegments,
        phiStart = private$phiStart,
        phiLength = private$phiLength,
        thetaStart = private$thetaStart,
        thetaLength = private$thetaLength
      )
      re
    }

  )
)
