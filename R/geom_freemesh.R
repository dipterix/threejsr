#' Free Geom object
#' @import assertthat
#' @export
GeomFreeMesh <- R6::R6Class(
  classname = 'FreeMesh',
  inherit = TGeom,

  private = list(
    vertices = NULL,
    faces = NULL,
    normals = NULL,
    colors = NULL
  ),

  public = list(
    initialize = function(
      position,
      mesh_name,
      mesh_info = mesh_name,
      vertices,
      faces,
      color = '#E6E6E6',
      ...
    ) {
      super$initialize(position = position, mesh_name = mesh_name,
                       mesh_info = mesh_info, mesh_type = 'freemesh', ...)

      private$vertices = as.vector(t(vertices[,1:3]))
      private$faces = as.vector(t(faces[,1:3]))
      private$normals = as.vector(replicate(nrow(vertices), c(0,0,1)))

      if(length(color) == 1){
        color = as.vector(grDevices::col2rgb(color))
        color = color / 255
        color = as.vector(replicate(nrow(vertices), color))
      }else{
        assertthat::assert_that(length(color) == 3 * nrow(vertices),
                                msg = 'color MUST either be a length of one such as "#E2E2E2", or a rgb matrix with dimension 3x(# of vertices)')
        color = as.numeric(color)
      }

      private$colors = color

    },

    get_data = function(name = 'vertices', reshape = F){
      assertthat::assert_that(name %in% c('vertices', 'faces'), msg = 'name must be "vertices" or "faces"')
      re = private[[name]]
      if(reshape){
        dim(re) = c(3, length(re) / 3)
      }
      re
    },


    get_vertex_position = function(idx, start_from = 0){
      private$vertices[(idx - start_from) * 3 + 1:3]
    },

    get_face = function(face_id, start_from = 1){
      vid = private$faces[(face_id - start_from) * 3 + 1:3]
      # TODO get positions and area of this face
      vid
    },

    to_list = function(){
      re = super$to_list()
      re$geom_args = list(
        vertices = private$vertices,
        faces = private$faces,
        normals = private$normals,
        colors = private$colors
      )
      re
    },

    print = function(x, quiet = F, ...){
      s = c(sprintf('threejsr geom [freemesh] - %s', private$mesh_name),
            sprintf('\tn vertices \t\t- %d', length(private$vertices) / 3),
            sprintf('\tn faces \t\t- %d', length(private$faces) / 3),
            '[GeomSphere] methods:', '',
            '\tget_data \t\t-Get vertices or faces data',
            '\tget_vertex_position \t- Given vertex id, tell its position. Vertex id starts from 0',
            '\tget_face \t\t- Given face id, tells vertices id. Face id starts from 1',
            ''
      )
      ss = super$print(x, quiet = T)
      cat(s, ss, sep = '\n')
      return(invisible(self))
    }

  )
)
