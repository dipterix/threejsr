# THREEJS Geoms

#' Abstract Geom Type
#' @export
TGeom <- R6::R6Class(
  classname = 'TGeom',
  private = list(
    mesh_name = '',
    geom_args = NULL,
    mesh_type = '',
    layer = 1,
    position = c(0,0,0),
    events = list(),
    transform = diag(1,4),
    controls = list(),
    enabled_events = NULL
  ),
  public = list(
    mesh_info = '',

    initialize = function(position, mesh_name, mesh_type, mesh_info = mesh_name, ...,
                          layer = 1, enabled = c('clip', 'hover'), .args = list()){
      private$position = position
      private$mesh_name = mesh_name
      private$mesh_type = mesh_type
      private$geom_args = c(list(...), .args)
      private$layer = layer
      private$enabled_events = enabled
      self$mesh_info = mesh_info
    },

    add_position_control = function(label, axis = 'z', min = 0, max = 1, initial = 0, step = 0.01){
      if(!is.list(private$controls[['position']])){
        private$controls[['position']] = list()
      }
      private$controls[['position']] = list(
        initial = initial,
        label = label,
        axis = axis,
        min = min,
        max = max,
        step = step
      )
      names(private$controls[['position']])[1] = label
    },

    rotateX = function( theta ){
      c = cos(theta)
      s = sin(theta)

      x = matrix(c(
        1, 0, 0, 0,
        0, c, - s, 0,
        0, s, c, 0,
        0, 0, 0, 1
      ), byrow = T, nrow = 4)

      private$transform = x %*% private$transform
    },
    rotateY = function( theta ){
      c = cos(theta)
      s = sin(theta)

      x = matrix(c(
        c, 0, s, 0,
        0, 1, 0, 0,
        - s, 0, c, 0,
        0, 0, 0, 1
      ), byrow = T, nrow = 4)

      private$transform = x %*% private$transform
    },
    rotateZ = function( theta ){
      c = cos(theta)
      s = sin(theta)

      x = matrix(c(
        c, - s, 0, 0,
        s, c, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
      ), byrow = T, nrow = 4)

      private$transform = x %*% private$transform
    },

    set_transform = function(mat, append = T){
      assertthat::assert_that(is.matrix(mat) && ncol(mat) == 4 && nrow(mat) == 4, msg = 'transform needs to be a 4x4 matrix.')
      if(append){
        mat = mat %*% private$transform
      }
      private$transform = mat
    },

    to_list = function(){
      list(
        mesh_type = private$mesh_type,
        mesh_name = private$mesh_name,
        geom_args = private$geom_args,
        position = private$position,
        layer = private$layer,
        events = private$events,
        controls = private$controls,
        transform = private$transform,
        mesh_info = self$mesh_info,
        enabled_events = private$enabled_events
      )
    },

    positional_event = function(event_data, axis = 'z'){
      private$events[['position']] = list(
        data = event_data,
        sub_type = axis
      )
    },

    animation_event = function(event_data, loop = FALSE){
      private$events[['animation']] = list(
        data = event_data,
        sub_type = ifelse(loop, 'loop', '')
      )
    }
  )
)



#' @export
as.list.TGeom <- function(obj){
  obj$to_list()
}
