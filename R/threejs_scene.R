#' @import htmlwidgets
#' @export
threejs_scene <- function(...){
  UseMethod('threejs_scene')
}

#' @import shiny
#' @export
threejs_scene.default <- function(
  ...,
  elements = list(),
  sidebar,
  show_stats = F,
  fps = 20,
  keyframe_shift = 0,
  control_gui = T,
  control_collapsed = T,
  control_animation = T,
  mouse_control = 'orthographic',
  mouse_control_target = c(0,0,0),
  control = 'orthographic',
  background_colors = c('#ffffff', '#efefef'),#c('#efefef', '#fefefe'),
  extra_cameras = list(),
  callback_id = '',
  width = '100%',
  height = '80vh'
) {
  control = mouse_control

  assertthat::assert_that(length(background_colors) == 2, msg = 'background_colors MUST be a length of 2 color vector, example: c("#efefef", "#fefefe")')

  if(!is.list(elements)){
    elements = list(elements)
  }
  elements = c(list(...), elements)
  elements <- sapply(elements, function(el){
    el = el$to_json()
    jsonlite::toJSON(el)
  }, simplify = F, USE.NAMES = F)

  extra_cameras <- lapply(extra_cameras, function(el){
    # extra_cameras = list(
    #   look_at = s[[1]],
    #   position = c(0,0,100)
    # )

    if(!is.character(el$look_at)){
      el$look_at = el$look_at$name
    }

    el
  })

  assertthat::assert_that(length(extra_cameras) == 0 || sum(duplicated(sapply(extra_cameras, '[[', 'look_at'))) == 0,
                          msg = 'Elements in extra_cameras must have distinct mesh_name(s)');

  if(missing(sidebar)){
    sidebar = NULL
  }

  if(length(control) != 1 || !control %in% c('orbit', 'trackball', 'orthographic')){
    control = 'orthographic'
  }

  elements = list(
    show_stats = show_stats,
    fps = fps,
    keyframe_shift = keyframe_shift,
    geoms = elements,
    control_gui= control_gui,
    control_collapsed = control_collapsed,
    control = control,
    control_animation = control_animation,
    mouse_control_target = mouse_control_target,
    extra_cameras = extra_cameras,
    callback_id = callback_id,
    background_colors = t(col2rgb(background_colors)) / 255,
    sidebar = as.character(sidebar)
  )





  # create the widget
  htmlwidgets::createWidget(
    "threejs_scene", elements, width = width, height = height,
    package = 'threejsr')
}

#' @import shiny
#' @export
threejsOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId = outputId, name = "threejs_scene", width, height, package = 'threejsr')
}


#' @export
renderThreejs <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, threejsOutput, env, quoted = TRUE)
}


#  threejs_scene(sidebar = h4('This is title'))
# threejsOutput('output')
# threejs_scene(GeomSphere$new(position = c(100,0,0), mesh_name = 'electrode 1'), show_stats = T)



