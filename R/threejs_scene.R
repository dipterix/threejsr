#' Main function to render threejsr scene
#' @param ... TGeom objects
#' @param elements In case you have a list of TGeom objects
#' @param sidebar Default NULL, customized html string for additional sidebars
#' @param show_stats show threejs render statustics
#' @param fps animation default start fps
#' @param keyframe_shift animation keyframe display shift. See details
#' @param control_gui show control panel or not
#' @param control_collapsed if show control panel, is control panel collapsed by default
#' @param control_animation if show control panel, show animation section or not
#' @param mouse_control default mouse control, "trackball", "orbit", or "orthographic", default is "orthographic"
#' @param mouse_control_target mouse control rotate around target position, default is origin
#' @param control ignored
#' @param background_colors render scene background color. First is the main scene, second is for side-cameras
#' @param main_camera a named list to set initial main camera params see details
#' @param extra_cameras side-cameras
#' @param callback_id shiny input ID, used along with extra_data method to generate shiny callbacks
#' @param width widget width, default is 100\%
#' @param height widget height, default is 600px - full screen height
#' @details
#' 1. Animation:
#'
#' Each TGeom objects has a method animation_event which takes 7 params, in
#' which "event_data", "key_frames" are the most important params.
#'
#' "key_frames" defines the time. Due to the implementation, key_frames must be
#' positive numeric vectors. "keyframe_shift" allows the displayed "key_frames"
#' in animation panel be negative by shifting the keyframes.
#'
#' "event_data" should be a matrix defines the color, whose nrow(.) should equals
#' to length(key_frames). The number of columns of event_data defines the color
#' mode: one column is usually grey-scale, three columns is rgb color, and four
#' columns is with transparency. To tell threejs your color mode, use "pixel_size"
#' to indicate the number of columns. (this will be auto-detected in the future)
#'
#' 2. Cameras:
#'
#' There are two types of cameras in the canvas: main camera and side cameras.
#'
#' "main_camera" parameter should be a list indicating initial camera params.
#' An example would be:
#'
#' \code{main_camera = list(position = c(0,0,500), up = c(0,1,0), zoom = 1)}
#'
#' position: initial position (x,y,z), here the camera is at z-axis
#' up: camera up-direction, the screen top is y axis
#' zoom: zoom-in level, here is 1, no zoom
#'
#' "extra_cameras" defines side-cameras. It should be a list of named lists.
#' An example would be:
#'
#' \code{phere = geom_sphere(position = c(1,2,300), 'ball 1', radius = 10, layer = 3)}
#'
#' \code{extra_cameras = list(
#'   list(look_at = sphere, position = c(-200, 0, 0)))}
#'
#' sphere is a GeomSphere object. We added a side camera focusing on the sphere
#' no matter how you move the main camera.
#'
#' 3. Layer and visibility:
#'
#' All geom objects has attribute "layer", you can pass layer param during the
#' initialize process. To enable visibility in main-camera, layer must be from 1
#' to 4. For side-cameras, layers must be from 3 or 4.
#' @examples
#' \dontrun{
#' sphere = geom_sphere(position = c(1,2,300), mesh_name =  'ball 1',
#'                      radius = 10, layer = c(1,3))
#' plane = geom_plane(
#'   position = c(0,0,0), mesh_name = 'plane', width = 100, height = 50,
#'   mesh_info = '<span style="color:red">Colored info</span>',
#' )
#' # Set hook
#' sphere$set_hook(c(0,0,0))
#'
#' # Use rgba color
#' pixel_size = 4 # 4 for rgba
#'
#' sphere$animation_event(
#'   name = 'ani color - sphere',
#'   event_data = matrix(c(
#'     0,0,1,1,
#'     1,0,1,0.3,
#'     1,1,1,0.8
#'   ), ncol = pixel_size, byrow = T),
#'   key_frames = 1:3,
#'   pixel_size = pixel_size,
#'   alpha = T   # show alpha
#' )
#'
#' plane$animation_event(
#'   name = 'ani color - plane',
#'   event_data = matrix(c(
#'     0,0,1,0.8
#'   ), ncol = pixel_size, byrow = T),
#'   key_frames = 1,
#'   pixel_size = pixel_size,
#'   alpha = T   # show alpha
#' )
#'
#' threejs_scene(
#'   sphere,
#'   plane,
#'   show_stats = T,
#'   extra_cameras = list(
#'   list(look_at = sphere, position = c(-200, 0, 0)),
#'       list(look_at = plane, position = c(0,-200, 0))
#'      )
#' )
#' }
#' @import htmlwidgets
#' @export
threejs_scene <- function(
  ...,
  elements = list(),
  sidebar,
  show_stats = F,
  fps = 1,
  keyframe_shift = 0,
  control_gui = T,
  control_collapsed = T,
  control_animation = T,
  mouse_control = 'orthographic',
  mouse_control_target = c(0,0,0),
  control = 'orthographic',
  background_colors = c('#ffffff', '#efefef'),#c('#efefef', '#fefefe'),
  main_camera = NULL,
  # list(
  #   position = c(0,0,500),
  #   up = c(0,1,0),
  #   zoom = 1
  # ), # look from z=500 to 0,0,0
  extra_cameras = list(),
  callback_id = '',
  width = '100%',
  height = '100vh'
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
    main_camera = main_camera,
    extra_cameras = extra_cameras,
    callback_id = callback_id,
    background_colors = t(grDevices::col2rgb(background_colors)) / 255,
    sidebar = as.character(sidebar)
  )





  # create the widget
  htmlwidgets::createWidget(
    "threejs_scene", elements, width = width, height = height,
    package = 'threejsr')
}

#' Shiny-threejsr output
#' @param outputId character ID for shiny outputs
#' @param width css width default '100\%'
#' @param height css height default '400px
#' @import shiny
#' @export
threejsOutput <- function(outputId, width = "100%", height = "600px") {
  htmlwidgets::shinyWidgetOutput(outputId = outputId, name = "threejs_scene", width, height, package = 'threejsr')
}


#' Render shiny-threejsr scene
#' @param expr expression to be evaluated
#' @param env runtime environment see shiny outputs
#' @param quoted is expr quoted?
#' @export
renderThreejs <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, threejsOutput, env, quoted = TRUE)
}


#  threejs_scene(sidebar = h4('This is title'))
# threejsOutput('output')
# threejs_scene(GeomSphere$new(position = c(100,0,0), mesh_name = 'electrode 1'), show_stats = T)



