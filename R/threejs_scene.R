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
  control = 'trackball',
  width = '100%',
  height = '80vh'
) {
  if(!is.list(elements)){
    elements = list(elements)
  }
  elements = c(list(...), elements)
  elements <- sapply(elements, function(el){
    if(is(el, 'TGeom')){
      el = as.list(el)
    }
    el
  }, simplify = F, USE.NAMES = F)

  aside_class = ''
  if(missing(sidebar)){
    # aside_class = 'hidden'
    sidebar = NULL
  }

  if(length(control) != 1 || !control %in% c('orbit', 'trackball')){
    control = 'trackball'
  }

  elements = list(
    show_stats = show_stats,
    fps = fps,
    geoms = elements,
    control = control,
    sidebar = as.character(
      div(
        class = paste('threejs-scene-aside', aside_class),
        style = sprintf('height: %s', height),
        fluidRow(
          column(
            12,
            sidebar,
            div(class = 'threejs-scene-control hidden'),
            div(class = 'threejs-scene-info')
          )
        )
      )
    )
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



