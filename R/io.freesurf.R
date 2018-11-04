# read ascii freesurfer

#' Read free-surfer files (ascii)
#' @param file file to be read
#' @import stringr
#' @export
read.freesurf.asc <- function(file){
  # file = '/Volumes/data/rave_data/data/YAB_congruency1_sliding/suma/lh.pial.asc'
  src = readLines(file)
  src = src[!str_detect(src, '^[\\ ]*#')]

  # header
  header = as.integer(str_split_fixed(src[1], '\\ ', 2)) # The first element is vertex and the second one is faces

  # Vertices
  vertices = str_split(src[1 + seq_len(header[1])], '[\\ ]+', simplify = T);
  dim = dim(vertices)
  vertices = as.numeric(vertices)
  dim(vertices) = dim

  # faces
  faces = str_split(src[1 + header[1] + seq_len(header[2])], '[\\ ]+', simplify = T);
  dim = dim(faces)
  faces = as.integer(faces)
  dim(faces) = dim

  return(list(
    header = header,
    vertices = vertices,
    faces = faces
  ))
}

#' Read free-surfer files (gifti)
#' @param file file to be read
#' @export
read.freesurf.gii <- function(file){
  file = '/Volumes/data/rave_data/ent_data/congruency/YAH/rave/suma/lh.pial.gii'
  if('gifti' %in% utils::installed.packages()[,1]){
    dat = eval(parse(text = sprintf('gifti::read_gifti(file)')))
    vertices = dat$data[[1]]
    faces = dat$data[[2]]
    header = c(nrow(vertices), nrow(faces))
    return(list(
      header = header,
      vertices = vertices,
      faces = faces
    ))
  }else{
    stop('"gifti" package is needed. Please run `install.packages("gifti")` to install it.')
  }
}
