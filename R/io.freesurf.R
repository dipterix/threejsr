# read ascii freesurfer

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
