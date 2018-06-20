#' @export

take_a_step = function(paths,roads){
  #' @title Track one movement
  #'
  #' @description Track one step of unknown movement by Jack, either on roads or through alleyways
  #'
  #' @param paths list of all possible paths already traveled
  #' @param roads data.frame of non-directional edge pairs for either the road graph or the alley graph
  #'
  #' @return list of all possible paths traveled by Jack
  #'
  #' @details
  #' The non-directional edge pairs are available via \code{data(roads)} or \code{data(alley)}
  #' This function does not account for the rule that Jack cannot travel through a road occupied by a police officer.

  paths = lapply(paths,gen_possibilities,roads=roads)
  return(unlist(paths,recursive = FALSE))
}

#' @export

take_a_carriage = function(paths){
  #' @title Track carriage movement
  #'
  #' @description Track two steps of unknown movement by Jack, on roads
  #'
  #' @param paths list of all possible paths already traveled
  #'
  #' @return list of all possible paths traveled by Jack
  data(roads)
  paths = take_a_step(paths,roads)
  paths = take_a_step(paths,roads)
  return(paths)
}

gen_possibilities = function(path,roads){
  start = rev(path)[1]
  p = unique(unlist(roads[roads$x == start | roads$y == start,]))
  p = p[p != start]
  return(lapply(p,add_possibilities,path=path))
}

add_possibilities = function(p,path){
  return(c(path,p))
}
