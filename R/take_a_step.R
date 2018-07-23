#' @export

take_a_step = function(paths,roads,blocked = NULL){
  #' @title Track one movement
  #'
  #' @description Track one step of unknown movement by Jack, either on roads or through alleyways
  #'
  #' @param paths list of all possible paths already traveled
  #' @param roads data.frame of non-directional edge pairs for either the road graph or the alley graph
  #' @param blocked list of node pairs which cannot be traversed because a police officer blocks it (should not be used for special movement)
  #'
  #' @return list of all possible paths traveled by Jack
  #'
  #' @details
  #' The non-directional edge pairs are available via \code{data(roads)} or \code{data(alley)}
  #' This function does not account for the rule that Jack cannot travel through a road occupied by a police officer.

  paths = lapply(paths,gen_possibilities,roads=roads,blocked=blocked)
  paths = unlist(paths,recursive = FALSE)

  return(paths)
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
  data(roads,envir = environment())
  paths = take_a_step(paths,roads)
  paths = take_a_step(paths,roads)
  return(paths)
}

#' @export

trim_possibilities = function(paths,node){
  #' @title Trim possible paths
  #'
  #' @description Remove known impossible end points for Jack, typically as a result of having found, but not arrested Jack.
  #'
  #' @param paths list of all possible paths already traveled
  #' @param node list of vectors of length 1 or 2 which specifies blocked nodes due to the presence of a policeman
  #'
  #' @return list of trimmed possible paths traveled by Jack
  keep = unlist(lapply(paths,function(x){!identical(sort(rev(x)[seq_along(node)]),sort(node))}))
  return(paths[keep])
}

gen_possibilities = function(path,roads,blocked = NULL){
  start = rev(path)[1]
  p = unique(unlist(roads[roads$x == start | roads$y == start,]))
  p = p[p != start]
  full_paths = lapply(p,add_possibilities,path=path)
  if(!is.null(blocked)){
    r = lapply(blocked,trim_possibilities,paths=full_paths)
    full_paths = Reduce(intersect,r,init=r[[1]])
  }
  return(full_paths)
}

add_possibilities = function(p,path){
  return(c(path,p))
}
