#' @export

inspect_space = function(paths,space,clue){
  #' @title Update paths based on inspections
  #'
  #' @description Updated the list of possible paths based on the results of police investigation
  #'
  #' @param paths list of all possible paths already traveled
  #' @param space vector of integers of the spaces inspected
  #' @param clue single logical value indicating if evidence of Jack was found
  #'
  #' @return list of all possible paths traveled by Jack
  #' @examples
  #' possibilities = start_round(64)
  #' possibilities = take_a_step(possibilities,roads)
  #' possibilities = take_a_step(possibilities,roads,blocked=list(c(63,82),c(63,65)))
  #' possibilities = inspect_space(possibilities,space = c(29,30), clue = FALSE)
  #' possibilities = inspect_space(possibilities,space = 49, clue = TRUE)

  if(clue){
    paths = lapply(paths,found_clue,space=space)
  } else {
    paths = lapply(paths,found_nothing,space=space)
  }
  return(plyr::compact(paths))
}

found_clue = function(path,space){
  if(!any(path %in% space)) return(NULL)
  return(path)
}

found_nothing = function(path,space){
  if(any(path %in% space)) return(NULL)
  return(path)
}
