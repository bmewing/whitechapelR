#' @export

end_round = function(paths,hideouts=NULL){
  #' @title Manage list of possible hideouts
  #'
  #' @description Create or update a list of possible hideouts based on final positions from the list of possible paths traveled.
  #'
  #' @param paths list of all possible paths already traveled
  #' @param hideouts optional vector of possible hideouts from previous rounds. Not used in round 1, only rounds 2 and 3
  #'
  #' @return list of all possible hideouts
  #' @examples
  #' possibilities = start_round(64)
  #' possibilities = take_a_step(possibilities,roads)
  #' possibilities = take_a_step(possibilities,roads,blocked=list(c(63,82),c(63,65)))
  #' possibilities = inspect_space(possibilities,space = c(29,30), clue = FALSE)
  #' possibilities = inspect_space(possibilities,space = 49, clue = TRUE)
  #' hideouts = end_round(possibilities,hideouts=NULL)
  #' possibilities = start_round(67)
  #' possibilities = take_a_step(possibilities,roads)
  #' hideouts = end_round(possibilities,hideouts=hideouts)

  possible_hideouts = lapply(paths,function(x){
    rev(x)[1]
  })
  possible_hideouts = unique(unlist(possible_hideouts))
  if(is.null(hideouts)) return(sort(possible_hideouts))
  hideouts = intersect(hideouts,possible_hideouts)
  return(sort(hideouts))
}
