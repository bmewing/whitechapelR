#' @title Undirected edge pairing of roads connecting nodes
#' @description Data used to establish possible connections used by Jack between nodes
#' @format A data frame with 767 rows and 2 variables
#' \describe{
#'   \item{x}{The smaller integer of the pair of vertices}
#'   \item{y}{The larger integer of the pair of vertices}
#' }
"roads"

#' @title Undirected edge pairing of alley connecting nodes
#' @description Data used to establish possible connections used by Jack between nodes via alleyways
#' @format A data frame with 452 rows and 2 variables
#' \describe{
#'   \item{x}{The smaller integer of the pair of vertices}
#'   \item{y}{The larger integer of the pair of vertices}
#' }
"alley"

#' @title x,y coordinates of node points from the game board
#' @description Data used to place nodes in graphical output according to their relative positions on the game board
#' @format A data frame with 195 rows and 4 variables
#' \describe{
#'   \item{id}{An artifact of the computer vision process used to obtain coordinates}
#'   \item{x}{The number of pixels from the left edge of the board to the center of the node}
#'   \item{y}{The number of pixels from the top edge of the board to the center of the node}
#'   \item{name}{The integer assigned to the node on the game board}
#' }
"node_locations"
