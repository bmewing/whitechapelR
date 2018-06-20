#' @export

show_board = function(paths=NULL,hideouts=NULL,roads,alley,node_locations){
  #' @title Display game board representation
  #'
  #' @description Show a graph representation of the game board with nodes placed in the appropriate relative spot, colored by the number of paths which include a particular node. Possible hideouts are marked with blue squares.
  #'
  #' @param paths optional list of all possible paths already traveled
  #' @param hideouts optional vector of possible hideouts from previous rounds.
  #' @param roads data.frame of non-directional edge pairs for the road graph
  #' @param alley data.frame of non-directional edge pairs for the alley graph
  #' @param node_locations data.frame of where nodes should be placed in the graph
  #'
  #' @return plotted igraph object
  #'
  #' @details
  #' roads, alley and node_locations are all bundled with the package (e.g. \code{data(roads)}).
  #' Solid lines in the graph represent road connections between nodes. Dashed lines represent alley way connections.

  r = roads
  a = alley
  r$lty = 1
  a$lty = 3
  r$weight = 1
  a$weight = 1
  l = as.matrix(node_locations[order(node_locations$name),c("x","y")])
  l[,2] = l[,2]*-1
  v = data.frame(name = 1:195
                ,cex=0.7
                ,color="white"
                ,shape="circle"
                ,stringsAsFactors = FALSE)

  if(!is.null(paths)){
    colors = c("#FFD9D9","#F5C5C5","#ECB1B1","#E39D9D","#D98A8A","#D07676","#C76262","#BE4E4E","#B43B3B","#AB2727","#A21313","#990000")
    tbl = table(unlist(paths))
    to_replace = colors[round(tbl/max(tbl)*10,0)]
    v$color[as.numeric(names(tbl))] = to_replace
  }

  if(!is.null(hideouts)){
    v$shapes[hideouts] = "square"
    v$color[hideouts] = "sky blue"
  }

  g = igraph::graph_from_data_frame(rbind(r,a),directed = FALSE,vertices = v)

  par(mai=c(0,0,0,0))
  plot(g,layout=l,vertex.size = 5)
}
