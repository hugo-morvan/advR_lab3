#' Dijkstra's Algorithm
#'
#' @description The algorithm finds the shortest path from a given node to the other nodes
#' in the graph.
#' @param graph a data frame with columns v1 (from), v2 (to), w (weight)
#' @param init_node the starting node
#' @return the shortest path from init_node to all other nodes
#' @seealso [Dijkstra's Algorithm](https://en.wikipedia.org/wiki/Dijkstra's_algorithm)
#' @export
#' @examples
#' wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'                          v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'                    w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)


dijkstra <- function(graph, init_node) {

  #pseudo code source = https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

  #Checking input
  stopifnot("v1" %in% colnames(graph) && "v2" %in% colnames(graph) && "w" %in% colnames(graph))

  if (nrow(graph) == 0) {
    stop("The graph is empty")
  }
  if (!(init_node %in% graph$v1)){
    stop("The initial node is not in the graph")
  }

  dist <- c()
  prev <- c()
  unvisited <- c()

  num_uniq <- length(unique(graph$v1))

  #initialize distance to infinity
  for (i in 1:num_uniq){
    dist[i] <- Inf
    unvisited[i] <- i
  }
  #Set the distance to zero for the initial node
  dist[init_node] <- 0

  #Set the current node to the initial node
  current <- init_node

  while(length(unvisited) > 0) {

    #find the node in the queue with the smallest dist
    min_dist <- Inf
    for (i in 1:length(unvisited)){
      if (dist[unvisited[i]] < min_dist) {
        min_dist <- dist[unvisited[i]]
        current <- unvisited[i]
      }
    }
    #remove current from Q
    unvisited <- unvisited[unvisited != current]

    #for each neighbor of current
    for (i in 1:nrow(graph)) {
      if (graph[i, 1] == current) {
        neighbor <- graph[i, 2]
        alt <- dist[current] + graph[i, 3] #weight to the the current node + weight of new path
        if (alt < dist[neighbor]) {
          dist[neighbor] <- alt #update the new distance
        }
      }
    }
  }
  #dist is a vector of the shortest distance from the init node to each node
  #ie dist[i] is the shortest distance from the initial node to node i
  return(dist)
}
