dijkstra <-
function(graph, init_node) {

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
