\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    stopifnot("v1" \%in\% colnames(graph) && "v2" \%in\% colnames(graph) && 
        "w" \%in\% colnames(graph))
    if (nrow(graph) == 0) {
        stop("The graph is empty")
    }
    if (!(init_node \%in\% graph$v1)) {
        stop("The initial node is not in the graph")
    }
    dist <- c()
    prev <- c()
    unvisited <- c()
    num_uniq <- length(unique(graph$v1))
    for (i in 1:num_uniq) {
        dist[i] <- Inf
        unvisited[i] <- i
    }
    dist[init_node] <- 0
    current <- init_node
    while (length(unvisited) > 0) {
        min_dist <- Inf
        for (i in 1:length(unvisited)) {
            if (dist[unvisited[i]] < min_dist) {
                min_dist <- dist[unvisited[i]]
                current <- unvisited[i]
            }
        }
        unvisited <- unvisited[unvisited != current]
        for (i in 1:nrow(graph)) {
            if (graph[i, 1] == current) {
                neighbor <- graph[i, 2]
                alt <- dist[current] + graph[i, 3]
                if (alt < dist[neighbor]) {
                  dist[neighbor] <- alt
                }
            }
        }
    }
    return(dist)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
