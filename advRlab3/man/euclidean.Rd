\name{euclidean}
\alias{euclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
euclidean(input1, input2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input1}{
%%     ~~Describe \code{input1} here~~
}
  \item{input2}{
%%     ~~Describe \code{input2} here~~
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
function (input1, input2) 
{
    stopifnot(is.numeric(input1), is.vector(input1), length(input1) == 
        1, is.numeric(input2), is.vector(input2), length(input2) == 
        1)
    min_inp <- min(input1, input2)
    max_inp <- max(input1, input2)
    remainder <- max_inp\%\%min_inp
    repeat {
        if (remainder != 0) {
            max_inp <- min_inp
            min_inp <- remainder
            remainder <- max_inp\%\%min_inp
        }
        else {
            return(abs(min_inp))
            break
        }
    }
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
