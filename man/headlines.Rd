% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/headlines-make.R
\name{headlines}
\alias{headlines}
\title{Make all headlines used in the experiment}
\usage{
headlines(news_type = "hard")
}
\arguments{
\item{news_type}{What news type to get, either \code{hard} (default) or \code{soft}.}
}
\description{
Make all headlines used in the experiment. Uses \code{headlines_data()} and \code{headlines_make_from_data()}. Returns a data frame with the headline, as well as associated id (linked to the data) and variables identifying the integrated treatment component.
}
\examples{
\dontrun{
 # Get and print the hard news headlines
 hard_news <- headlines_all("hard")
 print(hard_news$headline)

 # Get and print the soft news headlines
 soft_news <- headlines_all("soft")
 print(soft_news$headline[1])
}
}
\keyword{headlines}
\keyword{prepublication}

