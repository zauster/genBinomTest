\name{genBinomTest}
\alias{genBinomTest}
\title{
Generalized binomial test
}
\description{
  Performs an exact test of three hypotheses about the probability of
  success in a Bernoulli experiment.
}
\usage{
genBinomTest(x, n, p = 0.5, alpha = 0.05)
}
\arguments{
  \item{x}{
    number of successes in the trials
}
  \item{n}{
    number of trials
}
  \item{p}{
    hypothesized probability of success.
}
  \item{alpha}{
    the type I error
}
}
\details{
  \code{genBinomTest} calculates p-Values and confidence intervals for
independently distributed Bernoulli random variables. The assumption
that these variables are also identically distributed can be relaxed
with this test.

  The confidence intervals are chosen so as to yield the shortest-length
  confidence interval with confidence level at least \eqn{1 - \alpha}.
}
\value{
A list with class "genbinom" containing the following components:

  \item{method}{
    a character string indicating the name and type of the test that was
    performed.
  }
  \item{data.name}{
    a character string giving the name(s) of the data.
  }
  \item{x}{
    number of successes
  }
  \item{n}{
    number of trials
  }
  \item{p}{
    hypothesized probability of success.
  }
  \item{ci.upper, ci.lower, ci.twosided}{
    confidence intervals of the three hypotheses
  }
  \item{alpha}{
    the type I error
  }
  \item{pvalues}{
    p-Values for the three tested hypotheses
  }
}
\references{
Schlag, K. (2013), "Doing Hoeffding's Homework - Tests and Confidence Intervals for the mean of finitely many independently distributed Bernoulli random variables."
}
\author{
Karl Schlag, Oliver Reiter
}
\seealso{
\url{http://homepage.univie.ac.at/karl.schlag/research/statistics/}
}
\examples{
genBinomTest(8, 13, p = 0.85)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{binomial test}
\keyword{independent trials}
