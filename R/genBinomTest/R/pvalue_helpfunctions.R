
## q.binom

## calculates the q-function from the paper
## also for facilitating the reading of the code
q.pbinom <- function(s, c, p, n)
    {
        nps <- ifelse(n*p - s >= 0, (n * p - s)/(n - s), 0)
        pbinom(c - s, n - s, nps)
    }

## calcPvalue

## calculates the pValue for the null that p_alternative <= p
## calcPvalue(4, 10, .5) ## is the same as
## binom.test(4, 10, .5, alt = "less")
calcPvalue <- function(c, n, p)
    {
        ## vectorized version, maybe a bit faster
        ## for better reading the other version though
        res <- ifelse(c <= (n*p - 1),
                      pbinom(c, n, p),
                      ifelse(c < (n*p),
                             max(sapply(0:c, q.pbinom,
                                        c = c, p = p, n = n)),
                             1))
        res
    }

## calcPvalueVec

## calculates pValues for all values from 0 to c,
## to facilitate the code in genBinomTest
calcPvalueVec <- function(c, n, p)
    {
        sapply(0:c, calcPvalue, n = n, p = p)
    }

## calcCI

## calculates the CI, given #succ and #N
## calcCI(5, 10, 0.05, side = "lower")
calcCI <- function(c, n, alpha, side = "upper")
    {
        CI <- function(p, c, n, alpha)
            {
                calcPvalue(c, n, p) - alpha
            }
        if(side == "upper")
            {
                res <- ifelse(c == n, 1,
                                uniroot(CI, interval = c(0, 1),
                                        c = c, n = n, alpha = alpha,
                                        tol = .Machine$double.eps)$root)

            }
        else if(side == "lower")
            {
                res <- 1 - ifelse(c == 0, 1,
                                uniroot(CI, interval = c(0, 1),
                                        c = n - c, n = n, alpha = alpha,
                                        tol = .Machine$double.eps)$root)
            }
        else
            {
                stop("Not a valid option.")
            }

        return(res)
    }
