
## genBinomTestCI

genBinomTestCI <- function(c, n, alpha = 0.05)
    {
        CI <- function(p, c, n, alpha)
            {
                genBinomTest(c, n, p) - alpha
            }
        upper <- ifelse(c == n, 1,
                        uniroot(CI, interval = c(0, 1),
                                c = c, n = n, alpha = alpha/2,
                                tol = .Machine$double.eps)$root)
        lower <- ifelse(c == 0, 1,
                        uniroot(CI, interval = c(0, 1),
                                c = n - c, n = n, alpha = alpha/2,
                                tol = .Machine$double.eps)$root)
        c(1 - lower, upper)
    }

## genBinomTest

genBinomTest <- function(c, n, p = 0.5, alpha = 0.05)
    {
        ## vectorized version, maybe a bit faster
        ## for better reading the other version though
        res <- ifelse(c <= (n*p - 1),
                      pbinom(c, n, p),
                      ifelse(c < (n*p),
                             max(sapply(0:c, q.pbinom,
                                        c = c, p = p, n = n)),
                             1))

        ## if(c <= (n*p - 1))
        ##     {
        ##         ## function B(c, p)
        ##         res <- pbinom(c, n, p)
        ##     }
        ## else
        ##     {
        ##         if(c >= (n*p))
        ##             {
        ##                 ## else return 1
        ##                 res <- 1
        ##             }
        ##         else
        ##             {
        ##                 ## function Q(c, p)
        ##                 res <- max(sapply(0:c, q.pbinom,
        ##                                   c = c, p = p, n = n))
        ##             }
        ##     }

        res

        ## Output
        ## cat("\n")
        ## cat("\tGeneralized binomial test\n")
        ## cat("\ndata: \n")
        ## cat("\tN\tObserved k\tExpected k\n")
        ## cat("\t", n, "\t\t", c, "\t", n*p, "\n")
        ## cat("\tAssumed p\tObserved p\n")
        ## cat("\t", p, "\t", c/n)
        ## cat("\n")
        ## res
    }


## q.binom
q.pbinom <- function(s, c, p, n)
    {
        nps <- ifelse(n*p - s >= 0, (n * p - s)/(n - s), 0)
        pbinom(c - s, n - s, nps)
    }

## qfunc, for debugging purposes
qfunc <- function(c, p, n)
    {
        max(sapply(0:c, q.pbinom, c = c, p = p, n = n))
    }
