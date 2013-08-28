
## calcCI

calcCI <- function(c, n, alpha = 0.05)
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

## calcPvalue

calcPvalue <- function(c, n, p = 0.5)
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


## q.binom
q.pbinom <- function(s, c, p, n)
    {
        nps <- ifelse(n*p - s >= 0, (n * p - s)/(n - s), 0)
        pbinom(c - s, n - s, nps)
    }

## genBinomTest

genBinomTest <- function(x, n, p = 0.5,
                         alternative = "less")
    {
        ## warnings
        DNAME <- deparse(substitute(x))


        ## calc statistics
        pval.lower <- calcPvalue(x, n, p)
        pval.upper <- calcPvalue(n - x, n, 1 - p)
        ## pval.twosided <-

        method <- "Generalized Binomial Test"
        null.hypothesis <- " to be added "
        ## sample.est <- n * p
        null.value <- p

        ## return values
        structure(list(method = method,
                       data.name = DNAME,
                       x = x,
                       n = n,
                       p = p,
                       null.hypothesis = null.hypothesis,
                       ## estimate = sample.est,
                       pvalues = c(pval.lower, pval.upper),
                       null.value = null.value),
                  class = "genbinom")

    }


##
## old implementation of calcPvalue
##
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
