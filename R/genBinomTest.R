
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

## q.binom

## calculates the q-function from the paper
## also for facilitating the reading of the code
q.pbinom <- function(s, c, p, n)
    {
        nps <- ifelse(n*p - s >= 0, (n * p - s)/(n - s), 0)
        pbinom(c - s, n - s, nps)
    }

## genBinomTest

## main function
## calculates the pValues for three hypothesis (greater, smaller,
## equal (= two sided)) for given successes x, total number of
## observations n and probability p
genBinomTest <- function(x, n, p = 0.5, alpha = 0.05)
    {
        ## warnings
        DNAME <- deparse(substitute(x))

        ## calc statistics
        pval.upper <- calcPvalue(n - x, n, 1 - p)
        pval.lower <- calcPvalue(x, n, p)
        pval.twosided <- NA
        if(pval.lower < pval.upper)
            {
                ## print("lower")
                ## find the smallest k s.t. P(X<=x) >= P(X>=k)
                ## and sum the two up
                u1 <- min(n,
                          n - (sum(calcPvalueVec(n - x, n, 1 - p) <= pval.lower) - 1))
                pval.two.upper <- calcPvalue(n - u1, n, 1 - p)
                pval.twosided <- pval.two.upper + pval.lower

                ## see if we can do better:
                ## increase g by 1 if
                ## P(X<=x) + P(X>=u1+g) < P(X<=l2) + P(X>=u1+g-1)
                ## where l2 is the largest value of l s.t.
                ## P(X<=l) <= (P(X<=x) + P(X>=u1+g))/2
                ## then the new p-Value is
                ## P(X<=x) + P(X>=u1+g)
                g <- 0
                ## l <- (pval.lower + calcPvalue(n - u1 + g, n, 1 -
                ## p))/2 == pval.twosided/2
                l2 <- sum(calcPvalueVec(x, n, p) <= pval.twosided/2) - 1
                while(((pval.lower + calcPvalue(n - u1 + g, n, 1 - p)) < (calcPvalue(l2, n, p) + calcPvalue(n - u1 + g - 1, n, 1 - p))) & (u1 + g <= n))
                    {
                        pval.two.upper <- calcPvalue(n - u1 + g, n, 1 - p)
                        pval.twosided <- pval.two.upper + pval.lower
                        g <- g + 1
                        l <- (pval.lower + calcPvalue(n - u1 + g, n, 1 - p))/2
                        l2 <- sum(calcPvalueVec(x, n, p) <= l) - 1
                    }
                ## reset g to its correct value
                g <- max(0, g - 1)
                ## the bounds of P(X<x) and P(X>u1+g)
                twosidedbounds <- c(x, u1 + g)
                ## in some cases, when np - 1<=x<np the pvalue may
                ## reach above one, reset this here
                pval.twosided <- ifelse(pval.twosided <= 1,
                                        pval.twosided, 1)
            }
        else if(pval.lower > pval.upper)
            {
                ## print("upper")
                u1 <- max(0, sum(calcPvalueVec(x, n, p) <= pval.upper) - 1)
                ## print(u1)
                pval.two.lower <- calcPvalue(u1, n, p)
                pval.twosided <- pval.two.lower + pval.upper
                ## print(pval.two.lower)

                g <- 0
                l2 <- sum(calcPvalueVec(n - x, n, 1 - p) <= pval.twosided/2) - 1
                ## cat("l2:", l2, "\n")
                while(((pval.upper + calcPvalue(u1 + g, n, p)) < (calcPvalue(n - l2, n, 1 - p) + calcPvalue(u1 + g - 1, n, p))) & (u1 + g <= n))
                    {
                        pval.two.lower <- calcPvalue(u1 + g, n, p)
                        pval.twosided <- pval.two.lower + pval.upper
                        g <- g + 1
                        ## print(paste("g:", g))
                        l <- (pval.upper + calcPvalue(u1 + g, n, p))/2
                        ## print(paste("l:", l))
                        l2 <- sum(calcPvalueVec(n - x, n, 1 - p) <= l) - 1
                        ## print(paste("l2:", l2))
                    }
                g <- max(0, g - 1)
                twosidedbounds <- c(u1 + g, x)
                pval.twosided <- ifelse(pval.twosided <= 1,
                                        pval.twosided, 1)
            }
        else
            {
                pval.twosided <- 1
                twosidedbounds <- c(x, x)
            }

        method <- "Generalized Binomial Test"
        ## null.hypothesis <- " to be added "
        null.value <- p
        pvalues <- c(pval.upper, pval.lower, pval.twosided)
        names(pvalues) <- c("pval.upper", "pval.lower", "pval.twosided")
        ci.upper <- c(calcCI(x, n, alpha = alpha, side = "lower"), 1)
        ci.lower <- c(0, calcCI(x, n, alpha = alpha, side = "upper"))
        ci.twosided <- c(calcCI(x, n, alpha = alpha/2, side = "lower"),
                         calcCI(x, n, alpha = alpha/2, side = "upper"))

        ## return values
        structure(list(method = method,
                       data.name = DNAME,
                       x = x,
                       n = n,
                       p = p,
                       ## null.hypothesis = null.hypothesis,
                       ## estimate = sample.est,
                       ci.upper = ci.upper,
                       ci.lower = ci.lower,
                       ci.twosided = ci.twosided,
                       conf.level = alpha,
                       pvalues = pvalues,
                       twosidedbounds = twosidedbounds,
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

## email:

## so for the two-sided test we are testing p=0.7 against p<>0.7, in
## fact we are testing p<=0.7 at alpha/2 and testing p>=alpha/2 where
## for the pvalue we search for the smallest value of alpha such that
## one of these two nulls is rejected when observing 6 successes out
## of 10 trials.

## this would mean given Pr(k <= 6) = 0.35 that the pvalue is 0.7. this
## is what many resources in the internet suggest. however one can do
## much better, the pvalue is typically smaller.  however the p value
## will be strictly larger than 2*Pr(k<=5)=0.304.

## consider table for p=0.7:
## Pr(k<=5)=0.1502
## Pr(k<=6)=0.35
## Pr(k<=7)=0.61
## and
## Pr(k>=10)=0.028
## Pr(k>=9)=0.1493
## Pr(k>=8)=0.38

## assume that alpha=0.6. then we search for a cutoff with level 0.3.
## we find 5 as lower and 9 as upper, if we take 5 then we can also
## take 8 as upper as 0.38+0.15=0.53<0.6 and if we take 9 as upper
## then we can also take 6 as lower as 0.35+0.15=0.5<0.6

## so the only questions, to choose between [5,8] and [6,9]. well at
## 0.6 both are fine but if we decrease alpha then we find for
## alpha=0.52 then we need to take either [6,9] or [5,9]. so the
## pvalue for k observed successes is equal to 0.35+0.15=0.5.

## if we instead take alpha=0.48 then we have possible borders 5 and
## 9, the only way to get total prob below 0.48 is to then choose
## both, so interval [5,9] with p value 0.3.

## so for each alpha we consider the upper cutoff below alpha/2 and
## find the lower one that can be chosen to keep total prob below
## alpha, then do the same for the lower cutoff and choose among the
## two intervals the one that has the lowest total probability as the
## interval. this defines a set of intervals and then we look for the
## one where the observed successes lies on the boundary and calculate
## the p value of this interval.

## however the algorithm itself is simpler:

## - find the smaller of Pr(k<=6) and Pr(k>=6), here Pr(k<=6) is smaller
## - assume Pr(k<=6) is smaller than Pr(k>=6)#
# let u1 be the smallest value of u such that Pr(k>=u)<=Pr(k<=6)
## set p value equal to Pr(k<=6)+Pr(k>=u1)
## set g=1, continue to increase g by 1 as long as new p value
## is generated as follows:
## replace p value by Pr(k<=6)+Pr(k>=u1+g) if
## given l2 is the largest value of l such that:
##      Pr(k <= l) <= (Pr(k <= 6) + Pr(k >= u1 + g))/2
## then Pr(k <= 6) + Pr(k >= u1 + g) < Pr(k <= l2) + Pr(k >= u1 + g - 1)
## - analogous if Pr(k<=6) is strictly larger than P(k>=6)
