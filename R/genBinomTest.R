
## calcCI

calcCI <- function(c, n, alpha)
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

calcPvalueVec <- function(c, n, p)
    {
        sapply(0:c, calcPvalue, n = n, p = p)
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
## - assume Pr(k<=6) is smaller than Pr(k>=6)
## let u1 be the smallest value of u such that Pr(k>=u)<=Pr(k<=6)
## set p value equal to Pr(k<=6)+Pr(k>=u1)
## set g=1, continue to increase g by 1 as long as new p value
## is generated as follows:
## replace p value by Pr(k<=6)+Pr(k>=u1+g) if
## given l2 is the largest value of l such that:
##      Pr(k <= l) <= (Pr(k <= 6) + Pr(k >= u1 + g))/2
## then Pr(k <= 6) + Pr(k >= u1 + g) < Pr(k <= l2) + Pr(k >= u1 + g - 1)
## - analogous if Pr(k<=6) is strictly larger than P(k>=6)

## so we find [6,9] and not [6,10] as 0.35+0.028>=0.1502+0.1493

## illustration: assume table
## Pr(k<=5)=0.17
## Pr(k<=6)=0.35
## Pr(k<=7)=0.61
## and
## Pr(k>=10)=0.028
## Pr(k>=9)=0.3
## Pr(k>=8)=0.38

## then we get [6,10] as Pr(k <= 6) + Pr(k >= 10) = 0.38 < 0.47 = Pr(k
## <= 5) + Pr(k >= 9) and hence pvalue 0.38

## note that the (1-alpha)*100% confidence interval is easier to
## find. just look for the two p values that make Pr(k<=6)=alpha/2 and
## Pr(k>=6)=alpha/2.

        ## calc statistics
        pval.upper <- calcPvalue(n - x, n, 1 - p)
        pval.lower <- calcPvalue(x, n, p)
        pval.twosided <- NA
        if(pval.lower < pval.upper)
            {
                u <- calcPvalueVec(n - x, n, 1 - p)
                ## print(u)
                u1 <- n - (sum(u <= pval.lower) - 1)
                ## print(paste("u1 1:", u1))
                pval.two.upper <- calcPvalue(n - u1, n, 1 - p)
                ## print(pval.two.upper)
                pval.twosided <- pval.two.upper + pval.lower

                g <- 0
                l <- (pval.lower + calcPvalue(n - u1 + g, n, 1 - p))/2
                ## print(paste("l 1:", l))
                l2 <- sum(calcPvalueVec(x, n, p) <= l)
                ## print(paste("l2 1:", l2))
                while(((pval.lower + calcPvalue(n - u1 + g, n, 1 - p)) < (calcPvalue(l2, n, p) + calcPvalue(n - u1 + g - 1, n, 1 - p))) & (u1 + g <= n))
                    {
                        ## print(paste("g 1:", g))
                        pval.two.upper <- calcPvalue(n - u1 + g, n, 1 - p)
                        pval.twosided <- pval.two.upper + pval.lower
                        g <- g + 1
                        l <- (pval.lower + calcPvalue(n - u1 + g, n, 1 - p))/2
                        ## print(paste("l 1:", l))
                        l2 <- sum(calcPvalueVec(x, n, p) <= l)
                        ## print(l2)
                    }
                twosidedbounds <- c(x, u1 + g - 1)
                pval.twosided <- ifelse(pval.twosided <= 1,
                                        pval.twosided, 1)
            }
        else if(pval.lower > pval.upper)
            {
                u <- calcPvalueVec(x, n, p)
                ## print(u)
                u1 <- sum(u <= pval.upper)
                ## print(paste("u1 2:", u1))
                pval.two.lower <- calcPvalue(u1, n, p)
                ## print(pval.two.lower)
                pval.twosided <- pval.two.lower + pval.upper

                g <- 0
                l <- (pval.upper + calcPvalue(u1 + g, n, p))/2
                ## print(l)
                l2 <- sum(calcPvalueVec(n - x, n, 1 - p) <= l)
                ## print(paste("l2 2:", l2))
                while(((pval.upper + calcPvalue(u1 + g, n, p)) < (calcPvalue(n - l2, n, 1 - p) + calcPvalue(u1 + g - 1, n, p))) & (u1 + g <= n))
                    {
                        ## print(paste("g 2:", g))
                        pval.two.lower <- calcPvalue(u1 + g, n, p)
                        pval.twosided <- pval.two.lower + pval.upper
                        g <- g + 1
                        l <- (pval.upper + calcPvalue(u1 + g, n, p))/2
                        ## print(paste("l 2:", l))
                        l2 <- sum(calcPvalueVec(n - x, n, 1 - p) <= l)
                        ## print(l2)
                    }
                twosidedbounds <- c(u1 + g - 1, x)
                pval.twosided <- ifelse(pval.twosided <= 1,
                                        pval.twosided, 1)
            }
        else
            {
                pval.twosided <- 1
                twosidedbounds <- c(x, x)
            }

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
                       pvalues = c(pval.lower, pval.upper, pval.twosided),
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
