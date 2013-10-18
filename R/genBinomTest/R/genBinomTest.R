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
                       alpha = alpha,
                       pvalues = pvalues,
                       twosidedbounds = twosidedbounds),
                  class = "genbinom")

    }
