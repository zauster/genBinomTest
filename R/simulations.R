
##
## Simulations
##

## testCI
## tests whether the CI from the generalized binomial test (genCI) and
## the CI from the binomial test (binCI) are the same.
## Returns TRUE if they are the same (meaning the difference is less
## than the specified tolerance)

testCI <- function(c, n, alpha = 0.05, threshold = 1e-13)
    ##.Machine$double.eps)
    {
        genCI <- genBinomTestCI(c, n, alpha)
        binCI <- binom.test(c, n, conf.level = 1 - alpha)$conf.int

        ## TRUE TRUE if both are the same, FALSE TRUE if the lower
        ## bound is different, TRUE FALSE if ..., FALSE FALSE if ...
        error <- abs(genCI - binCI) <= threshold

        return(error)
    }

## Loop over these alphas
for(alpha in c(0.01))#, 0.025, 0.05, 0.1))
    {
        ## Loop over these n's
        for(n in c(2:10))#, c(10 + 1:18*5), c(100 + 1:16*25)))
            {
                print(alpha)
                print(n)
                res <- matrix(ncol = 2, nrow = n + 1)

                ## Loop over k (from 0 to n)
                for(k in 0:n)
                    {
                        print(k)
                        res[k + 1, ] <- testCI(k, n, alpha = alpha)
                    }
                ## Create an object with the n and alpha in its name
                assign(paste("res_", n, "_", alpha, sep = ""), res)
            }
    }

## testPattern
## tests whether the TRUE/FALSE pattern of one iteration result is the
## one we would expect (i.e., everywhere TRUE, expect at the second
## (one sucess) and second to last position (n - 1 sucesses))
## Returns TRUE if this is the case
testPattern <- function(mat)
    {
        n <- nrow(mat)
        first <- all(mat[1, ])
        last <- all(mat[n, ])

        ## the lower bound should be different, thus FALSE TRUE
        second <- mat[2, ] == c(FALSE, TRUE)
        ## the upper bound should be different, thus TRUE FALSE
        secondlast <- mat[n - 1, ] == c(TRUE, FALSE)

        middle <- all(mat[3:(n - 2), ])

        first && last && second && secondlast && middle
    }

## Loop over all iterations results and check whether we find the
## expected TRUE/FALSE pattern.
## Prints out the names of the objects that don't fit
for(mat in ls(pattern = "res_"))
    {
        res <- testPattern(eval(parse(text = mat)))
        if(res == FALSE)
            print(mat)
    }

## Should print out nothing...
