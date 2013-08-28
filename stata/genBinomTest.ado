
// generalized binomial test
/* capture program drop genBinomTest */
program genBinomTest, rclass

    version 12.0
    args N succ p
/* syntax N(integer) succ(integer) p(real 0.5) */

    display as text  _newline %~60s "Generalized Binomial test" _newline

    display as text _column(9) "N   Observed k   Expected k   Assumed p   Observed p"
    display as text "{hline 60}"
    display as result %9.0g `N' "   " %9.0f `succ' "    " %9.2fc `N'*`p' "   " %9.6f `p' "   " %9.6f `succ'/`N'

    display as text ""

/* 12345678901234567890123456789012345678901234567890 */
  /* Pr(k >= 4)           = 0.898005  (one-sided test) */
  /* Pr(k <= 4)           = 0.261563  (one-sided test) */
  /* Pr(k <= 4 or k >= 8) = 0.361122  (two-sided test) */

    /* P(k >= succ) */
    local Nsucc = `N' - `succ'
    local oneminusp = 1 - `p'
    calcPvalue `N' `Nsucc' `oneminusp'
    local pvalupper = r(Pvalue)
    display as text _column(3) "Pr(k >= " as result %-1.0f `succ' as text ")" _column(24) "= " as result %8.6f `pvalupper' as text _column(36) "(one-sided test)"

    /* P(k <= succ) */
    calcPvalue `N' `succ' `p'
    local pvallower = r(Pvalue)
    display as text _column(3) "Pr(k <= " as result %-1.0f `succ' as text ")" _column(24) "= " as result %8.6f `pvallower' as text _column(36) "(one-sided test)"

    /* values to be returned */
    return scalar pvallower = `pvallower'
    return scalar pvalupper = `pvalupper'
end


// calculate the Pvalues
/* capture program drop calcPvalue */
program calcPvalue, rclass

    version 12.0
    args N succ p

    /* display `succ' */

    if `succ' <= (`N' * `p' - 1) {
        /* display "B" */
        local res = binomial(`N', `succ', `p')
    }

    else if `succ' < `N' * `p' {
        /* display "Q" */
        local maximalvalue = 0
        forvalues s = 0/`succ' {
            local nps = (`N' * `p' - `s')/(`N' - `s')
            local qvalue = binomial(`N' - `s', `succ' - `s', `nps')

            if `qvalue' > `maximalvalue' {
                local maximalvalue = `qvalue'
            }
        }
        local res = `maximalvalue'
    }

    else {
        /* display "E" */
        local res = 1
    }

    return scalar Pvalue = `res'
end
