
// generalized binomial test
/* capture program drop genBinomTest */
program genBinomTest, rclass

    version 12.0
    args N x p
/* syntax N(integer) x(integer) p(real 0.5) */

    display as text  _newline %~60s "Generalized Binomial test" _newline

    display as text _column(9) "N   Observed k   Expected k   Assumed p   Observed p"
    display as text "{hline 60}"
    display as result %9.0g `N' "   " %9.0f `x' "    " %9.2fc `N'*`p' "   " %9.6f `p' "   " %9.6f `x'/`N'

    display as text ""

    /* P(k >= x) */
    local Nx = `N' - `x'
    local oneminusp = 1 - `p'
    calcPvalue `N' `Nx' `oneminusp'
    local pvalupper = r(Pvalue)


    /* P(k <= x) */
    calcPvalue `N' `x' `p'
    local pvallower = r(Pvalue)

    /* P(k = x) */
    if `pvallower' < `pvalupper' {
        /* find smallest u st P(X <= x) >= P(X >= u) */
        local u = `N' - `x'
        calcPvalue `N' `u' `oneminusp'
        local pvaltwoupper = r(Pvalue)
        while `pvaltwoupper' > `pvallower' {
            local u = `u' - 1
            calcPvalue `N' `u' `oneminusp'
            local pvaltwoupper = r(Pvalue)
            /* display `u' */
            /* display `pvaltwoupper' */
            if `u' < 0 {
                exit
            }
        }

        local pvaltwosided = `pvallower' + `pvaltwoupper'

        local l = (`pvallower' + `pvaltwoupper')/2
        local l2 = 0
        calcPvalue `N' `l2' `p'
        local test = r(Pvalue)
        while `test' < `l' {
            local l2 = `l2' + 1
            calcPvalue `N' `l2' `p'
            local test = r(Pvalue)
        }

        calcPvalue `N' `x' `oneminusp'
        local pvalplusg = r(Pvalue)
        local xminus1 = `x' - 1
        calcPvalue `N' `xminus1' `oneminusp'
        local pvalplusgminus1 = r(Pvalue)
        local g = 0
        while (`pvallower' + `pvalplusg') < (`test' + `pvalplusgminus1') {
            local pvaltwosided = `pvallower' + `pvalplusg'
            local g = `g' + 1
            local xg = `x' + `g'
            calcPvalue `N' `xg' `p'
            local l = (`pvallower' + r(Pvalue))/2
            local l2 = 0
            calcPvalue `N' `l2' `p'
            local test = r(Pvalue)
            while `test' < `l' {
                local l2 = `l2' + 1
                calcPvalue `N' `l2' `p'
                local test = r(Pvalue)
            }
            calcPvalue `N' `xg' `oneminusp'
            local pvalplusg = r(Pvalue)
            local xgminus1 = `xg' - 1
            calcPvalue `N' `xgminus1' `oneminusp'
            local pvalplusgminus1 = r(Pvalue)
        }
        local g = max(0, `g' - 1)

        local pvaltwosided = min(`pvaltwosided', 1)
        local lowerbound = `x'
        local upperbound = `N' - `u' + `g'
    }

    else if `pvallower' > `pvalupper' {
        local u = `x'
        /* display `u' */
        calcPvalue `N' `u' `p'
        local pvaltwolower = r(Pvalue)
        /* display `pvaltwolower' */
        while `pvaltwolower' > `pvalupper' {
            local u = `u' - 1
            calcPvalue `N' `u' `p'
            local pvaltwolower = r(Pvalue)
            /* display `u' */
            /* display `pvaltwolower' */
            if `u' > `N' {
                exit
            }
        }
        local pvaltwosided = `pvalupper' + `pvaltwolower'

        local l = (`pvalupper' + `pvaltwolower')/2
        local l2 = 0
        calcPvalue `N' `l2' `oneminusp'
        local test = r(Pvalue)
        while `test' < `l' {
            local l2 = `l2' + 1
            calcPvalue `N' `l2' `oneminusp'
            local test = r(Pvalue)
        }

        calcPvalue `N' `x' `p'
        local pvalplusg = r(Pvalue)
        local xminus1 = `x' - 1
        calcPvalue `N' `xminus1' `p'
        local pvalplusgminus1 = r(Pvalue)
        local g = 0
        while (`pvalupper' + `pvalplusg') < (`test' + `pvalplusgminus1') {
            local pvaltwosided = `pvallower' + `pvalplusg'
            local g = `g' + 1
            local xg = `x' + `g'
            calcPvalue `N' `xg' `oneminusp'
            local l = (`pvallower' + r(Pvalue))/2
            local l2 = 0
            calcPvalue `N' `l2' `oneminusp'
            local test = r(Pvalue)
            while `test' < `l' {
                local l2 = `l2' + 1
                calcPvalue `N' `l2' `oneminusp'
                local test = r(Pvalue)
            }
            calcPvalue `N' `xg' `p'
            local pvalplusg = r(Pvalue)
            local xgminus1 = `xg' - 1
            calcPvalue `N' `xgminus1' `p'
            local pvalplusgminus1 = r(Pvalue)
        }
        local g = max(0, `g' - 1)

        local pvaltwosided = min(`pvaltwosided', 1)
        local lowerbound = `u' + `g'
        local upperbound = `x'
    }
    else {
        pvaltwosided = 1
        local lowerbound = `x'
        local upperbound = `x'
    }

    local colnumber = 22 + length(string(`lowerbound')) + length(string(`upperbound'))

    display as text _column(3) "Pr(k >= " as result %-1.0f `x' as text ")" _column(`colnumber') "= " as result %8.6f `pvalupper' as text _column(36) "(one-sided test)"

    display as text _column(3) "Pr(k <= " as result %-1.0f `x' as text ")" _column(`colnumber') "= " as result %8.6f `pvallower' as text _column(36) "(one-sided test)"

    display as text _column(3) "Pr(k <= " as result %-1.0f `lowerbound' as text " or k >= " as result %-1.0f `upperbound' as text ")" _column(`colnumber') "= " as result %8.6f `pvaltwosided' as text _column(36) "(two-sided test)"

    /* values to be returned */
    return scalar pvallower = `pvallower'
    return scalar pvalupper = `pvalupper'
    return scalar pvaltwosided = `pvaltwosided'
end


// calculate the Pvalues
/* capture program drop calcPvalue */
program calcPvalue, rclass

    version 12.0
    args N x p

    /* display `x' */

    if `x' <= (`N' * `p' - 1) {
        /* display "B" */
        local res = binomial(`N', `x', `p')
    }

    else if `x' < `N' * `p' {
        /* display "Q" */
        local maximalvalue = 0
        forvalues s = 0/`x' {
            local nps = (`N' * `p' - `s')/(`N' - `s')
            local qvalue = binomial(`N' - `s', `x' - `s', `nps')

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
