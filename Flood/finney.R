# Adjusted (Unbiased) MLE ----
# From Finney 1941 "On the Distribution of a Variate Whose Logarithm is Normally Distributed"

# Bias correction factor ----
.finney_series = function(x, n, degree) {
    if (degree <= 1) {
        g_1 =  ((n - 1) / n) * x
        return(g_1)
    }
    else {
        g_ = .finney_series(x, n, degree-1)
        g = g_ + g_ *   
            (n - 1)^2 / (n * degree) *
            x / (n + 2 * degree - 3)
        return(g)
    }
}
finney_series = function(x, n, degree = 500) {
    return(1 + .finney_series(x, n, degree))
}