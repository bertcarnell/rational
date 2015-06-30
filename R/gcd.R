# Greatest Common Denominator
# 
# http://tolstoy.newcastle.edu.au/R/e2/help/07/04/14709.html
# 
# param a first integer of a pair
# param b second integer of a pair
.gcd <- function(a, b) ifelse(b==0, a, .gcd(b, a %% b))
