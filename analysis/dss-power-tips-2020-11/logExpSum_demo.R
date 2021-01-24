################################################################################
# log space is used for higher precision
dpois(x = 5, lambda = 1000)
dpois(x = 5, lambda = 1000, log=T)



################################################################################
# good for multiplication: log(a*b) = log(a) + log(b)
dpois(x = 5, lambda = 1000)^2
2*dpois(x = 5, lambda = 1000, log=T)



################################################################################
# fails for addition: log(a+b)=?
# We want to calculate a + b, but only have accurate log(a) and log(b):
# 
# a + b = exp(log(a)) + exp(log(b))
#
# But exponentiating destroys precision!
dpois(x = 5, lambda = 1000, log=T)
exp(dpois(x = 5, lambda = 1000, log=T))



################################################################################
# So how can we calculate:
dpois(x = 5, lambda = 1000) + dpois(x = 5, lambda = 1000)





















################################################################################
# Solution: keep the largest part in log space!
# suppose a >= b:
# log(a + b) = log(exp(log(a)) + exp(log(b)))
#            = log( exp(log(a)) * (1+exp(log(b)/log(a))) )
#            = log( exp(log(a)) ) + log( 1+exp(log(b)-log(a))) )
#            = log(a) + log1p( exp(log(b)-log(a)) )
logSumExp <- function(x) {
  if(all(is.infinite(x))) { return(x[1]) }
  x = x[which(is.finite(x))]
  ans = x[1]
  for(i in seq_along(x)[-1]) {
    ma = max(ans,x[i])
    mi = min(ans,x[i])
    ans = ma + log1p(exp(mi-ma))
  }
  return(ans)
}

x = c(dpois(x = 5, lambda = 1000, log = T), 
      dpois(x = 5, lambda = 1000, log = T))
logSumExp(x)
