rqpois = function(n, mu, scale) {
mu = lambda
k = mu/scale/(1-1/scale)

r1 = rnbinom(n, mu = mu, size = k)
r2 = rnbinom(n, size=k,prob=1/scale)
r = cbind(r1,r2)
return(r)
}


