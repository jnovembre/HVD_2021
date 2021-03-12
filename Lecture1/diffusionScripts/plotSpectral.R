## package for loading python modules in R
require('reticulate')
source_python('spectralFuncs.py')


N <- 1e4
mu <- 1e-8
my.x <- 1:(2*N-1)/(2*N)
my.ts <- 0:(6*N)


blah <- lapply(
    my.ts,
    function(TIME)
        spectralTDF(1/(2*N),TIME,N,mu,mu,deltaY=1/(2*N))
)

my.tdfs <- do.call(rbind,blah)
dim(my.tdfs)

plot(
    my.x,
    blah,
    type='l'
)




