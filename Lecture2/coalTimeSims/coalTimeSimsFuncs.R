constructPopHistory <- function(N,t,r=rep(1,length(N))){
    if(!all(r==rep(1,length(N))))
        stop('exponential growth not implemented yet')

    rep(N,times=t)
}
coalTimeSims <- function(N,Nanc,reps=1000){
    ## get elapsed coalescent time per generation
    scaled.time <- cumsum(1/(2*N))
    max.time <- max(scaled.time)
    max.gens <- length(scaled.time)
    ## sim times on natural coalescent timescale
    scaled.cts <- rexp(reps)
    ## convert to generation timescale
    times <- sapply(
        scaled.cts,
        function(CT,mt=max.time,mg=max.gens,na=Nanc){
            if(CT<mt){
                sum(CT>scaled.time)+1
            } else {
                mg+(mt-CT)*2*na
            }
        }
    )
    times
}
sizeHistoryPlot <- function(N,my.xlim){
    gens <- seq_along(N)
    plot(
        gens,
        N,
        type='l',
        bty='n',
        ylim=c(0,ceiling((max(N)+1)/2000))*2000,
        xlim=my.xlim
    )
}
coalTimePlot <- function(times,my.xlim){
    ##recover()
    my.ecdf <- ecdf(times)
    plot(
        my.ecdf,
        xlim=my.xlim
    )

}

if(FALSE){
    hi <- constructPopHistory(c(1000,10000),c(4000,100000))
    coals <- coalTimeSims(hi,5000,reps=10000)
    max.coal <- max(coals)
    om <- round(log(max.coal,10),0)
    my.xlim <- c(0,10^om)



    par(mfrow=c(2,1))
    sizeHistoryPlot(
        hi,
        my.xlim=my.xlim
    )
    coalTimePlot(
        coals,
        my.xlim=my.xlim
    )
}
