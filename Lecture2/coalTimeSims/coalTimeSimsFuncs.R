constructPopHistory <- function(N,t,r=rep(1,length(N))){
    if(!all(r==rep(1,length(N))))
        stop('exponential growth not implemented yet')

    rep(N,times=t)
}
returnPopHistFunc <- function(starting.sizes,epoch.boundaries,growth.rates=rep(1,length(starting.sizes)-1)){
    if(!all(growth.rates==rep(1,length(starting.sizes)-1)))
        stop('exponential growth not implemented yet')
    eb <- c(0,epoch.boundaries)
    popSize <- function(t){
        for(i in 1:(length(eb)-1)){
            if( eb[i] <=t & t < eb[i+1]){
                return(starting.sizes[i])
            } else if (eb[i+1]<=t){
                return(starting.sizes[i+1])
            }
        }
    }
    return(popSize)
}
coalTimeDist <- function(N,eb,gr=NULL){
    if(!is.null(gr))
        stop('growth not implemented yet')

    
    
}
rescaleTimes <- function(ct,st,na){
    ## ct = coalescent scale coalescent times
    ## st = generations on coalescent time scale
    ## na = population size in the ancient epoch
    mt <- max(st) # max scaled coal time before ancient epoch
    mg <- length(st) # number of generations until ancient epoch 
    if(ct<mt){
        sum(ct>st)+1
    } else {
        mg+(ct-mt)*2*na
    }
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
        function(CT) rescaleTimes(CT,scaled.time,Nanc)
    )
    times
}
sizeHistoryPlot <- function(N,start.anc,Nanc,my.xlim){
    gens <- seq_along(N)
    max.gen <- length(N)
    plot(
        gens,
        N,
        type='l',
        bty='n',
        lwd=2,
        ylim=c(0,ceiling((max(N,Nanc)+1)/2000))*2000,
        xlim=my.xlim
    )
    if(start.anc<max(gens)){
        anc.times <- c(max.gen,my.xlim[2])
        lines(
            x=anc.times,
            y=rep(Nanc,length(anc.times)),
            lwd=2
        )
        lines(
            x=rep(max.gen,2),
            y=c(tail(N,1),Nanc),
            lwd=2
        )
    }
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
    hi <- constructPopHistory(c(1000,1000),c(4000,100000))
    coals <- coalTimeSims(hi,1000,reps=10000)
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
