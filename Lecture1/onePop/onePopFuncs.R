require('reticulate')
dadi <- import('dadi')


constructPopHistory <- function(N,boundaries,gr=rep(0,length(N)-1)){
    ## copied from coal time sims exercise
    if(any(diff(boundaries)<0))
        stop('boundaries must be strictly increasing')
    eb <- boundaries
    el <- diff(c(0,eb)) # get length of each epoch in generations
    Nanc <- tail(N,1)
    gt <- lapply(diff(c(0,eb)),function(TIME) rev(0:(TIME-1)))
    gf <- exp(unlist(gt)*rep(gr,sapply(gt,length)))
    size.history <- c(rep(head(N,-1),times=el)*gf,rep(Nanc,20*Nanc))
    return(size.history)
}

sizeHistoryPlot <- function(N,my.xlim,eb){
    ## copied from coal time sims exercise
    gens <- seq_along(N)
    par(mar=c(5.1,5.1,4.1,2.1))
    plot(
        gens,
        N,
        type='l',
        bty='n',
        lwd=2,
        ylim=c(0,ceiling((max(N)+1)/2000))*2000,
        xlim=my.xlim,
        xlab='',
        ylab=''
    )
    mtext(
        'Generations Ago',
        side=1,
        line=2.8,
        cex=2
    )
    mtext(
        'Population Size',
        side=2,
        line=2.8,
        cex=2
    )
}


if(FALSE){

    spec <- dadi$Demographics1D$bottlegrowth(c(1,1,0.01),list(as.integer(200)),as.integer(2000))
    spec


    plot(
        spec,
        type='p',
        bty='n',
        pch=20,
        log='xy'
    )

}
