## library(ape) #this gives the plotting functions for phylogenies
## require(phyclust) #this contains ms (Hudsons program)
## source('plotCoalTrees.R')

#### TODO:

#### fix integral of coalescent time distribution
#### in ancient past so that it is exact rather than truncated at 20*Nanc

### fix issue where lines cut off before edge of plot when population sizes are small


constructPopHistory <- function(N,boundaries,gr=rep(0,length(N)-1)){
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
coalTimeDist <- function(gN,nSamp=2){
    coal.rates <- choose(nSamp,2)/(2*gN)
    int.acc <- cumsum(coal.rates)
    my.exp <- exp(-int.acc)
    cdf <- 1 - my.exp
    pdf <- coal.rates*my.exp
    return(list(cdf=cdf,pdf=pdf))
}
sizeHistoryPlot <- function(N,my.xlim,eb){
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
coalPDFPlot <- function(pdf,my.xlim,mean.coal.time,nSamp=2){
    par(mar=c(5.1,5.1,4.1,2.1))
    plot(
        pdf,
        type='l',
        bty='n',
        lwd=2,
        xlim=my.xlim,
        xlab='',
        ylab=''
    )
    mtext(
        'Generations Ago (T)',
        side=1,
        line=2.8,
        cex=2
    )
    mtext(
        'Density of coalescent times',
        side=2,
        line=2.8,
        cex=2
    )
    ymax <- par('usr')[4]
    abline(v=mean.coal.time,lty=1,lwd=2,col='blue')
    x.seq <- seq(my.xlim[1],my.xlim[2])
    eff.size.coal.dist <- exp(-x.seq/mean.coal.time)/mean.coal.time
  ##  lines(x.seq,eff.size.coal.dist,col='blue',lwd=2,lty=3)
    if(nSamp==2){
        label <- "Average pairwise coalescent time"
    } else {
        label <- paste("Average time to first coalescent event in a sample of",nSamp)
    }

    text(
        x=mean.coal.time*1.1,
        y=0.8*ymax,
        labels=label,
        cex=1.8,
        adj=0
    )
    text(
        x=mean.coal.time*1.1,
        y=0.75*ymax,
        labels=paste(ceiling(mean.coal.time),'generations'),
        cex=1.8,
        adj=0
    )
}
coalCDFPlot <- function(cdf,my.xlim,eb){
    par(mar=c(5.1,5.1,4.1,2.1))
    plot(
        cdf,
        type='l',
        bty='n',
        lwd=2,
        xlim=my.xlim,
        xlab='',
        ylab=''
    )
    mtext(
        'Generations Ago (T)',
        side=1,
        line=2.8,
        cex=2
    )
    mtext(
        'Probability of coalescing before time T',
        side=2,
        line=2.8,
        cex=2
    )
    my.ys <- cdf[eb]
    i=1
    for(i in 1:2){
        lines(
            x=rep(eb[i],2),
            y=c(0,my.ys[i]),
            col='red',
            lty=2,
            lwd=2
        )
        lines(
            x=c(0,eb[i]),
            y=rep(my.ys[i],2),
            col='red',
            lty=2,
            lwd=2
        )
    }

}
if(FALSE){

    ## script to make figures for in class exercise

    N <- c(1000,1000,10000)
    boundaries <- c(1000,5020)
    gr <- c(0.004,0)

    size.history <- constructPopHistory(N,boundaries,gr)
    coal.dist <- coalTimeDist(size.hist)

    my.xmax <- 40000
    ## max.coal <- which.max(coal.dist$cdf>0.99)
    ## om <- round(log(max.coal,10),0)
    my.xlim <- c(0,my.xmax)

    mean.time <- sum(1-coal.dist$cdf)
    mean.rate <- 1/mean.time

    sizeHistoryPlot(
        size.history,
        my.xlim=my.xlim
    )

    mult <- 2
    png('figures/timeDistQuestionDensity.png',height=5*mult,width=8*mult,units='in',res=200)
    coalPDFPlot(
        coal.dist$pdf,
        my.xlim=my.xlim,
        mean.coal.time=mean.time,
        nSamp=2
    )
    dev.off()

    png('figures/timeDistQuestionDistribution.png',height=5*mult,width=8*mult,units='in',res=200)
    coalCDFPlot(
        coal.dist$cdf,
        my.xlim=my.xlim,
        eb=boundaries
    )
    dev.off()



    nsamples <- 10
    ms_options <- '-T'
    ret.ms <- ms(nsam = nsamples, opts = ms_options)
    tree.anc <- read.my.tree(tree = ret.ms[3])
    draw.my.tree(tree.anc)







    layout(matrix(c(1,1,2,2),nrow=2),widths=c(1,1))
    par(mar=c(5.1,4.1,4.1,0))
    phylogram.plot(tree.anc$edge, nsamples, tree.anc$Nnode, xx, yy, horizontal, edge.color, edge.width, edge.lty)
    res1 = plot.phylo(
        tree.anc,
        type="phylogram",
        direction = "leftwards",
        show.tip.label = F,
        root.edge=TRUE
    )
    axis(1)






}
