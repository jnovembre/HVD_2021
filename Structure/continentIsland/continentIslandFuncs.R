wfBinomWMig <- function(N,ngens,reps,pCont,pIsle,m=0.01){
    freq.list <- list()
    freq.list[[1]] <- rep(pIsle,each=reps)
    for(i in 2:ngens){
        ptemp <- freq.list[[i-1]]*(1-m)+pCont*m
        freq.list[[i]] <- rbinom(reps,2*N,ptemp)/(2*N)
    }
    freq.list
}
het <- function(x) {
    tbl <- table(x)
    1 - sum((tbl/sum(tbl))^2)
}
fst <- function(x,p){
    het <- 0.5 * (x*(1-x) + p*(1-p))
    xbar <- (x+p)/2
    tothet <- xbar*(1-xbar)
    fst <- (tothet-het)/tothet
    sumtothet <- sum(tothet)
    sumhets <- sum(het)
    meanfst <- (sumtothet-sumhets)/sumtothet
    list(fst,meanfst)
}
freqPlot <- function(sims,pCont,ngens=length(my.sims)){
    rando <- sample(1:length(sims[[1]]),1)
    freq.mat <- do.call(rbind,sims)
    freq.mat.mean <- rowMeans(freq.mat)

    plot(type='n',y=c(0,1),x=c(0,ngens),xlab='Time, generations',ylab='Frequency, p', cex.lab=1.4,cex.axis=1.2)
    abline(h=pCont,lty=2,lwd=2,col='blue')
    lines(freq.mat.mean,col='orange',lwd=2,lty=2)
    matplot(
        freq.mat,
        type='l',
        lty=1,
        lwd=3/4,
        col=adjustcolor('red',0.1),
        add=T
    )
    lines(freq.mat[,rando],col='red',lwd=1.5,lty=1)
    legend(
        x='topleft',
        legend=c(
            '1 simulation from island population',
            'Island mean',
            'Continent mean'
            ),
        col=c('red','orange','blue'),
        lty=c(1,2,2),
        lwd=rep(1.5,2,2),
        bg='white'
    )

}


fstPlot <- function(fsts,mean.fsts,ngens,scaled.mig.rate){

    max.fst <- max(unlist(fsts))
    tmp.ymax <- min(max.fst*1.05,1)
    my.ymax <- ifelse(is.nan(tmp.ymax),1,tmp.ymax)

    plot(
        NA,
        type='n',
        ylim=c(0,my.ymax),
        xlim=c(0,ngens),
        xlab='Time, generations',
        ylab='Fst',
        cex.lab=1.4,
        cex.axis=1.2
    )
    matplot(
        fsts,
        type='l',
        lty=1,
        lwd=3/4,
        col=adjustcolor('black',0.1),
        add=T
    )
    lines(
        mean.fsts,
        lwd=1.5,
        lty=1
    )
    abline(
        h=1/(1+scaled.mig.rate),
        lwd=1.5,
        lty=2
    )

}
