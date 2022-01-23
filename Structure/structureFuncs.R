wfBinomWMig <- function(N,ngens,reps,p0,m=0.01,mu=1/(400*N)){
    stopifnot(length(p0)==2)
    freq.list <- list()
    freq.list[[1]] <- matrix(rep(p0,each=reps),ncol=2)


    for(i in 2:ngens){
        ptemp0 <- freq.list[[i-1]]
        ptemp1 <- cbind(
            ptemp0[,1]*(1-m)+ptemp0[,2]*m,
            ptemp0[,2]*(1-m)+ptemp0[,1]*m
        )
        ptemp2 <- ptemp1 + mu*(1-2*ptemp1)

        freq.list[[i]] <- apply(
            ptemp2,
            2,
            function(P)
                rbinom(reps,2*N,P)/(2*N)
        )
    }
    ##my.freqs <- do.call(cbind,freq.list)
    freq.list
}
het <- function(x) {
    tbl <- table(x)
    1 - sum((tbl/sum(tbl))^2)
}
fst <- function(x){
    het <- rowMeans(x*(1-x))
    xbar <- rowMeans(x)
    tothet <- xbar*(1-xbar)
    fst <- (tothet-het)/tothet
    sumtothet <- sum(tothet)
    sumhets <- sum(het)
    meanfst <- (sumtothet-sumhets)/sumtothet
    list(fst,meanfst)
}
meanFst <- function(x){

}

freqPlot <- function(sims,ngens){
    rando <- sample(1:nrow(sims[[1]]),1)
    pop1 <- sapply(sims,function(X)X[,1])
    pop2 <- sapply(sims,function(X)X[,2])
    pop1mean <- colMeans(pop1)
    pop2mean <- colMeans(pop2)

    plot(type="n",y=c(0,1),x=c(0,ngens),xlab="Time, generations",ylab="Frequency, p", cex.lab=1.4,cex.axis=1.2)
    lines(pop1mean,col='red',lwd=1.5,lty=2)
    lines(pop2mean,col='orange',lwd=1.5,lty=2)
    matplot(
        t(pop1),
        type='l',
        lty=1,
        lwd=3/4,
        col=adjustcolor("red",0.1),
        add=T
    )
    matplot(
        t(pop2),
        type='l',
        lty=1,
        lwd=3/4,
        col=adjustcolor("orange",0.1),
        add=T
    )
    lines(pop1[rando,],col='red',lwd=1.5,lty=1)
    lines(pop2[rando,],col='orange',lwd=1.5,lty=1)
    legend(
        x="topleft",
        legend=c(
            "1 simulation from population 1",
            "1 simulation from population 2",
            "Population 1 mean",
            "Population 2 mean"
            ),
        col=c("red","orange","red","orange"),
        lty=c(1,1,2,2),
        lwd=rep(1.5,4),
        bg="white"
    )
}


fstPlot <- function(fsts,mean.fsts,ngens,scaled.mig.rate){

    max.fst <- max(unlist(fsts))
    tmp.ymax <- min(max.fst*1.05,1)
    my.ymax <- ifelse(is.nan(tmp.ymax),1,tmp.ymax)

    plot(
        NA,
        type="n",
        ylim=c(0,my.ymax),
        xlim=c(0,ngens),
        xlab="Time, generations",
        ylab="Fst",
        cex.lab=1.4,
        cex.axis=1.2
    )
    matplot(
        fsts,
        type='l',
        lty=1,
        lwd=3/4,
        col=adjustcolor("black",0.1),
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
