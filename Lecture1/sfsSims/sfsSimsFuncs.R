







if(FALSE){

    N <- 1000
    my.freqs <- 1:(2*N-1)/(2*N)
    fspec <- 1/my.freqs


    png('figures/sfs.png',width=6,height=5,units='in',res=300)
    par(mar=c(5.1,5.1,4.1,2.1))
    plot(
        x=my.freqs,
        y=fspec,
        type='l',
        bty='n',
        lwd=2,
        xlab='Allele Frequency',
        ylab='Density',
        cex.lab=1.7
    )
    dev.off()
    
    
}
