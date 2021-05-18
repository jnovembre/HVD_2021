moPlot <- function(gen,env,hist.gens,hist.envs,h2,mp_phen=1,xlims.scat=c(-2.5,2.5),ylims.scat=c(-2,2),xlims.gen=c(-3,3),xlims.env=c(-3,3),cex.axis=3,xlab.scat='Genetic Component',ylab.scat='Environmental Component',main.title='Joint distribution of mid-parent genetic and environmental phenotypic components',conditionCounter){

    ## gen <- rnorm(n=num,0,h2)
    ## env <- rnorm(n=num,0,1-h2)
    phen <- gen+env
    noncondition.color <- 'lightgray'
    condition.color <- 'darkred'

    if(mp_phen >= 1){
        these <- phen>mp_phen*0.95 & phen<mp_phen*1.05
    } else if(mp_phen < 1 & mp_phen > 0){
        these <- phen > (mp_phen-0.05) & phen < (mp_phen+0.05)
    } else if(mp_phen < 0) {
        these <- phen<mp_phen*0.95 & phen>mp_phen*1.05
    } else if(mp_phen > 1 & mp_phen < 0){
        these <- phen > (mp_phen-0.05) & phen < (mp_phen+0.05)
    } else if(mp_phen == 0)
        these <- phen>-0.05 & phen<0.05



    disp.mat <- matrix(c(1,1,2,1,1,3),nrow=3)
    layout(mat=disp.mat)
    plot(
        x=gen[!these],
        y=env[!these],
        pch=20,
        cex=1,
        xlim=xlims.scat,
        ylim=ylims.scat,
        xlab='',
        ylab='',
        xaxt='n',
        yaxt='n',
        cex.lab=cex.axis
    )
    abline(
        h=0,
        lty=2
    )
    abline(
        v=0,
        lty=2
    )
    mtext(
        text=ylab.scat,
        side=2,
        line=2.4,
        cex=cex.axis*0.7
    )
    mtext(
        text=xlab.scat,
        side=1,
        line=2.4,
        cex=cex.axis*0.7
    )
    mtext(
        text=main.title,
        side=3,
        line=2.4,
        cex=cex.axis*0.7
    )
    axis(1,at=-3:3,cex.axis=cex.axis)
    axis(2,at=-2:2,cex.axis=cex.axis)
    if(conditionCounter%%2==1){
        abline(
            a=mp_phen,
            b=-1,
            col=condition.color,
            lwd=3
        )
        points(
            x=gen[these],
            y=env[these],
            pch=20,
            col=condition.color
        )
        points(
            x=gen[!these],
            y=env[!these],
            pch=20,
            col=noncondition.color
        )
    }
    else {
        points(
            x=gen[these],
            y=env[these],
            pch=20,
            col='black'
        )
    }

    if(conditionCounter%%2==1){
        ## Genetic histogram
        hist(
            hist.gens,
            breaks=20,
            xlim=xlims.gen,
            xlab='',
            ylab='',
            main='',
            xaxt='n',
            col='lightgray',
            freq=F
        )
        abline(
            v=h2*mp_phen,
            col='black',
            lwd=3
        )
        abline(
            v=mp_phen,
            col=condition.color,
            lty=2,
            lwd=3
        )
        mtext(
            text=xlab.scat,
            side=1,
            line=2.4,
            cex=cex.axis*0.7
        )
        legend(
            'topleft',
            legend=c(
                'midparent phenotype',
                'Avg. midparent genetic component',
                'Distribution of midparent genetic component'
            ),
            col=c(condition.color,'black','black'),
            pt.bg=c(0,0,'lightgray'),
            lty=c(2,1,0),
            pch=c(0,0,22),
            lwd=3,
            pt.cex=c(0,0,3)
        )

        axis(1,at=-2:2,cex.axis=cex.axis)


        ## Environmental histogram
        hist(
            hist.envs,
            breaks=20,
            xlim=xlims.env,
            xlab='',
            ylab='',
            main='',
            xaxt='n',
            freq=F
        )
        abline(
            v=(1-h2)*mp_phen,
            col='black',
            lwd=3
        )
        abline(
            v=mp_phen,
            col=condition.color,
            lty=2,
            lwd=3
        )
        mtext(
            text=ylab.scat,
            side=1,
            line=2.4,
            cex=cex.axis*0.7
        )
        axis(1,at=-2:2,cex.axis=cex.axis)
    }

}
