barplot_dots<-function(treatment,data,main=NULL,xlab=NULL,ylab=NULL,sig_codes=NULL,ylim=NULL,xpd=NA,cex.axis=NULL,cex.lab=NULL,las=NULL,sig.cex=NULL,cex=1,...)
{
    for_barplot=aggregate(data~treatment, FUN=mean)
    for_barplot_sd=aggregate(data~treatment, FUN=sd)
    
    if(is.null(ylim))
    {
        ylim=c(0,1.2*max(data))
        
    }
    x=barplot_with_errorbars(for_barplot$data,sd_height=for_barplot_sd$data,names.arg=for_barplot$treatment,xlab=xlab,ylab=ylab,main=main,
    sig_codes=sig_codes,ylim=ylim,xpd=xpd,cex.axis=cex.axis,cex.lab=cex.lab,las=las,sig.cex=sig.cex)
    
    
    
    
    
    plot_dot_column(x[match(treatment,for_barplot$treatment)],data,cex=cex,...)
    
    return(x)
    
    
}
