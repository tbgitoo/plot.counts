multipleHistograms<-function(x,levels, breaks ="Sturges",overhead=0.2,xlab="",category_labels=NULL,add.legend=FALSE,legend.text=NULL,
	legend.args=vector(mode="list",length=0),barcolors=NULL,cex.axis=1,FUN=NULL,...)
{
	if(is.factor(levels))
    {
        risk_levels=1:length(levels(levels))
        
    } else
    {
        risk_levels = sort(unique(levels))
    }
	if(is.null(barcolors))
	{
		barcolors=grey(1-risk_levels/max(risk_levels))
	}
	ymin=(max(risk_levels)-risk_levels)/(max(risk_levels)+1)*(1-overhead)
	
	ymax=ymin+1/(max(risk_levels)+1)
	
	ymax=1-max(ymax)+ymax
	
	
	
	for(theLevelIndex in 1:length(risk_levels))
	{
		relLevelIndex = theLevelIndex/length(risk_levels)
		
		par(fig=c(0,1,ymin[theLevelIndex],ymax[theLevelIndex]), new=theLevelIndex!=1)
		
		theLevel=risk_levels[theLevelIndex]
        if(is.factor(levels))
        {
            theLevel=levels(levels)[theLevel]
        }
		info=hist(x[levels==theLevel],breaks=breaks,plot=FALSE)
		thecounts=matrix(nrow=1,data=info$counts)
		if(theLevelIndex==1 & length(breaks)==1)
		{
				breaks=info$breaks
		}
		xlab_to_plot=NULL
		if(theLevel==max(risk_levels))
		{
			xlab_to_plot=xlab
		}
		x_vals=barplot(thecounts,col=barcolors[theLevelIndex],xlab=xlab_to_plot,...)
		
		if(!is.null(FUN))
		{
			FUN(level=theLevel,histogram_info=info,x=x_vals)	
		}
		
		odds=seq(from=1,to=length(breaks)-1,step=2)
		
		if(is.null(category_labels))
		{
			category_labels=vector(mode="numeric",length=length(breaks)-1)
			category_labels=(breaks[1:(length(breaks)-1)]+breaks[2:length(breaks)])/2
			
		}
		labels=category_labels
		
		at<-x_vals[odds]
		
		
		if(theLevel==min(risk_levels) & add.legend)
		{
			if(is.null(legend.text))
			{
				legend.text=risk_levels
			}
			if(is.null(legend.args[["legend"]]))
			{
				legend.args[["legend"]]=legend.text
			}
			if(is.null(legend.args[["x"]]))
			{
				legend.args[["x"]]=x_vals[1]
			}
			if(is.null(legend.args[["y"]]))
			{
				legend.args[["y"]]=max(thecounts)
			}
			if(is.null(legend.args[["pch"]]))
			{
				legend.args[["pch"]]=rep(22,length(risk_levels))
			}
			if(is.null(legend.args[["pt.bg"]]))
			{
				legend.args[["pt.bg"]]=barcolors
			}
			
			do.call(legend,legend.args)
			
		}
		
		if(theLevel==max(risk_levels))
		{
			
			
			
			
		}
		else
		{
			labels[-c(1,length(labels))]=""
			
		}
		
		axis(1, col="black", at=at, labels=labels,cex.axis=cex.axis)
		
	}
	
	
	
	
	
	
	
    
}

