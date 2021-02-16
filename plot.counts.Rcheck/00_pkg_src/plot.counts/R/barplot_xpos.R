barplot_xpos<-function (height, xpos=NULL,width = 0.5,col = NULL, border = par("fg"), main=NULL, sub=NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, 
 axes = TRUE, cex.axis = par("cex.axis"), plot.new=TRUE,   density=NULL, angle=45, at_x=NULL, at_y=NULL, labels_x = NULL, labels_y=NULL,
...) 
{
		
    
    
	
    height=c(height)
	width=c(width)
    
    n=length(height)
	
	if(is.null(xpos))
	{
		xpos=1:n
	}
	
	if(is.null(xlim))
	{
		xlim=c(min(xpos)-0.1*(max(xpos)-min(xpos)),max(xpos)+0.1*(max(xpos)-min(xpos)))
	}
	
	if(is.null(ylim))
	{
		ymin=min(c(height,0))
		ymax=max(c(height,0))
		
		ylim=c(ymin-0.1*(ymax-ymin),ymax+0.1*(ymax-ymin))
	}
	
	if(is.null(col))
	{
		col=rep("grey",n)
	}
	
    

        
	par(yaxs = "i")
    
	if(plot.new)
	{
	plot.new()
	plot.window(xlim, ylim, ...)
    }
	rect(xleft=xpos-width/2,xright=xpos+width/2,ybottom=0,ytop=height,col=col,border=border,density=density,angle=angle)
	        
		
		
	title(main = main, sub = sub, xlab = xlab, ylab = ylab, 
			  ...)
	if (axes) {
		
		if(0>=min(ylim) && 0<=max(ylim) )
		{
			axis( 1, cex.axis = cex.axis, pos=0, at=at_x ,labels=labels_x,...)
		}
		else
		{
			axis( 1, cex.axis = cex.axis, at=at_x , labels=labels_x, ...)
		}
		axis( 2, cex.axis = cex.axis,at_y=at_y, labels=labels_y, ...)
	}
        
}
