errorbars<-function (x,y,sd_y=0,angle=90,code=3,horiz=FALSE,...) 
{
	
	ystart=y
	ystop=y
	if(code==2 || code==3)
	{
		ystop=y+sd_y
	}
	if(code==1 || code==3)
	{
		ystart=y-sd_y
	}
	
	if(horiz)
	{
		arrows(x0=ystart,y0=c(x),x1=ystop,y1=c(x),angle=angle,code=code,...)
	}
	else
	{
		arrows(x0=x,y0=ystart,x1=x,y1=ystop,angle=angle,code=code,...)
		
	}
	
	
	
	
	
	
	
    
}

