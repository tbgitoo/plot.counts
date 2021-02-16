plot_counts <-
function(x,y,sd_y=NULL,sig_codes=NULL,showlinreg="NONE", plot.new = TRUE, groups=NULL, group_order = NULL, weights=NULL, category_bounds=NULL,sig.cex=1,sd_x=NULL,...)
{
	
	args=list(...)
    
    
    
	notna=!is.na(x) & !is.na(y)
	
	if(!is.null(groups))
	{
		notna = notna & !is.na(groups)
	}
	
	if(!is.null(sd_y) & length(sd_y) ==length(x))
	{
		sd_y=sd_y[notna]
	}
	
	if(!is.null(sig_codes) & length(sig_codes) ==length(x))
	{
		sig_codes=sig_codes[notna]
	}
	
	if(!is.null(groups) & length(groups) ==length(x))
	{
		groups=groups[notna]
	}
	
	if(!is.null(weights) & length(weights) ==length(x))
	{
		weights=weights[notna]
	}
	
	x=x[notna]
	y=y[notna]
	
	
	
	if(is.null(args$type))
	{
		args$type="b"
	}
	
	if(!is.null(category_bounds) & is.null(groups))
	{
		category_bounds = sort(category_bounds,decreasing=FALSE)
		cat_indexes=vector(mode="numeric",length=length(x))
		mids = vector(mode="numeric",length=length(category_bounds)+1)
		for(index in 1:(length(category_bounds)+1))
		{
			if(index==1)
			{
				cat_indexes[x<category_bounds[index]]=index
				if(length(category_bounds)==1)
				{
					mids[index]=category_bounds[index]-sd(x)
				}
				else
				{
					mids[index]=category_bounds[index]-(category_bounds[index+1]-category_bounds[index])/2
				}
				
			}
			else
			{
				if(length(category_bounds)==1)
				{
					mids[index]=category_bounds+sd(x)
				}
				else
				{
					if(index<=length(category_bounds))
					{
						mids[index]=(category_bounds[index-1]+category_bounds[index])/2
					}
					else
					{
						mids[index]=category_bounds[length(category_bounds)]+(category_bounds[length(category_bounds)]-category_bounds[length(category_bounds)-1])/2
					}
				}
				cat_indexes[x>=category_bounds[index-1]]=index
			}
		}
		y_aggregated = aggregate(y,by=list(cat=cat_indexes),FUN=mean,na.rm=TRUE)
		sd_y_aggregated = aggregate(y,by=list(cat=cat_indexes),FUN=sd_mean,na.rm=TRUE)
		x_aggregated = mids[y_aggregated$cat]
		
		argsplot=args
		
		argsplot$x=x_aggregated
		argsplot$y=y_aggregated$x
		argsplot$sd_y=sd_y_aggregated$x
		argsplot[["showlinreg"]]=showlinreg
		argsplot[["plot.new"]]=plot.new
        argsplot[["sig.cex"]]=sig.cex
		
		do.call(plot_counts,argsplot)
		
		
	}
	
	
	
	
	if(is.null(sd_y)) { sd_y_for_limit=0 } else {sd_y_for_limit = sd_y }
	
	if(is.null(args$ylim))
	{
		
		upper_y = max(c(y,max(y+sd_y_for_limit*1.5,na.rm=TRUE)),na.rm=TRUE);
		lower_y = min(c(y,min(y-sd_y_for_limit*1.5,na.rm=TRUE)),na.rm=TRUE);
		limit_y=c(lower_y,upper_y);
	}
	else
	{
		limit_y=args$ylim	
	}
	
	args$ylim=limit_y
	
	
	if(! is.null(groups))
	{
		if(is.null(group_order))
		{
			group_unique_values = unique(groups)
		}
		else
		{
			group_unique_values = group_order
		}
		
		   
		first=TRUE
		col_index=1
		
		for(thegroup in group_unique_values)
		{
			
			theargs=args
			theargs[["x"]]=x[groups==thegroup]
			theargs[["y"]]=y[groups==thegroup]
			if(!is.null(sd_y))
			{
				theargs[["sd_y"]]=sd_y[groups==thegroup]
			}
            if(!is.null(sd_x))
            {
                theargs[["sd_x"]]=sd_x[groups==thegroup]
            }
			if(! is.null(sig_codes))
			{
				theargs[["sig_codes"]] = sig_codes[groups==thegroup]	
			}
			if( !is.null(weights))
			{
				theargs[["weights"]] = weights[groups==thegroup]
			}
			if( !is.null(theargs[["pch"]]))
			{
					theargs[["pch"]]=theargs[["pch"]][min(length(theargs[["pch"]]),which(group_unique_values==thegroup))]
					
			}
            if( !is.null(theargs[["lty"]]))
            {
                theargs[["lty"]]=theargs[["lty"]][min(length(theargs[["lty"]]),which(group_unique_values==thegroup))]
                
            }
			if( !is.null(theargs[["bg"]]))
			{
				theargs[["bg"]]=theargs[["bg"]][min(length(theargs[["bg"]]),which(group_unique_values==thegroup))]
			}
			theargs[["showlinreg"]]=showlinreg
			theargs[["plot.new"]] = (plot.new & first)
			theargs[["category_bounds"]]=category_bounds
			
			thecol=palette()[min(length(palette()),col_index)]
			if(!is.null(theargs[["col"]]))
			{
				thecol=theargs[["col"]][min(length(theargs[["col"]]),col_index)]
			}
			theargs[["col"]]=thecol
			
			if(first)
			{
				if(is.null(theargs$xlim))
				{
					allowed_x = !is.na(x) & is.finite(x)
					if(!is.null(theargs$log))
					{
						if(length(grep(pattern="x",x=theargs$log)))
						{
							allowed_x = allowed_x & (x>0)
						}
					}
					theargs$xlim=c(min(x[allowed_x]),max(x[allowed_x]))	
				}
				first=FALSE
			}
			
			theargs[["sig.cex"]]=sig.cex
			
			do.call(plot_counts,theargs)
			
			col_index=col_index+1
			
		}
		
		
		return(group_unique_values)
		
		
	}
	else
	{
		
		
		
		
		
		
		
		
		if(plot.new)
		{
			args$x=x
			args$y=y
			argsplot=args
			if(!is.null(argsplot$code))
			{
				argsplot=argsplot[-which( names(argsplot) %in% c("code","length") )] 
			}
			do.call(plot,argsplot)
			
			
		}
		else
		{
			args$xy=xy.coords(x=x,y=y)
			if(is.null(args$type))
			{
				args$type="b"
			}
			argsplot=args
			if(!is.null(argsplot$code))
			{
				argsplot=argsplot[-which( names(argsplot) %in% c("code","length") )] 
			}
			if(!is.null(argsplot$log))
			{
				argsplot=argsplot[-which( names(argsplot) %in% c("log") )] 
			}
            
            
			
			do.call(plot.xy,argsplot)	
		}
		errorbars_plotable = !is.na(y+sd_y) & sd_y>0
        errorbars_x_plotable = !is.na(x+sd_x) & sd_x>0
		
		l=list(...)
		
		if(!is.null(l$col))
		{
			col=l$col
		}
		else
		{
			col="black"
		}
		
		code=3
		if(!is.null(l$code))
		{
			code=l$code
		}
		angle=90
		if(!is.null(l$angle))
		{
			angle=l$angle
		}
		theLength=0.25
		if(!is.null(l$length))
		{
			theLength=l$length
		}
		y0=(y-sd_y)[errorbars_plotable]
		if(code==2)
		{
			y0=(y)[errorbars_plotable]
		}
		
		y1=(y+sd_y)[errorbars_plotable]
		if(code==1)
		{
			y1=(y)[errorbars_plotable]
		}
        
        x0=(x-sd_x)[errorbars_x_plotable]
        if(code==2)
        {
            x0=(x)[errorbars_x_plotable]
        }
        
        x1=(x+sd_x)[errorbars_x_plotable]
        if(code==1)
        {
            x1=(x)[errorbars_x_plotable]
        }
        
        
        
		# If the y-axis is logarithmic, we can't have negative values for the errorbar limits 
		if(!is.null(l$log))
		{
			if(grepl("y",l$log))
			{
				y0[y0<=0]=limit_y[1]
			}
		}
        lwd=1
        if(!is.null(l$lwd))
        {
            lwd=l$lwd
        }
        if(!is.null(sd_y))
        {
            arrows(x0=x[errorbars_plotable],y0=y0,y1=y1,x1=x[errorbars_plotable],code=code, angle=angle,col=col,length=theLength,lwd=lwd)
        }
        if(!is.null(sd_x))
        {
            arrows(x0=x0,y0=y[errorbars_x_plotable],y1=y[errorbars_x_plotable],x1=x1,code=code, angle=angle,col=col,length=theLength,lwd=lwd)
        }
		if(is.null(weights))
		{
				if(is.null(sd_y))
			{
				weights=rep(1,length(x))
			}
			else
			{
				weights=max(sd_y)/sd_y;
			}
		}
		
		linregCol="red"
		if(!is.null(l$col))
		{
			linregCol=l$col
		}
		if(showlinreg=="LINEAR")
		{
			linmod = lm(y~x,weights=weights)
			
			
			
			plot.xy(xy.coords(x=x,y= linmod$coefficients["(Intercept)"]+x*linmod$coefficients["x"]),type="l",col=linregCol,lwd=lwd)
		}
		if(showlinreg=="SQUARE")
		{
			xs=x^2
			
			linmod = lm(y~x+xs,weights=weights)
			
			x_for_plotting=seq(from=min(x), to=max(x), length.out=100)
			
			plot.xy(xy.coords(x=x_for_plotting,y= linmod$coefficients["(Intercept)"]+x_for_plotting*linmod$coefficients["x"]+x_for_plotting^2*linmod$coefficients["xs"]),type="l",col=linregCol,lwd=lwd)
			
		}
		sd_y_for_text=0;
		if(!is.null(sd_y))
		{
			sd_y_for_text = sd_y;
		}
		text(x,y+sd_y+limit_y[2]*0.03,sig_codes,col=col,cex=sig.cex)
		
	}
	
}

