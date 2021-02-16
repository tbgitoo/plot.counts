plot_dot_column<-function(x,y,y_threshold_for_shifting=0.1,lateral_shift=0.06,type="p",...){
    
    # loop through the unique x values, plotting a column for each of the unique values
    
    
    group_order=sort(unique(x))
    
    for(theX in group_order)
    {
        single_dot_column(theX,y[x==theX],
        y_threshold_for_shifting=y_threshold_for_shifting,
        lateral_shift=lateral_shift,type=type,...)
    }
    
    
}

# helper function plotting a single dot column

single_dot_column<-function(x,y,y_threshold_for_shifting=0.1,lateral_shift=0.06,type="p",...){
    
    xplot=rep(x,length(y))
    
    shift=lateral_shift
    
    start=1
    
    ord=order(y)
    
    y=y[ord]
    
    xplot=xplot[ord]
    
    
    if(length(y)>1)
    {
        for(ind in 2:length(y))
        {
            
            # We should finish off a running group because a large step happened, or because we are
            # the end of the column and we should do the shifting
            if(abs(y[ind]-y[ind-1])>=y_threshold_for_shifting | ind==length(y))
            {
                if(ind-start-1>0) # Shifting only if there is more than one element in the group
                {   if(ind<length(y))
                    {
                        xplot[start:(ind-1)]=mean(xplot[start:(ind-1)])+seq(from=-0.5,to=0.5,length.out=ind-start)[sample(ind-start)]*(ind-start)*lateral_shift
                    }
                    if(ind==length(y) & abs(y[ind]-y[ind-1])<y_threshold_for_shifting)
                    {
                        xplot[start:ind]=mean(xplot[start:ind])+seq(from=-0.5,to=0.5,length.out=ind-start+1)[sample(ind-start+1)]*(ind-start)*lateral_shift
                    }
                    if(ind==length(y) & abs(y[ind]-y[ind-1])>=y_threshold_for_shifting)
                    {
                        xplot[start:(ind-1)]=mean(xplot[start:(ind-1)])+seq(from=-0.5,to=0.5,length.out=ind-start)[sample(ind-start)]*(ind-start)*lateral_shift
                    }
                } else
                {
                    # Special case, we are at the end and the last two elements are one group
                    if(ind==length(y) & abs(y[ind]-y[ind-1])<y_threshold_for_shifting)
                    {
                        xplot[start:ind]=mean(xplot[start:ind])+seq(from=-0.5,to=0.5,length.out=ind-start+1)[sample(ind-start+1)]*(ind-start)*lateral_shift
                    }
                }
                start=ind
            }
            
        }
    }
    
    
    lines(xplot,y,type=type,...)
}