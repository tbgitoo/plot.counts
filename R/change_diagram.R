change_diagram<-function(y_initial,y_final,x_initial=NULL,x_final=NULL,groups=NULL,plot.new=TRUE,...)
{
    # Neither initial nor final x-coordinates given, use default (from 0 to 1)
    if(is.null(x_initial) & is.null(x_final))
    {
        x_initial=rep(0,max(length(y_initial),length(y_final)))
        x_final=rep(1,max(length(y_initial),length(y_final)))
    } else {
        if(is.null(x_initial)) # x_final is given but not x_initial
        {
            # x_final has only one distinct value
            if(length(unique(x_final))==1)
            {
                x_initial=rep(x_final[1]-1,max(length(y_initial,y_final)))
            } else
            {
                x_initial=rep(2*min(x_final)-max(x_final),max(length(y_initial),length(y_final)))
            }
        } else
        {
            if(is.null(x_final))
            {
                # x_initial is given but not x_final
                if(length(unique(x_initial))==1) # only one distinct value
                {
                    x_final=rep(x_initial[1]+1,max(length(y_initial),length(y_final)))
                } else
                {
                    x_final=rep(2*max(x_initial)-max(x_initial),max(length(y_initial),length(y_final)))
                }
            }
        }
    }
    
    started=FALSE
    if(!plot.new){started=TRUE}
    # At this point, we have initial and final values, either user provided or by default
    
    
    groups_given = TRUE
    if(is.null(groups))
    {
        groups=rep(1,max(length(y_initial),length(y_final)))
        
        groups_given=FALSE
        
    }
    # Now we also have groups
    
    
    # Handling of graphical arguments
    dot_args=list(...)
    
    #Default values for background, point symbol and color
    bg="black"
    pch=1
    col=palette()
    type="b"
    lty=1
    
    # If provided by the user, use it instead
    if(!is.null(dot_args$pch))
    {
        pch=dot_args$pch
    }
    
    # If there are several values in the col, bg and pch arguments, go through them in order
    
    if(!is.null(dot_args$bg))
    {
        bg=dot_args$bg
    }
    
    if(!is.null(dot_args$col))
    {
        col=dot_args$col
    }
    
    if(!is.null(dot_args$type))
    {
        type=dot_args$type
    }
    
    if(!is.null(dot_args$lty))
    {
        lty=dot_args$lty
    }

    
    
    for(theGroupIndex in 1:length(unique(groups)))
    {
        
        theGroup = unique(groups)[theGroupIndex]
        
        y_initial_group = y_initial[groups==theGroup]
        y_final_group=y_final[groups==theGroup]
        x_initial_group=x_initial[groups==theGroup]
        x_final_group=x_final[groups==theGroup]
        
        
        
        # If a group vector was provided, we should get the pch symbol from the group
        if(groups_given)
        {
            
            # Get values to use for the graphical parameters per group
            pch_to_use = pch[min(c(length(pch),theGroupIndex))]
            bg_to_use = bg[min(c(length(bg),theGroupIndex))]
            col_to_use = col[min(c(length(col),theGroupIndex))]
            type_to_use = type[min(c(length(type),theGroupIndex))]
            lty_to_use = lty[min(c(length(lty),theGroupIndex))]
            
            
        }
        
        
        for(index_line in 1:length(y_initial_group))
        {
            
            
            if(!groups_given)
            {
                
                
                # No groups are given get the graphical parameters per line to be drawn
                pch_to_use = pch[min(c(length(pch),index_line))]
                bg_to_use = bg[min(c(length(bg),index_line))]
                col_to_use = col[min(c(length(col),index_line))]
                type_to_use = type[min(c(length(type),index_line))]
                lty_to_use = lty[min(c(length(lty),index_line))]
                
                
            }
            
            
            
            
            # Prepare the arguments to the drawing calls
            # Copy the variable arguments already provided as input
            args_call = dot_args
            # Add the pch argument if appropriate
            if(!is.null(dot_args$pch))
            {
                args_call$pch=pch_to_use
            }
            if(!is.null(dot_args$bg))
            {
                args_call$bg=bg_to_use
            }
            # Color is used anyways
            args_call$col=col_to_use
            args_call$type=type_to_use
            args_call$lty=lty_to_use
            
            
            
            args_call$x=c(x_initial_group[index_line],x_final_group[index_line])
            args_call$y=c(y_initial_group[index_line],y_final_group[index_line])
            
            
            if(!started)
            {
                started=TRUE
                
                do.call(plot,args_call)
                
                
            } else
            {
                do.call(lines,args_call)
            }
            
            
        }
        
        
        
    }
    
    return(unique(groups))
    
}
