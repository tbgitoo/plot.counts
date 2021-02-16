significance_labels <- function(x,levels=c(0.1,0.05,0.01,0.001),codes=c(".","*","**","***"))
{
	s=x;
	s[]='';
	for(level_index in 1:length(levels))
	{
		s[x<=levels[level_index]]=codes[level_index]
	}
	return(s)
	
			
	
}

