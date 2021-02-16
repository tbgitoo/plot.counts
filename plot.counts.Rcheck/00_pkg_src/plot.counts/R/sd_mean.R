sd_mean <-
function(x,na.rm=FALSE)
{
	l=length(x);
	if(na.rm)
	{
		l=length(x[!is.na(x)])
	}
	return(sd(x,na.rm=na.rm)/sqrt(l))
}

