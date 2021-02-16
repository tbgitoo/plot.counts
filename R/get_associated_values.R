get_associated_values <- function(descriptive_data,lookup_data,lookup_name_correspondence=NULL,FUN=NULL,lookup_value_col=NULL,...)
{
	
	column_names<-function(m)
	{
		if(!is.null(names(m)))
		{
			return(names(m))
		}
		if(!is.null(dimnames(m)))
		{
			d=dimnames(m)
			if(!is.null(d[[2]]))
			{
				return(d[[2]])
			}
		}
		return(NULL)
	}
	
	default_name_correspondence<-function(desc,lookup)
	{
		lc=matrix(nrow=min(dim(descriptive_data)[2],dim(lookup_data)[2]),ncol=2)
		
		if(!is.null(column_names(descriptive_data)))
		{
			lc[,1]=column_names(descriptive_data)[1:(dim(lc)[1])] 
		}
		else
		{
			lc[,1]=1:(dim(lc)[1])	
		}
		
		if(!is.null(column_names(lookup_data)))
		{
			lc[,2]=column_names(lookup_data)[1:(dim(lc)[1])] 
		}
		else
		{
			lc[,2]=1:(dim(lc)[1])	
		}
		return(lc)
		
	}
	
# The idea is that lookup_name_correspondence should be a matrix of two columns, indicating the columns that should be equal
	if(is.null(lookup_name_correspondence))
	{
		lookup_name_correspondence = default_name_correspondence(descriptive_data,lookup_data)
	}
	if(is.null(lookup_value_col))
	{
		if(!is.null(column_names(lookup_data)))
		{
			lookup_value_col = column_names(lookup_data)[length(column_names(lookup_data))]
		}
		else
		{
			lookup_value_col = dim(lookup_data)[2]
		}
	}
	if(is.vector(lookup_name_correspondence))
	{
			lookup_name_correspondence = matrix(nrow=length(lookup_name_correspondence),ncol=2,byrow=FALSE,data=lookup_name_correspondence)
	}
	
	if(is.null(FUN))
	{
		FUN=mean
	}
	
	retValue = vector(mode="numeric",length=dim(descriptive_data)[1])
	
	for(ind in 1:(dim(descriptive_data)[1]))
	{
		logical_vector = vector(mode="logical",length=dim(lookup_data)[1])
		logical_vector[]=TRUE
		
		
		
		for(lookup_index in 1:(dim(lookup_name_correspondence)[1]))
		{
			theValue = descriptive_data[ind,lookup_name_correspondence[lookup_index,1]]
			logical_vector = logical_vector & (lookup_data[,lookup_name_correspondence[lookup_index,2]]==theValue)
		}
		
		retValue[ind]=FUN(lookup_data[logical_vector,lookup_value_col],...)
	}
	
	return(retValue)
}

