get_t_test_matrix <-
function(treatment_factor,data,...)
{
	treatment_factor = as.factor(treatment_factor)
	l=levels(treatment_factor)
	t_test_matrix = matrix(nrow=length(l), ncol=length(l))
	dimnames(t_test_matrix)=list(as.character(l),as.character(l))
	for(a1 in l)
	{
		for(a2 in l)
		{
			if((length(data[treatment_factor==a1])==1) | (length(data[treatment_factor==a2])==1))
			{
				pval=1
			}
			else
			{
				# t.test gives an error if all values are constant and equal 
				if(all(data[treatment_factor==a1] == (data[treatment_factor==a1][1])) & all(all(data[treatment_factor==a2] == (data[treatment_factor==a1][1]))))
				{
					pval=1
				}
				else
				{
					t_test=t.test(data[treatment_factor==a1],data[treatment_factor==a2],...)
					pval=t_test$p.value
				}
			}
			t_test_matrix[as.character(a1),as.character(a2)]=pval
		}
	}
	
	return(t_test_matrix)
	
			
	
}

