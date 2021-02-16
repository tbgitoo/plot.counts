pkgname <- "plot.counts"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('plot.counts')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("get_associated_values")
### * get_associated_values

flush(stderr()); flush(stdout())

### Name: get_associated_values
### Title: get_associated_values
### Aliases: get_associated_values
### Keywords: misc

### ** Examples

descriptive = data.frame(matching_col1=c(1,1,2,2,3,3), matching_col2=c(16,17,16,17,16,17),irrelevant_col1=c(1,1,1,1,1,1),irrelevant_col2=c(16,18,20,15,2,-1))
lookup=data.frame(match_col_1=c(1,1,1,2,2,2,3,3,3),match_col_2=c(16,17,17,16,17,17,16,17,17),value_col=c(1,2,2.5,4,3.5,3,6,5.5,NA),some_col=c(5,5,5,4,3,2,1,2,3))
lookup_name_correspondence=matrix(data=c("matching_col1","match_col_1","matching_col2","match_col_2"),ncol=2,byrow=TRUE)
lookup_value_col="value_col"
assoc_vals=get_associated_values(descriptive_data=descriptive,lookup_data=lookup,lookup_name_correspondence=lookup_name_correspondence,FUN=sum,lookup_value_col=lookup_value_col,na.rm=TRUE)
descriptive_and_associated=descriptive
descriptive_and_associated$associated = assoc_vals
cat("The base data: ")
descriptive
cat("The lookup table:")
lookup
cat("The associated values found (here, the sum of corresponding lines):")
assoc_vals
cat("The associated values in comparison with the base data:")
descriptive_and_associated



cleanEx()
nameEx("get_t_test_matrix")
### * get_t_test_matrix

flush(stderr()); flush(stdout())

### Name: get_t_test_matrix
### Title: Get a T-Test matrix comparing the results under different
###   treatments
### Aliases: get_t_test_matrix
### Keywords: misc

### ** Examples

test_data=data.frame(condition=c(rep("A",5),rep("B",5),rep("C",8)),outcome=c(1,3,2,2.5,3.2,8,8.25,9,8.5,7.5,0.1,0.5,-0.5,0.2,-0.25,0,0,1))
get_t_test_matrix(test_data$condition,test_data$outcome)



cleanEx()
nameEx("pie_with_errorbars")
### * pie_with_errorbars

flush(stderr()); flush(stdout())

### Name: pie_with_errorbars
### Title: pie_with_errorbars
### Aliases: pie_with_errorbars
### Keywords: misc

### ** Examples

pie_with_errorbars(x=c(1,2,3),sd_x=c(0.1,0.2,0))




cleanEx()
nameEx("plot_counts")
### * plot_counts

flush(stderr()); flush(stdout())

### Name: plot_counts
### Title: plot_counts
### Aliases: plot_counts
### Keywords: misc

### ** Examples

plot_counts(x=c(1,2,3),y=c(2,2,3),sd_y=c(1,1,0.5))




cleanEx()
nameEx("plot_dot_column")
### * plot_dot_column

flush(stderr()); flush(stdout())

### Name: plot_dot_column
### Title: plot_dot_column
### Aliases: plot_dot_column
### Keywords: misc

### ** Examples

plot_dot_column(x=c(1,2,3),y=c(2,2,3),sd_y=c(1,1,0.5))




cleanEx()
nameEx("sd_mean")
### * sd_mean

flush(stderr()); flush(stdout())

### Name: sd_mean
### Title: sd_mean
### Aliases: sd_mean
### Keywords: misc

### ** Examples

x<-c(1,2,3,4,3.5,2.5,1)
sd(x)
sd_mean(x)




cleanEx()
nameEx("significance_labels")
### * significance_labels

flush(stderr()); flush(stdout())

### Name: significance_labels
### Title: significance_labels
### Aliases: significance_labels
### Keywords: misc

### ** Examples


significance_labels(c(0.01,0.04,0.05,0.5))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
