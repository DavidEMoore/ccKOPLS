compute.auc.ci <- function(auc_results) {
iteration_means = colMeans(auc_results)[2:ncol(auc_results)]
stdev = sd(iteration_means)
mean_value = mean(iteration_means)
ci_upper = mean_value + 1.96*stdev/sqrt(length(iteration_means))
ci_lower = mean_value - 1.96*stdev/sqrt(length(iteration_means))
return(c(ci_lower,ci_upper,mean_value))
}
