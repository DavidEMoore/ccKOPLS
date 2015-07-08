compute.auc.ci <- function(auc_results) {
iteration_means = colMeans(auc_results)[2:ncol(auc_results)]
stdev = sd(iteration_means)
mean_value = mean(iteration_means)
ci_upper = mean_value + 1.96*stdev/sqrt(length(iteration_means))
ci_lower = mean_value - 1.96*stdev/sqrt(length(iteration_means))
return(c(ci_lower,ci_upper,mean_value))
}

# cckopls = read.csv('auc_files/cckopls_auc1.csv')
# print(compute.auc.ci(cckopls))
# 
# kopls = read.csv('auc_files/kopls_auc.csv')
# print(compute.auc.ci(kopls))
# 
# ccnox0 = read.csv('auc_files/ccnox0_auc.csv')
# print(compute.auc.ci(ccnox0))
# 
# nox0 = read.csv('auc_files/nox0_auc.csv')
# print(compute.auc.ci(nox0))
