# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

# Min value in each row "MARGIN = 1" tells apply to look at the row
apply(dat, MARGIN = 1, FUN = min)

# Max Value in each row
apply(dat, MARGIN = 1, FUN = max)

# Looking for the mean of the values, "MARGIN = 2" tells apply to lok at the columes
apply(dat, MARGIN = 2, FUN = mean)

library("here")
moths = read.csv(here("data", "moths.csv"))
head(moths)

#calculate the Standard Sample Error, sse
## You'll need the sample standard deviation (ssd) and the sample size

# Sample size
n <- 30

# Degrees of Freedom
df <- (n-1)

#Sample Standard Deviation
ssd <- 
  
#Sample Standard Error
sse_mean = function(x)
{
  sse = sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
    
  return(sse)  
}

sse_mean(moths)

sse = ssd / sqrt(n); sse

require(palmerpenguins)

sse_mean = function(x, na.rm = TRUE) sd(x)/sqrt(lentgh(x))
dat_gentoo = subset(penguins, species=="Gentoo")
head(dat_gentoo)

# Calculate the sample size N
sum(!is.na(dat_gentoo$bill_length_mm))

#Sample size n= 123

#Calc SSD
ssd = sd(dat_gentoo$bill_length_mm, na.rm = TRUE)
ssd
#Standard Sample Error
sse_mean(dat_gentoo$bill_length_mm)
sse_mean
# Q3
crit_upper = qt(1 - (0.05/2), 122)
crit_upper
crit_lower = qt(0.05/2, 122)
crit_lower

t_crit1 = qt(c(0.025, 0.975), df = (n-1))

#q4
sse = ssd / sqrt(n)

# Calculate critical t-values using alpha value (1 - 0.95)
alpha = 0.05

t_crit = abs(qt(alpha / 2, n - 1))
t_crit

#q5
# Calculate the CI
mean(penguins$bill_length_mm, na.rm = TRUE)

# This is the radius of the CI
ci_parametric = t_crit * sse
ci_parametric

#Express CI as mean +/- radius
#upper
43.92193 + 0.02701383
#lower
43.92193 - 0.02701383

#Q6
##Create the results vector
m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

#Perform the bootstrap
for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}

#Calculate the Quentiles
mean(result)

# the 2.5% and 97.5% quantiles of the bootstrap distribution
quantile(result,c(0.025,0.975))

#Installing boot package
install.packages("boot")

#Basic Syntax for boot
require(boot)
#R is the number of resamplings you want to preform
##data  is the data object you want to resample. It can be a vector, matrix, or data.frame.
###is a function that returns the statistic of interest. You don’t put quotes around the name of the function.
boot(data, statistic, R)

#Extending R for the mean in boot
##The key point is that we have to use our boot_mean() function, rather than mean() within our call to boot()
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

str(myboot)
mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

#Q6
#Penguin bootstrapping
pen_boot = 
  boot(
    data = dat_gentoo$bill_length_mm,
    statistic = boot_mean,
    R = 10000
  )
print(pen_boot)

quantile(
  pen_boot$t,
  c(0.025, 0.975))

#extract our bootstrap confidence interval as follows:
quantile(
  myboot$t,
  c(0.025, 0.975))

moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

#First Draft
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#second Draft
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 10000)
head(rarefact)

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 10000)
head(rarefact)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

rm(list = ls())

moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]
n = nrow(moth_dat)
rarefaction_sampler = function(input_dat, n_iterations)
  {
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    
    for(j in 1:n)
    {
    
      rows_j = sample(n, size = j, replace=TRUE)
      
      t1 = input_dat[rows_j, ]
      
      t2 = apply(t1, 2, sum)
      
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}
rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Convidence Intervals with 10k samples',
  )

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  cex = (0.65),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))
