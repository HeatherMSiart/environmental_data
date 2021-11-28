require(palmerpenguins)

# The sample standard error of the mean is the sample standard deviation divided
## by the square root of the sample size SSEm = Sx/ square root of n
rm(list = ls())
sse_mean = function(x)
{
 sse = sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
  
  return(sse)  
}

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)
sse_mean(penguins$bill_depth_mm)

boxplot(flipper_length_mm ~ species, data = penguins)

# Removes the third species
dat_pen1 = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen1)

# We can use droplevels() to remove unused factor levels from a data.frame:
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

# for reproducibility
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")


#Question 2
two_group_resample = function(x, n_1, n_2)
{
  diff_1 = sample(x, n_1, replace = TRUE)
  diff_2 = sample(x, n_2, replace = TRUE)
  difference_in_means = mean(diff_1, na.rm = TRUE) - mean(diff_2, n.rm = TRUE)
  
  return(difference_in_means)
}

two_group_resample(penguins$bill_depth_mm, penguins$bill_length_mm)

#Q4
n = 50000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences,
     main = "n = 50000",
     xlab = "Mean Difference",
     col = "thistle3")

t_test = t.test(flipper_shuffled ~ dat_pen$species)
t_test
192.1974 - 190.8209
sum(abs(mean_differences) >= diff_observed)


#walk through
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

#Classical test on resampled data: What if we reshuffled the data and re-ran the t-test?
## The t-test output does not support rejecting a null hypothesis that the two flipper lengths are the same between the two species
t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

#Difference of means
## the t-test great for comparing the means of two groups. We can see the group means in the t-test output:
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate

#The difference in means is:
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

# for reproducibility
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(bill_depth_mm ~ species, dat_pen, main = "Bill Depth", ylab = "Bill depth in mm")
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")

t_test = t.test(dat_pen$bill_depth_mm ~ dat_pen$species)
t_test

t_test$estimate

n = 10000
mean_differences1 = c()
for (i in 1:n)
{
  mean_differences1 = c(
    mean_differences1,
    two_group_resample(dat_pen$bill_depth_mm, 68, 152)
  )
}
sum(abs(mean_differences) >= diff_observed)

18.42059 - 18.34636

