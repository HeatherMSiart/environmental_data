# Importing the data file for the Oregon Bird Data
require(here)
dat_bird_sub = read.csv(
  here("data", "bird.sub.csv")
)
head(dat_bird_sub)

#Importing the data file for the habitat data
require(here)
dat_habitat_sub = read.csv(
  here("data", "hab.sub.csv")
)
head(dat_habitat_sub)

#Importing the data file for the veg data
veg = read.csv(here("data", "vegdata.csv"))

#walk through START
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

# t-test with the alternative hypothesis that Adelie penguins have shorter flippers than Chinstrap penguins
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

#install simpleboot package
install.packages("simpleboot")
require("simpleboot")
#two.boot is used to bootstrap the difference between various univariate statistics.
## An example is the difference of means. Bootstrapping is done by independently resampling from sample1 and sample2.

#usage
two.boot(sample1, sample2, FUN, R, student = FALSE, M, weights = NULL, ...)
#sample1 = first sample; a vector of numbers
#sample2 = second sample; a vector of numbers
#FUN = the statistic which is applied to each sample. this can be a quoted string or a function name
#R = Number of bootstrap replicates
#student = should we do a studentized bootstrao? This requires a double bootstrap so it might take longer
#M = If student is set to TRUE, the M is the number of internal bootstrap replications to do.
#weights = Resampling weights; a list with two components. The first component of the list is a vector of weights for sample1 and the second component of the list is a vector of weights for sample2
#.... = Other (named) arguments that should be passed to FUN.

dat_adelie = subset(penguins, species=="Adelie")
head(dat_adelie)
dat_chin = subset(penguins, species == "Chinstrap")
head(dat_chin)

adelie_chin = two.boot(sample1 = dat_adelie$flipper_length_mm, sample2 = dat_chin$flipper_length_mm, FUN = mean, R = 10000)

hist(adelie_chin)

#Walk through Tree data
boxplot(pine ~ treatment, dat = veg)

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)

#create a table
help("table")
table(dat_tree$treatment)

#wilcoxon Test
aggregate(pine ~ treatment, dat = dat_tree, FUN = mean)
wilcox.test(x, y, alternative = "two.sided")
x = 17.875
y = 1.875

#bootstrap tree data
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)
require(boot)

boot.ci(tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")

quantile(tree_boot$t, 0.025)

#Resampling: linear regression
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

#the z-standardization process
# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

mean(dat_all$b.sidi.standardized)
#7.166938e-17 is effectively 0

sd(dat_all$b.sidi.standardized)
#1

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)

dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

mean(dat_all$s.sidi.standardized)
#2.984718e-17 effectly 0

sd(dat_all$b.sidi.standardized)
#1

#Model variables
par(mar = c(2, 2, 2, 2))
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#simple linear regression
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#the slope coefficent
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

#Resampling the Data
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#Randomization loop
m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
} 

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

quantile(result, c(.05))

#slope
sum[as.numeric()]

#Questions 1 - 4
na.omit(penguin_dat)
pen_boot = 
  two.boot(
    subset(penguin_dat, species == "Chinstrap")$flipper_length_mm,
    subset(penguin_dat, species == "Adelie")$flipper_length_mm,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

boot.ci(pen_boot)
par(mar = c(4, 4, 4, 4))
hist(pen_boot$t,
     main = "Bootstrap Mean Penguin Flipper Lenghth ",
     xlab = "Difference of mean of flipper length (mm)",
     ylab = "Frequency")

mean(pen_boot$t)
sd(pen_boot$t)

#Questions 5 - 7
help(ecdf)
# R Program to compute the value of
# Empirical Cumulative Distribution Function
# Creating a Numeric Vector
#x <- seq(1, 50, by = 2)
# Calling ecdf() Function
#y <- ecdf(x)

pen_ecdf=ecdf(pen_boot$t)
1-pen_ecdf(-4.5)

plot(pen_ecdf)

#Quetion 9
aggregate(pine ~ treatment, dat = dat_tree, FUN = mean)
x = 17.875
y = 1.875
wilcox.test(x, y, alternative = "two.sided")

wilcox.test(pine ~ treatment, data = dat_tree)

#Questions 10 - 11

dat_veg = droplevels(subset(veg, treatment %in% c("control", "clipped")))
veg_sp = (veg, )
tree_boot = 
  two.boot(
    subset(dat_veg, treatment == "clipped"),
    subset(dat_veg, treatment == "control"),
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

