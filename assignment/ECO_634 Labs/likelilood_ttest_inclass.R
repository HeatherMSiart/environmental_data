##Likelihood
#Question 1
#poisson
dat_all$WIWR
sum(log(dpois(x = c(2, 6), lambda = 4.5)))
sum(log(dpois(x = c(2, 6), lambda = 2)))
sum(log(dpois(x = c(2, 6), lambda = 4)))

sum(log(dpois(x = c(2, 6), lambda = 4 + 0.1)))
sum(log(dpois(x = c(2, 6), lambda = 4 - 0.1)))

#Question 2

sum(log(dpois(dat_all$WIWR, lambda = 1.2)))
sum(log(dpois(dat_all$WIWR, lambda = 1.3)))
sum(log(dpois(dat_all$WIWR, lambda = 1.4)))
sum(log(dpois(dat_all$WIWR, lambda = 1.456023)))

mean(dat_all$WIWR)
sd(dat_all$WIWR)

#Question 3
#Binomial
summary(dat_all$WIWR)

n = 20

20 * x = 1.45
prob_guess = 1.45 / n

sum(log(dbinom(
  x = dat_all$WIWR,
  size = n, 
  prob = prob_guess + 0.0001)))


#Assignment from Tuesday
#t-tests
require(palmerpenguins)

dat_pen = droplevels(subset(penguins, species == "Adelie"))

boxplot(body_mass_g ~ sex, data = dat_pen)

#one-sample t-test
dat_pen_f = droplevels(subset(dat_pen, sex = "females"))
dat_pen_m = droplevels(subset(dat_pen, sex = "males"))

#These are a one tail test

t.test(
  x = dat_pen_f$body_mass_g,
  mu = 3400)


t.test(
  x = dat_pen_m$body_mass_g,
  mu = 4000,
  alternative = "g")

#These are two tailed tests
# x/y syntex
t.test(
  x = dat_pen_f$body_mass_g,
  y = dat_pen_m$body_mass_g,
  alternative = "t")

#formula/data syntex
t.test(
  formula = body_mass_g ~ sex,
  data = dat_pen,
  alternative = "g"
)
