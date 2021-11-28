require("here")
catrate = read.csv(here("data", "catrate.csv"))

head(catrate)
summary(catrate)

hist(catrate$cat.rate, 
    main = " Salamander Reproduction Catastrophic Rate",
    xlab = "Catastrophic Rate")

shapiro.test(catrate$cat.rate)

install.packages("nortest")

?t.test
pond_cat = merge(catrate$cat.rate, catrate$pond)
t.test(catrate$cat.rate, mu = 0.2857143)
t.test(catrate$cat.rate, catrate$pond)
t.test(catrate$cat.rate,catrate$pond, alternative ="l")
t.test(pond_cat, alternative = "two.sided")
1 - 0.994

wilcox.test(catrate$cat.rate, catrate$pond)
wilcox.test(catrate$cat.rate, mu = 2 / 7, alternative  = "g")
wilcox.test(catrate$cat.rate, mu = 2 / 7, alternative  = "l")

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")

# Extract the Adelie penguin data
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
?shapiro.test
mean(dat_adelie$flipper_length_mm)
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)

par(mar = c(4.5,4.5,4.5,4.5))
par(mfrow=c(1,2))
hist(dat_adelie$flipper_length_mm,
     main = "Adelie Flipper Length",
     xlab = "Flipper Length mm",
     ylab = "frequency")
hist(dat_chinstrap$flipper_length_mm,
     main = "Chrinstrap Flipper Length",
     xlab = "Flipper Length mm",
     ylab = "Frequency")

t.test(dat_adelie$flipper_length_mm, dat_chinstrap$flipper_length_mm, alterantive = "two.sided")
