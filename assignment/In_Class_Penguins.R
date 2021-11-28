install.packages("palmerpenguins")
install.packages("here")
require(palmerpenguins)
require(here)

class(penguins)

penguins = data.frame(penguins)

mean(penguins$body_mass_g)

head(penguins)

?mean

summary(penguins)

pairs(penguins)

plot(penguins$bill_length_mm, penguins$bill_depth_mm)

hist(penguins$bill_length_mm)

boxplot(penguins$bill_depth_mm)

boxplot(bill_depth_mm ~ sex, data = penguins)

par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
