#Aggregate Demo
require(palmerpenguins)
#Taking flipper length, grouping it by species, and calculated the mean for each species
aggregate(penguins$flipper_length_mm, 
          list(penguins$species), 
          FUN = mean, na.rm = TRUE)

#Same as above but different way of writting it
## Using formula notation
aggregate(flipper_length_mm ~ species, 
          data = penguins,
          FUN = mean, na.rm = TRUE)

#You can take that same code and make a boxplot (this formula notation)
boxplot(flipper_length_mm ~ species, 
          data = penguins)

#You can do multiple groupings
aggregate(flipper_length_mm ~ species + sex,
          data = penguins,
          FUN = mean, na.rm = TRUE)

#Critical t-value (At which value along the x axis would I find this)
n = nrow(penguins)

qt(0.975, n - 1)
qt(0.025, n - 1)

#another way to do this is to add an alpha
alpha = 0.05

qt(1 - alpha / 2, n - 1)
qt(alpha/2, n - 1)