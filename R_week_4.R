# Importing the data file for the Oregon Bird Data
require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

# Importing the data file for the habitat data
require(here)
dat_habitat = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_habitat)

hist(dat_bird$CBCH,
     xlab = "Number of birds counted",
     breaks = 0:7 - 0.5)

sum(dat_bird$sta)
