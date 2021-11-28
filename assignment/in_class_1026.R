dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWR)

hist(dat_all$WIWR)
hist(dat_all$WIWR, breaks = 0:7 - .5,
     xlab = "Winter Wren",
     main = "Winter Wren Census")