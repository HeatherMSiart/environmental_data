require(palmerpenguins)
require("here")


image_file = "ugly_histogram.png"
save_png_1 = function(image_file)
{
require(here)
png(
  here("images", image_file),
  width = 1200, height = 1000
  )
}

save_png_1("ugly_histogram")

hist(penguins$flipper_length_mm)

dev.off()

dat_vec = penguins$body_mass_g
my_title = "Heather's Histogram"
x_label = "Heather's Data"

hist(
  dat_vec, 
  col = "steelblue", 
  main = my_title,
  xlab = x_label)

steelblue_hist_fun = function(dat_vec, my_title, x_label)
{
  hist(
    dat_vec, 
    col = "steelblue", 
    main = my_title,
    xlab = x_label)
}

steelblue_hist_fun(
  dat_vec = sample(x = 1:100, size = 1000, replace = TRUE),
  my_title = "heather's random numbers",
  x_label = "x-values"
)



# Generate a vector of x-values
svg(filename = here("images","norm_1.svg"))
require(here)
x = seq(-20, 20, length.out = 1000)
y = dnorm(x, mean = 10.4, sd = 2.4)

plot(x, y, main = "Standard Normal, Mean 10.4, SD 2.4", type = "l", xlim = c(0, 20))
abline(h = 0)
