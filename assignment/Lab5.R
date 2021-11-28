library("here")
dat_dispersal <- read.csv(here("data", "dispersal.csv"))

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

plot(dat_dispersal$dist.class, dat_dispersal$disp.rate.ftb,
     xlab = "Class",
     ylab = "First Time Breeder",
     main = "Marbled Salamander, first time breeders")

curve(
  ricker_fun(x, 0.0087, (1/225)), add = TRUE)

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

summary(dat_dispersal$disp.rate.ftb)
lm(dat_dispersal)
lm(dat_dispersal$disp.rate.ftb)

locator(15)

dat_dispersal$resids_ricker <- c(0.386, 0.386, 0.386, 0.386, 0.399,
                            0.399, 0.399, 0.399, 0.412, 0.425,
                            0.425, 0.412, 0.412, 0.412, 0.425)

dat_dispersal$resids_linear <- c( 0.672,  0.646, 0.606,  0.550, 0.489,  0.454,  0.393,  0.332,
                             0.241, 0.196, 0.150,  0.0850,
                             0.0496,  0.00408, 0.00331)


dat_dispersal$resids_exp <- c(0.728, 0.531, 0.399, 0.307,
                         0.241, 0.175, 0.135, 0.096,
                         0.0961, 0.069, 0.056, 0.0301,
                         0.0301, 0.0301, 0.043)

View(dat_dispersal)
resid_linear <- c(dat_dispersal$disp.rate.ftb - dat_dispersal$resids_linear)
resid_exp <- c(dat_dispersal$disp.rate.ftb - dat_dispersal$resids_exp)
resid_ricker <- c(dat_dispersal$disp.rate.ftb - dat_dispersal$resids_ricker)


disp_resids <- data.frame(dat_dispersal$resids_linear, dat_dispersal$resids_exp, dat_dispersal$resids_ricker)

require(here)
png(
  filename = here("images", "lab_05_hist.png"))


par(mfrow = c(3, 1))
hist(resid_linear, main = "Histogram of Linear Resids", xlab = "", col = "darkorange1", xlim = c(-0.5, 0.5))
hist(resid_exp, main = "Histogram of Exponential Resids", xlab = "", col = "coral2", xlim = c(-0.5, 0.5))
hist(resid_ricker, main = "Histogram of Ricker Resids", xlab = "", col = "darkgoldenrod2", xlim = c(-0.7, 0.3))


dev.off()