library("here")
dat_habitat <- read.csv(here("data", "hab.sta.csv"))

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
par(mfrow = c(2, 3), mar = c(4,4,4,4))
hist(dat_habitat$elev, 
     main = "Elevation",
     col = "blue",
     xlab = "Elevation",
     ylab = "Number of Sample sites")
hist(dat_habitat$aspect, 
     main = "Aspect",
     col = "red",
     xlab = "Aspect",
     ylab = "Number of Sample sites")
hist(dat_habitat$slope, 
     main = "Slope",
     col = "purple",
     xlab = "Slope",
     ylab = "Number of Sample sites")
plot(
  x = dat_habitat$elev, 
  y = dat_habitat$ba.tot,
  col = "blue",
  cex = 0.1,
  xlab = "Elevation",
  ylab = "Total Basal Area")
curve(line_point_slope(x, x1 = 1, y1 = 1, slope = 0.05), add = TRUE)
plot(
  x = dat_habitat$aspect,
  y = dat_habitat$ba.tot,
  col = "red",
  cex = 0.1,
  xlab = "Aspect",
  ylab = "Total Basal Area")
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.05), add = TRUE)
plot(
  x = dat_habitat$slope,
  y = dat_habitat$ba.tot,
  col = "purple",
  cex = 0.1,
  xlab = "Slope",
  ylab = "Total Basal Area")
curve(line_point_slope(x, x1 = 5, y1 = 6, slope = 0.05), add = TRUE)

         