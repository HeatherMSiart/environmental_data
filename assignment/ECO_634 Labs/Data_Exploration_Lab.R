require(psych)
pairs.panels(iris)

require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

require(here)
dat_habitat = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_habitat)

dat_all = data.frame(merge(dat_bird, dat_habitat))

plot(ba.tot ~ elev, data = dat_all)

cewa_present_absent = (dat_all$CEWA >1)
as.numeric(cewa_present_absent > 1)
plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, 
     y = cewa_present_absent,
     cex = 0.8,
     col = "dark green",
     pch = 4)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

pairs.panels(dat_habitat[c(6:8,17)])

par(mar = c(5.5, 5.5, 5.5, 5.5))
BCCH_present_absent = (dat_all$BCCH >= 1)
as.numeric(BCCH_present_absent >= 1)
plot(x = dat_all$ba.tot, 
     y = BCCH_present_absent,
     col = "darkorange2",
     cex = 1,
     main = "Presence/Absence of Black-cap Chickadees in Regards to Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence of Black-cap Chickadees")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.5), add = TRUE)

par(mar = c(5.5, 5.5, 5.5, 5.5))
AMRO_present_absent = (dat_all$AMRO >= 1)
as.numeric(AMRO_present_absent >= 1)
plot(x = dat_all$ba.tot, 
     y = AMRO_present_absent,
     col = "goldenrod2",
     cex = 1,
     main = "Presence/Absence of American Robin in Regards to Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence of the American Robin")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.5), add = TRUE)

par(mar = c(5.5, 5.5, 5.5, 5.5))
GRJA_present_absent = (dat_all$GRJA >= 1)
as.numeric(GRJA_present_absent >= 1)
plot(x = dat_all$ba.tot, 
     y = GRJA_present_absent,
     col = "darkolivegreen3",
     cex = 1,
     main = "Presence/Absence of Gray Jays in Regards to Total Basal Area",
     xlab = "Total Basal Area",
     ylab = "Presence of Gray Jays")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -0.5), add = TRUE)

par(mar = c(0.5, 0.5, 0.5, 0.5))
pairs.panels(dat_all)
dim(dat_all)

dat_terrian = head(dat_all[c("elev", "slope", "aspect", "ba.tot")])
pairs.panels(dat_terrian)  
dat_terrian

sum(dat_all$GRJA)

dat_GRJA = as.numeric(dat_all$GRJA > 0)
sum(dat_GRJA)

sum(as.numeric(dat_all$GRJA > 0))
