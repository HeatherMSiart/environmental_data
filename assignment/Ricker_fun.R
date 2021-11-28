ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 75, add = FALSE, 
  ylab = "f(x)", xlab = "x", col = "black")
curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 75, add = TRUE, 
  ylab = "f(x)", xlab = "x", col = "black", lty = 2)
curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 75, add = TRUE, 
  ylab = "f(x)", xlab = "x", col = "black", lty = 2)
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 75, add = TRUE, 
  ylab = "f(x)", xlab = "x", col = "red")
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 75, add = TRUE, 
  ylab = "f(x)", xlab = "x",col = "red", lty = 2)
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 75, add = TRUE, 
  ylab = "f(x)", xlab = "x", col = "red", lty = 2)

exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 1, 1),
  from = 0, to = 5, add = FALSE,
  main = "Exponential Function: a=1, b=1",
  ylab = "f(x)", xlab = "x")

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

curve(
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "black"); box()
curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "black",lty = 2); box()
curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "red"); box()
curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)", col = "red", lty = 2); box()