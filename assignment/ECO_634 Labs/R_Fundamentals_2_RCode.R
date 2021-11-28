n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_1

vec_2 <- (vec_1 == 3)

vec_1[vec_2]
 
length(vec_1)
sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
vec_1

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
vec_1
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
vec_1

for (i in 1:10)
{
  print(paste0("This is loop iteration: ", i))
}

n <- 55
for (i in 1:n)
{
  print(paste0("This is loop iteration: ", i))
}

n = 17
min = 69
max = 420
vec_1 = sample(69:420, n, replace = TRUE)
paste0("The element of vec_1 at index 1: ", sum(vec_1 == 3))
for (i in 1:n)
{
  print(paste0("The element of vec_1 at index: ", i, " is ", vec_1[i]))
}


create_and_print_vec = function(n, min = 1, max = 10)
{
  vec_1 = sample(min : max, n, replace = TRUE)
  paste0("The element of vec_1 at index 1: ", sum(vec_1 == 3))
  for (i in 1:n)
  {
    print(paste0("The element of vec_1 at index: ", i, " is ", vec_1[i]))
  }
}
create_and_print_vec(10, min = 1, max = 10)