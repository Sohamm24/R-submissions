# Basic cat()
cat("Hello", "World")

# Print multiple values
cat("The zero occurs at", 2*pi, "radians.", "\n")

# Date
d = date()
cat("Today's date is:", d, "\n")

# Vector
x = 1:10
cat(x, sep = " ++ ")

cat("\n")

cat(x, sep = " / ")

# Square
x = 7
cat("The square of", x, "is", x^2, "!\n")

# Square root with formatting
cat(
  "The square root of",
  x,
  "is approximately",
  format(sqrt(x), digits = 3),
  "\n"
)

# Even numbers
evenno = c(2, 4, 6, 8, 10)

cat("The first few even numbers are:", evenno, "...\n")

# fill and labels
x = 1:10
cat(
  x,
  fill = 2,
  labels = paste("(", letters[1:10], "):")
)