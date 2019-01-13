#Nama : Naufal Alfiansyah Kurniawan
#NIM  : 17523194

data1 <- read.csv(file.choose(),header = TRUE)
data1

#no 1
model <- lm(y ~ x, data=data1)
summary(model)

#no 2
predict(model, data.frame(x=55))


data2 <- read.csv(file.choose(), header = TRUE)
data2

#n0 3
poly.calc(data2)

#no 4
fc <- function(x){
  return(7.5*x^2 + 6.5*x^3 + 20.6*x^4 - 5*x^5 + x^6)
}
fc(2.75)

#no 11
f1 <- function(x){
  return(x^2 - 6)
}
trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- b - a
  
  fxdx <- (h / 2) * (f(a) + f(b))
  
  return(fxdx)
}
trapezoid(f1, 0, 1)

#no 12
f2 <- function(x){
  return(x^3 + 4*x^2 - 10)
}
trapezoid(f2, 1, 2)
