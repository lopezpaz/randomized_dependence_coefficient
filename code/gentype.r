titles  <- c("Linear", "Quadratic", "Cubic", "Sine", "Cosine",
             "Power 0.25", "Circle", "Step", "Bell", "Two Waves",
             "Logarithm")

n_types <- 11

gentype <- function(typ, n, l, noise, num.noise) {
  x = runif(n)

  if(typ==1)  y = x+ noise *(l/num.noise)* rnorm(n)
  if(typ==2)  y = 4*(x-.5)^2+  noise * (l/num.noise) * rnorm(n)
  if(typ==3)  y = 128*(x-1/3)^3-48*(x-1/3)^3-12*(x-1/3)+10* noise  * (l/num.noise) *rnorm(n)
  if(typ==4)  y = sin(3*pi*x) + 3*noise * (l/num.noise) *rnorm(n)
  if(typ==5)  y = cos(3*pi*x) + 3*noise * (l/num.noise) *rnorm(n)
  if(typ==6)  y = x^(1/4) + noise * (l/num.noise) *rnorm(n)
  if(typ==7)  y = (2*rbinom(n,1,0.5)-1) * (sqrt(1 - (2*x - 1)^2)) + noise/4*l/num.noise *rnorm(n)
  if(typ==8)  y = (x > 0.5) + noise*5*l/num.noise *rnorm(n)
  if(typ==9)  y = dnorm(x, .5, .1) + 4*noise*l/num.noise*rnorm(n) 
  if(typ==10) y = (2*rbinom(n,1,0.5)-1) * sin(3*pi*x) + (l/num.noise)*rnorm(n)
  if(typ==11) y = log(x) + 4*noise*l/num.noise*rnorm(n) 
  
  list(x=x,y=y)
}

