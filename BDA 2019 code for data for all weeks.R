
# Create data for the BDA course at the OII

### Week 0 ###
# NONE

### Week 1 ###
# NONE

### Week 2 ###
# a - normal
set.seed(11)
mydata = data.frame(a = rnorm(1000, 100, 10))

# b - power law
set.seed(12)
dpareto <- function(x, a=0.5, b=1) a*b^a/x^(a+1)
ppareto <- function(x, a=0.5, b=1) (x > b)*(1-(b/x)^a)
qpareto <- function(u, a=0.5, b=1) b/(1-u)^(1/a)
rpareto <- function(n, a=0.5, b=1) qpareto(runif(n),a,b)
b = rnorm(1020)
b = rpareto(b) 
mydata$b = b[order(b, decreasing=F)[1:1000]]

# c - log normal
set.seed(12)
c = rnorm(1000,100,10)
c = exp(c) + rnorm(1000, c, (c/10))
mydata$c = c

# d - exponential
set.seed(14)
d = rexp(10000,0.2)
d = d[order(d)]
d = split(d, ceiling(seq_along(d)/10))
d = lapply(d, mean)
d = unlist(d)
mydata$d = d

# df.power
x = seq(1,100)
df.power = data.frame(
  x = x,
  #y0 = x^0,
  y1 = x^-1,
  y1.2 = x^-1.2,
  y1.3 = x^-1.3,
  y1.4 = x^-1.4,
  y2 = x^-2
)
df.power = df.power %>%
  gather(power,
         value,
         y1:y2)

# df.compare
x = seq(1,10)
power_function = x^2 # power function
exponential_function = 2^x #exponential function
df.compare = data.frame(x,power_function,
                        exponential_function)
df.compare = df.compare %>%
  gather(function_name,
         value,power_function:exponential_function)

# Save week 2
save(mydata,
     df.power,
     df.compare,
     file = "~/Dropbox/BDA-2019/bda_week2.RData")



### Week 3 ###
# mydata2
set.seed(10)
x = rnorm(20000,10,2) #10,000 values between 10 and 100
y = x^1.5 + rnorm(20000,10,10)
mydata2 = data.frame(x,y); rm(x,y)

# mydata3 - sine curve data to demonstrate loess
set.seed(10)
x = 1:120
y = sin(2*pi*x/120) + runif(length(x),-1,1)
mydata3 = data.frame(x,y)
mydata3$y2 = rnorm(120,
                   mydata3$x,
                   5)

# mydata4
set.seed(10)
x = seq(0.1,100,0.1)
y = (x)^(-1/2) * exp(rnorm(length(x),
                           sd = 0.25))
mydata4 = data.frame(x,y)
# Save week 3
save(mydata2,
     mydata3,
     mydata4,
     file = "~/Dropbox/BDA-2019/bda_week3.RData")


### Week 4 ###
# mydata5
set.seed(1)
b = rnorm(1000,mean=10,sd=10)
a = b + rnorm(1000,mean=1,sd=5)
df1 = data.frame(a,b)
a = c(a[101:1000], a[1:100]) #rearrange y
time = seq(as.Date('2011-01-01'),by = 1, length.out = length(a))
mydata5 = data.frame(a, b, time)

# mydata6
time = seq(as.Date('2000-01-01'), by = 'month', length.out = 1000 )
x = seq(0.1,100,0.1)
y = sin(x)
mydata6 = data.frame(time,y)

# mydata7
set.seed(1)
x = c(1,4,3,2,5,4,4)
x = rep(x, 40)
y = x + rnorm(x, 0, 1.5)
time = seq(as.Date('2017-01-01'), by = 'day', length.out = length(y) )
mydata7 = data.frame(time, y)

# Save week 4
save(mydata5,
     mydata6,
     mydata7,
     file = "~/Dropbox/BDA-2019/bda_week4.RData")
