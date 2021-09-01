# Descriptive statistics
#reading dataset
library(datasets)
str(rivers)
x = rivers


#####ungrouped data
# mean
mean(x)

# weighted mean
#giving weight by the rank of river

weighted.mean(x,w = rank(x)) # weights equal to their value

# geometric mean
exp(mean(log(x)))

# harmonic mean
1/mean(1/x)

# trimmed mean
mean(x, trim = .05)

# median
median(x)

## adding condition: of only those where x<500
mean(x[x<500])
median(x[x<500])

# mode
a = table(x)
which.max(table(x)) ## gives the mode element and its position. 
                    ## in case of multimodal dist, gives the smallest one

which(a==max(a))    ## gives all the modal elements and their position

as.numeric(names(which(a==max(a)))) # a vector of modal values

a[which(a==max(a))]
# quantiles
quantile(x, probs = c(0,.25,.50,.75,1)) # fives min, 1st, 2nd, 3rd quartiles, max

# percentiles and stuff
quantile(x,probs = .35) # 35 percentile

# five num summary
fivenum(x)
# five num summary+mean
summary(x)


# discrete


# convert to ungrouped, and continue

y=discoveries
head(y)
x=table(y)
x=as.data.frame(x)

x

x1=rep(x$y,x$Freq)
x1
# continuous

# grouping
x=precip
x
bins = seq(min(precip),max(precip)+5,by = 5)
class_x = cut(x,breaks = bins,right = FALSE)
grouped_x = table(class_x)

grouped_x
# midpoints:
lower_limit = seq(min(precip),max(precip), by =5)
upper_limit = seq(min(precip)+5,max(precip)+5,by=5)
midpt = (lower_limit+upper_limit)/2
midpt
# mean
sum(grouped_x*midpt)/sum(grouped_x)

# median : l+h(N/2-c)/f
## cumulative frequency
cf = cumsum(grouped_x)
N = sum(grouped_x)
l = lower_limit[min(which(cf>=N/2))]
f = as.numeric(grouped_x[min(which(cf>=N/2))])
c = as.numeric(cf[max(which(cf<=N/2))])
h = 1 #class height

l+h*(N/2-c)/f

# Mode : l+h(f1-f0)/(2f1-f0-f2)
l = lower_limit[which(grouped_x==max(grouped_x))]

h=5
f1 = grouped_x[which(grouped_x==max(grouped_x))]
f0 = grouped_x[which(grouped_x==max(grouped_x))-1]
f2 = grouped_x[which(grouped_x==max(grouped_x))+1]

as.numeric(l+h*(f1-f0)/(2*f1-f0-f2))

# Quantiles: 
# lower: l+h*(N/4-cf)/f
## cumulative frequency

cf = cumsum(grouped_x)
N = sum(grouped_x)
l = lower_limit[min(which(cf>=N/4))]
f = as.numeric(grouped_x[min(which(cf>=N/4))])
min(which(cf>=N/4)) 
c = (cf[min(which(cf>=N/4))-1])
min(which(cf>=N/4))
c=0 # since the quartile class is the first class
h = 5 #class height

l+h*(N/4-c)/f

# Upper:
cf = cumsum(grouped_x)
N = sum(grouped_x)
l = lower_limit[min(which(cf>=3*N/4))]
f = as.numeric(grouped_x[min(which(cf>=3*N/4))])
c = (cf[min(which(cf>=3*N/4))-1])

h = 5 #class height
l+h*(3*N/4-c)/f


# Percentiles - sir 10.1
# pth percentile: 
p = 67
cf = cumsum(grouped_x)
N = sum(grouped_x)
l = lower_limit[min(which(cf>=p*N/100))]
f = as.numeric(grouped_x[min(which(cf>=p*N/100))])
c = as.numeric(cf[min(which(cf>=p*N/100))-1])

h = 5 #class height
l+h*(p*N/100-c)/f

## deciles  
d = 4 
cf = cumsum(grouped_x) 
N = sum(grouped_x) 
l = lower_limit[min(which(cf>=d*N/10))] 
f = as.numeric(grouped_x[min(which(cf>=d*N/10))]) 
c = as.numeric(cf[min(which(cf>=d*N/10))-1]) 
h=5 

l+h*(d*N/10-c)/f
