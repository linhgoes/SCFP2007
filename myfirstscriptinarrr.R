poop <- read.table("atusact_2009.dat",sep="," , header=TRUE)
TRTO_LN <- poop$TRTO_LN
summary(TRTO_LN)
TRTO_LN.filtered <- poop$TRTO_LN[which(poop$TRTO_LN < 100 & poop$TRTO_LN > 0)]
summary(TRTO_LN.filtered)

dim(TRTO_LN.filtered) # only works on matrices
length(TRTO_LN.filtered)

dim(poop)
length(poop) # will only return length of the 
			 # first row (ie the number of columns)

# Creates PNG
png("histogram_TRTO_LN_filtered.png")
hist(TRTO_LN.filtered, main="Thomson!",xlab="minutes",ylab="number of people")
dev.off() # turns off png drawer

# sample data to fit to a linear regression
x <- 1:50
y <- runif(50,1,100) #random number between 1 and 100, 50 times
plot(x,y)
linear.regression <- lm(y~x) # calculate linear regression
summary(linear.regression)
abline(linear.regression) # overlay a line over an already produced plot