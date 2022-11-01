#The geometric mean
out = NULL
sdvals = seq(0, 10, 0.1)
for(i in 1:length(sdvals)){
d = rnorm(500, 20, sd=sdvals[i])  
d = d[which(d>0)]
out[i] = exp(mean(log(d)))}
plot(sdvals, out, las=1, xlab="SD", ylab="Geometric mean")
abline(h=20)

#Simulating data
x = rnorm(n=100, mean=5, sd=1)
mean(x)
sd(x)
CV = (sd(x)/mean(x))
hist(x, las=1, main="")

#Assessing uncertainty in variables
#First example: Bootstrapping
#To ensure reproducibility of the results set the seed
set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))

#for-loop for the sd
out = NULL
for(i in 1:1000){
sample = sample(x, replace=TRUE)  
out[i] = mean(sample)}
hist(out, las=1, main="")
sd(out)
se_x


#95% confidence interval:
quantile(out, c(0.025, 0.975))
#Recall that we could also have derived the 95% confidence interval analytically as ±1.96SE.
mean(x) - 1.96*se_x
mean(x) + 1.96*se_x

#for-loop for the CV
out = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)  
  out[i] = sd(sample)/mean(sample)}
hist(out, las=1, main="")
sd(out) #SE of the CV
se_x

#95% confidence interval:
quantile(out, c(0.025, 0.975))
#Recall that we could also have derived the 95% confidence interval analytically as ±1.96SE.
mean(x) - 1.96*se_x
mean(x) + 1.96*se_x

#creating a matrix
mat = matrix(nrow = 1000, ncol = 2)
for(i in 1:1000){
x = rnorm(50, 10, 2)  
log_x = log(x)
mat[i, 1] = sd(x)/mean(x)
mat[i, 2] = sd(log_x)}
plot(mat[1:1000, 1], mat[1:1000, 2], xlab = "CV", ylab = "sd_log")

#adding a line
abline(0, 1)
