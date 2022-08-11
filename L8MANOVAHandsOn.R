# install packages
install.packages("mvnormtest")
install.packages("car")

# load packages
library("mvnormtest")
library("car")

##Load Data

### Question Set Up
##It is well-known that men are more likely to have heart attacks than women. 
#How does gender (sex) influence some of the heart attack predictors 
#like resting blood pressure (trestbps) and cholesterol (chol)?

sapply(heartAttacks, function(x)sum(is.na(x)))

#Make sure variables are numeric
str(heartAttacks$trestbps)
str(heartAttacks$chol)

##Subsetting
keepers <- c("trestbps", "chol")
heartAttacks2 <- heartAttacks[keepers]

# Then limit the number of rows:
heartAttacks3 <- heartAttacks2[1:5000,]

head(heartAttacks3)

sapply(heartAttacks3, function(x)sum(is.na(x)))

##Format as a Matrix
heartAttacks4 <- as.matrix(heartAttacks3)

#define character vector
x <- c('1', '2', '3', NA, '4', 'Hey')

#convert to numeric vector
x_num <- as.numeric(x)

#display numeric vector
x_num

# Let's check out how many NAs we introduced by coercion
sapply(heartAttacks4, function(x)sum(is.na(x)))

heartAttacks5 <- na.omit(heartAttacks4)

# Did we remove them?
sapply(heartAttacks5, function(x)sum(is.na(x)))

##Format as a Matrix
heartAttacks5 <- as.matrix(heartAttacks4)
## Sample Size

##Multivariate Normality

mshapiro.test(t(heartAttacks5))

##Homogeneity of Variance

leveneTest(heartAttacks3$trestbps,
heartAttacks3$chol, data=heartAttacks3)

# as noted above we know these numbers are strings so 
#lets convert to numeric
heartAttacks3$trestbps <- as.numeric(heartAttacks3$trestbps)
# Let's check out how many NAs we introduced by coercion
sapply(heartAttacks3, function(x)sum(is.na(x)))

# Let's remove those 
heartAttacks3 <- na.omit(heartAttacks3)
sapply(heartAttacks3, function(x)sum(is.na(x)))

leveneTest(heartAttacks3$trestbps, 
heartAttacks3$chol, data=heartAttacks3)




