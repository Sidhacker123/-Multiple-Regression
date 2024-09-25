prostate_cancer <- read.csv("prostate_cancer.csv")
View(prostate_cancer)
head(prostate_cancer)
# For illustration purposes, we will create X and y
X <- prostate_cancer[1:8] # 97x8 matrix containing input variables
y <- prostate_cancer[9]   # 97x1 vector containing output variable
X <- cbind(1,X)       # we attach a 97x1 vector of 1s to X to account for the intercept
colnames(X)[1] <- "Intercept"
head (X)
head(y)
# normal matrix function
X <- as.matrix(X)
y <- as.matrix(y)
beta_matrix = solve(t(X)%*%X,t(X)%*%y)
beta_matrix
# lm() function
lm.fit <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason +pgg45, data = prostate_cancer)
beta_lm <- lm.fit$coefficients
beta_lm
summary(lm.fit)
