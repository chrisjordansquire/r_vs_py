# This is the code for an example that popped up on the statsmodels
# list a year or so ago, showing a bizarre example of how things
# can work when R is creating model matrices for the user. 

# (At least that's what the discussion thread was about. I'm not
# sure if the real issue is the model matrix or how the anova
# command works.)

# n is the sample size
# d1 is the number of categories in x
# d2 is the number of categories in y
# It's left arbitrary just to show this works for any d1, d2
n<-200
d1<-3
d2<-5

x<-sample(1:d1, n, replace=T)
y<-sample(1:d2, n, replace=T)

x<-factor(x)
y<-factor(y)

X<-model.matrix(~x*y)
k<-dim(X)[2]

# Generate some output values with an
# arbitrary model
out<- X %*% 1:k + rnorm(n, sd=2)

# Fit two different models, with m1 nested in m2
# in the sense that y*x is y+x+y:x
m1<-lm(out~y+y:x)
m2<-lm(out~y*x)

# Now look at the difference in sum of squares between them
anova(m1,m2)

# The models are the "same" in the sense that they have the
# same fitted values

# However, the models are not the same. 
# The model matrices are not the same. 
all(model.matrix(m1) == model.matrix(m2))
# And the interaction coefficients are not the same
(coef(m1)-coef(m2))/coef(m1)
