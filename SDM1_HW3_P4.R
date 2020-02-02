library(boot)

set.seed(1)
x = rnorm(100)
y=x-2*x^2+rnorm(100)

plot(x,y, main = "Plot of the equation given")

df = data.frame(x,y)

#### 1st degree polynomial ################
glm_fit_1 = glm(y~x)
cv_models = cv.glm(data = df,glm_fit_1)
error_1 = cv_models$delta[1]

#### 2nd degree polynomial ################
glm_fit_2 = glm(y ~ poly(x, 2))
cv_models_2 = cv.glm(data = df,glm_fit_2)
error_2 = cv_models_2$delta[1]

#### 3rd degree polynomial ################
glm_fit_3 = glm(y ~ poly(x, 3))
cv_models_3 = cv.glm(data = df,glm_fit_3)
error_3 = cv_models_3$delta[1]

#### 4th degree polynomial ################
glm_fit_4 = glm(y ~ poly(x, 4))
cv_models_4 = cv.glm(data = df,glm_fit_4)
error_4 = cv_models_4$delta[1]

which.min(c(error_1,error_2,error_3,error_4))
min(error_1,error_2,error_3,error_4)

summary(glm_fit_1)
summary(glm_fit_2)
summary(glm_fit_3)
summary(glm_fit_4)
