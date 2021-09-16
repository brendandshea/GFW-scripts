
#### Blue Shark HawaiiOBS Cpue Models ####
library(emmeans)
pred2 
emmeans(model.blueshark, ~ lat + long, type="response", offset=log(1000))

library(glm.predict)
pred <- predicts(model.blueshark, "min-max;min-max; 2013-2017; log(1000)", position=1)

#Delta-lognormal?
library(fishMod)
model.blueshark.DL <- deltaLN(COUNT_BLUE_SHARK ~ lat + long + DEPART_YEAR + offset(log_hooks),  
                              ~ lat + long + DEPART_YEAR, fulldat)

plot(model.blueshark.DL$fitted, model.blueshark.DL$residuals[,"quantile"], pch=20, main="Delta Log-Normal quantile residuals")
abline( h=0, col="red")

pred2 <- data.frame(subset(fulldat, select = c("lat", "long", "DEPART_YEAR", "NUM_HKS_SET")),
                    observed = fulldat$COUNT_BLUE_SHARK, predicted = predict(model.blueshark.DL, type = "response", se.fit=TRUE))