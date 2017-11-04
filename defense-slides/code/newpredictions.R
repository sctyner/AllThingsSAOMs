# add grayed out predictions

newdata <- read_csv("data/newdata_pred_glmm.csv")
newdata$category = "inside"
densneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "dens")$size), -4.902777, by = filter(newdata, type2 == -1, test_param == "dens")$size[2:3] %>% diff %>% abs)
denspos1 <- seq( -4.902777, min(filter(newdata, type2 == 1, test_param == "dens")$size), by = filter(newdata, type2 == 1, test_param == "dens")$size[2:3] %>% diff %>% abs)
newdata2_dens <- data.frame(test_param = "dens", 
                            type2 = rep(c(-1,1), c(length(densneg1), length(denspos1))), 
                            size = c(densneg1, denspos1), category = 'outside')
recipneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "recip")$size), 0, by = filter(newdata, type2 == -1, test_param == "recip")$size[2:3] %>% diff %>% abs)
recippos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "recip")$size), by = filter(newdata, type2 == 1, test_param == "recip")$size[2:3] %>% diff %>% abs)
newdata2_recip <- data.frame(test_param = "recip", 
                             type2 = rep(c(-1,1), c(length(recipneg1), length(recippos1))), 
                             size = c(recipneg1, recippos1), category = 'outside')
jttpneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "jttp")$size), -4.902777, by = filter(newdata, type2 == -1, test_param == "jttp")$size[2:3] %>% diff %>% abs)
jttppos1 <- seq( -4.902777, min(filter(newdata, type2 == 1, test_param == "jttp")$size), by = filter(newdata, type2 == 1, test_param == "jttp")$size[2:3] %>% diff %>% abs)
newdata2_jttp <- data.frame(test_param = "jttp", 
                             type2 = rep(c(-1,1), c(length(jttpneg1), length(jttppos1))), 
                             size = c(jttpneg1, jttppos1), category = 'outside')
jttsneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "jtts")$size), -4.902777, by = filter(newdata, type2 == -1, test_param == "jtts")$size[2:3] %>% diff %>% abs)
jttspos1 <- seq( -4.902777, min(filter(newdata, type2 == 1, test_param == "jtts")$size), by = filter(newdata, type2 == 1, test_param == "jtts")$size[2:3] %>% diff %>% abs)
newdata2_jtts <- data.frame(test_param = "jtts", 
                             type2 = rep(c(-1,1), c(length(jttsneg1), length(jttspos1))), 
                             size = c(jttsneg1, jttspos1), category = 'outside')
simttbneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "simttb")$size), -4.902777, by = filter(newdata, type2 == -1, test_param == "simttb")$size[2:3] %>% diff %>% abs)
simttbpos1 <- seq( -4.902777, min(filter(newdata, type2 == 1, test_param == "simttb")$size), by = filter(newdata, type2 == 1, test_param == "simttb")$size[2:3] %>% diff %>% abs)
newdata2_simttb <- data.frame(test_param = "simttb", 
                             type2 = rep(c(-1,1), c(length(simttbneg1), length(simttbpos1))), 
                             size = c(simttbneg1, simttbpos1), category = 'outside')
samettpneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "samettp")$size), -4.902777, by = filter(newdata, type2 == -1, test_param == "samettp")$size[2:3] %>% diff %>% abs)
samettppos1 <- seq( -4.902777, min(filter(newdata, type2 == 1, test_param == "samettp")$size), by = filter(newdata, type2 == 1, test_param == "samettp")$size[2:3] %>% diff %>% abs)
newdata2_samettp <- data.frame(test_param = "samettp", 
                             type2 = rep(c(-1,1), c(length(samettpneg1), length(samettppos1))), 
                             size = c(samettpneg1, samettppos1), category = 'outside')

newdata %>% group_by(test_param, type2) %>% 
  summarize(minp = min(size), maxp = max(size), diff = diff(size[1:2])) -> summnewdat
summnewdat$est <- rep(c(-4.902777, -3.4498957, 3.3403020, 
                     4.8931092, 1.3290509, 10.0908294), each = 2)
head(summnewdat)
summnewdat %>% filter(type2 == -1)
