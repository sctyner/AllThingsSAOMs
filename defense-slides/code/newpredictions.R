# add grayed out predictions

modelData_sig <- turk22 %>% mutate(type2 = ifelse(sign == 0, sign(initialEst), sign),
                                        type2 = ifelse(type == "one", type2, -1)) %>% filter(pic_id != 3132) 

modelData_sig$centersize <- as.numeric(scale(modelData_sig$size))
sub <- attr(scale(modelData_sig$size), "scaled:center")
div <- attr(scale(modelData_sig$size), "scaled:scale")

newdata <- read_csv("data/newdata_pred_glmm.csv")
newdata$category = "inside"
densneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "dens")$size), -4.902777, by = filter(newdata, type2 == -1, test_param == "dens")$size[2:3] %>% diff %>% abs)
denspos1 <- seq( -4.902777, min(filter(newdata, type2 == 1, test_param == "dens")$size), by = filter(newdata, type2 == 1, test_param == "dens")$size[2:3] %>% diff %>% abs)
newdata2_dens <- data.frame(test_param = "dens", 
                            type2 = rep(c(-1,1), c(length(densneg1), length(denspos1))), 
                            size = c(densneg1, denspos1), category = 'outside')
min1 <- min(filter(newdata, type2 == -1, test_param == "dens")$size)
max1 <- max(filter(newdata, type2 == 1, test_param == "dens")$size)
densneg1b <- seq(min1 - (max1-min1)*.1, min1, by = filter(newdata, type2 == -1, test_param == "dens")$size[2:3] %>% diff %>% abs)
denspos1b <- seq(max1, max1 + (max1-min1)*.1 , by = filter(newdata, type2 == 1, test_param == "dens")$size[2:3] %>% diff %>% abs)
newdata2b_dens <- data.frame(test_param = "dens", 
                            type2 = rep(c(-1,1), c(length(densneg1b), length(denspos1b))), 
                            size = c(densneg1b, denspos1b), category = 'outside2')
newdata2_dens <- rbind(newdata2_dens, newdata2b_dens)


recipneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "recip")$size), 4.893109, by = filter(newdata, type2 == -1, test_param == "recip")$size[2:3] %>% diff %>% abs)
recippos1 <- seq(4.893109, min(filter(newdata, type2 == 1, test_param == "recip")$size), by = filter(newdata, type2 == 1, test_param == "recip")$size[2:3] %>% diff %>% abs)
newdata2_recip <- data.frame(test_param = "recip", 
                             type2 = rep(c(-1,1), c(length(recipneg1), length(recippos1))), 
                             size = c(recipneg1, recippos1), category = 'outside')
min1 <- min(filter(newdata, type2 == -1, test_param == "recip")$size)
max1 <- max(filter(newdata, type2 == 1, test_param == "recip")$size)
recipneg1b <- seq(min1 - (max1-min1)*.1, min1, by = filter(newdata, type2 == -1, test_param == "recip")$size[2:3] %>% diff %>% abs)
recippos1b <- seq(max1, max1 + (max1-min1)*.1, by = filter(newdata, type2 == 1, test_param == "recip")$size[2:3] %>% diff %>% abs)
newdata2b_recip <- data.frame(test_param = "recip", 
                             type2 = rep(c(-1,1), c(length(recipneg1b), length(recippos1b))), 
                             size = c(recipneg1b, recippos1b), category = 'outside2')
newdata2_recip <- rbind(newdata2_recip, newdata2b_recip)
jttpneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "jttp")$size), 0, by = filter(newdata, type2 == -1, test_param == "jttp")$size[2:3] %>% diff %>% abs)
jttppos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "jttp")$size), by = filter(newdata, type2 == 1, test_param == "jttp")$size[2:3] %>% diff %>% abs)
newdata2_jttp <- data.frame(test_param = "jttp", 
                             type2 = rep(c(-1,1), c(length(jttpneg1), length(jttppos1))), 
                             size = c(jttpneg1, jttppos1), category = 'outside')
min1 <- min(filter(newdata, type2 == -1, test_param == "jttp")$size)
max1 <- max(filter(newdata, type2 == 1, test_param == "jttp")$size)
jttpneg1b <- seq(min1 - (max1-min1)*.1, min1, by = filter(newdata, type2 == -1, test_param == "jttp")$size[2:3] %>% diff %>% abs)
jttppos1b <- seq(max1, max1 + (max1-min1)*.1, by = filter(newdata, type2 == 1, test_param == "jttp")$size[2:3] %>% diff %>% abs)
newdata2b_jttp <- data.frame(test_param = "jttp", 
                              type2 = rep(c(-1,1), c(length(jttpneg1b), length(jttppos1b))), 
                              size = c(jttpneg1b, jttppos1b), category = 'outside2')
newdata2_jttp <- rbind(newdata2_jttp, newdata2b_jttp)

jttsneg1 <- seq(0, min(filter(newdata, type2 == -1, test_param == "jtts")$size), by = filter(newdata, type2 == -1, test_param == "jtts")$size[2:3] %>% diff %>% abs)
jttspos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "jtts")$size), by = filter(newdata, type2 == 1, test_param == "jtts")$size[2:3] %>% diff %>% abs)
newdata2_jtts <- data.frame(test_param = "jtts", 
                             type2 = rep(c(-1,1), c(length(jttsneg1), length(jttspos1))), 
                             size = c(jttsneg1, jttspos1), category = 'outside')
min1 <- max(filter(newdata, type2 == 1, test_param == "jtts")$size)
max1 <- max(filter(newdata, type2 == -1, test_param == "jtts")$size)
min2 <- min(filter(newdata,type2 == 1, test_param == "jtts")$size)
jttspos1b <- seq(min1 , min1 + (max1-min2)*.1, by = filter(newdata, type2 == -1, test_param == "jtts")$size[2:3] %>% diff %>% abs)
jttsneg1b <- seq(max1, max1 + (max1-min2)*.1, by = filter(newdata, type2 == 1, test_param == "jtts")$size[2:3] %>% diff %>% abs)
newdata2b_jtts <- data.frame(test_param = "jtts", 
                              type2 = rep(c(-1,1), c(length(jttsneg1b), length(jttspos1b))), 
                              size = c(jttsneg1b, jttspos1b), category = 'outside2')
newdata2_jtts <- rbind(newdata2_jtts, newdata2b_jtts)

simttbneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "simttb")$size), 0, by = filter(newdata, type2 == -1, test_param == "simttb")$size[2:3] %>% diff %>% abs)
simttbpos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "simttb")$size), by = filter(newdata, type2 == 1, test_param == "simttb")$size[2:3] %>% diff %>% abs)
newdata2_simttb <- data.frame(test_param = "simttb", 
                             type2 = rep(c(-1,1), c(length(simttbneg1), length(simttbpos1))), 
                             size = c(simttbneg1, simttbpos1), category = 'outside')
min1 <- min(filter(newdata, type2 == -1, test_param == "simttb")$size)
max1 <- max(filter(newdata, type2 == 1, test_param == "simttb")$size)
simttbneg1b <- seq(min1 - (max1-min1)*.1, min1, by = filter(newdata, type2 == -1, test_param == "simttb")$size[2:3] %>% diff %>% abs)
simttbpos1b <- seq(max1, max1 + (max1-min1)*.1, by = filter(newdata, type2 == 1, test_param == "simttb")$size[2:3] %>% diff %>% abs)
newdata2b_simttb <- data.frame(test_param = "simttb", 
                              type2 = rep(c(-1,1), c(length(simttbneg1b), length(simttbpos1b))), 
                              size = c(simttbneg1b, simttbpos1b), category = 'outside2')
newdata2_simttb <- rbind(newdata2_simttb, newdata2b_simttb)

samettpneg1 <- seq(0, min(filter(newdata, type2 == -1, test_param == "samettp")$size),  by = filter(newdata, type2 == -1, test_param == "samettp")$size[2:3] %>% diff %>% abs)
samettppos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "samettp")$size), by = filter(newdata, type2 == 1, test_param == "samettp")$size[2:3] %>% diff %>% abs)
newdata2_samettp <- data.frame(test_param = "samettp", 
                             type2 = rep(c(-1,1), c(length(samettpneg1), length(samettppos1))), 
                             size = c(samettpneg1, samettppos1), category = 'outside')
min1 <- min(filter(newdata, type2 == -1, test_param == "samettp")$size)
max1 <- max(filter(newdata, type2 == 1, test_param == "samettp")$size)
samettpneg1b <- seq(max1 , max1 + (max1-min1)*.1, by = filter(newdata, type2 == -1, test_param == "samettp")$size[2:3] %>% diff %>% abs)
samettppos1b <- seq(max1, max1 + (max1-min1)*.1, by = filter(newdata, type2 == 1, test_param == "samettp")$size[2:3] %>% diff %>% abs)
newdata2b_samettp <- data.frame(test_param = "samettp", 
                              type2 = rep(c(-1,1), c(length(samettpneg1b), length(samettppos1b))), 
                              size = c(samettpneg1b, samettppos1b), category = 'outside2')
newdata2_samettp <- rbind(newdata2_samettp, newdata2b_samettp)

newdata2 <- rbind(newdata2_dens, newdata2_jttp, newdata2_jtts, newdata2_recip, newdata2_recip, newdata2_samettp, newdata2_simttb)
newdata2$type2 <- as.factor(newdata2$type2)
newdata2$pic_id <- sample(1001:1050, replace = T, size = nrow(newdata2))
newdata2$nick_name <- sample(LETTERS, replace = T, size = nrow(newdata2))
newdata2$centersize <- (newdata2$size - sub)/div


newdata3 <- rbind(newdata[,-7], newdata2[,c("test_param", "type2", "size", "centersize", "pic_id", "nick_name", "category")])

newdata3$lps_param <- with(newdata3, interaction(test_param, as.factor(type2)))
newdata3$predictfinal <- predict(model2randomscalesize, newdata3, allow.new.levels = T, type = 'response')
