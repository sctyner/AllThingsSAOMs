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
recipneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "recip")$size), 4.893109, by = filter(newdata, type2 == -1, test_param == "recip")$size[2:3] %>% diff %>% abs)
recippos1 <- seq(4.893109, min(filter(newdata, type2 == 1, test_param == "recip")$size), by = filter(newdata, type2 == 1, test_param == "recip")$size[2:3] %>% diff %>% abs)
newdata2_recip <- data.frame(test_param = "recip", 
                             type2 = rep(c(-1,1), c(length(recipneg1), length(recippos1))), 
                             size = c(recipneg1, recippos1), category = 'outside')
jttpneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "jttp")$size), 0, by = filter(newdata, type2 == -1, test_param == "jttp")$size[2:3] %>% diff %>% abs)
jttppos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "jttp")$size), by = filter(newdata, type2 == 1, test_param == "jttp")$size[2:3] %>% diff %>% abs)
newdata2_jttp <- data.frame(test_param = "jttp", 
                             type2 = rep(c(-1,1), c(length(jttpneg1), length(jttppos1))), 
                             size = c(jttpneg1, jttppos1), category = 'outside')
jttsneg1 <- seq(0, min(filter(newdata, type2 == -1, test_param == "jtts")$size), by = filter(newdata, type2 == -1, test_param == "jtts")$size[2:3] %>% diff %>% abs)
jttspos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "jtts")$size), by = filter(newdata, type2 == 1, test_param == "jtts")$size[2:3] %>% diff %>% abs)
newdata2_jtts <- data.frame(test_param = "jtts", 
                             type2 = rep(c(-1,1), c(length(jttsneg1), length(jttspos1))), 
                             size = c(jttsneg1, jttspos1), category = 'outside')
simttbneg1 <- seq(max(filter(newdata, type2 == -1, test_param == "simttb")$size), 0, by = filter(newdata, type2 == -1, test_param == "simttb")$size[2:3] %>% diff %>% abs)
simttbpos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "simttb")$size), by = filter(newdata, type2 == 1, test_param == "simttb")$size[2:3] %>% diff %>% abs)
newdata2_simttb <- data.frame(test_param = "simttb", 
                             type2 = rep(c(-1,1), c(length(simttbneg1), length(simttbpos1))), 
                             size = c(simttbneg1, simttbpos1), category = 'outside')
samettpneg1 <- seq(0, min(filter(newdata, type2 == -1, test_param == "samettp")$size),  by = filter(newdata, type2 == -1, test_param == "samettp")$size[2:3] %>% diff %>% abs)
samettppos1 <- seq(0, min(filter(newdata, type2 == 1, test_param == "samettp")$size), by = filter(newdata, type2 == 1, test_param == "samettp")$size[2:3] %>% diff %>% abs)
newdata2_samettp <- data.frame(test_param = "samettp", 
                             type2 = rep(c(-1,1), c(length(samettpneg1), length(samettppos1))), 
                             size = c(samettpneg1, samettppos1), category = 'outside')

newdata2 <- rbind(newdata2_dens, newdata2_jttp, newdata2_jtts, newdata2_recip, newdata2_recip, newdata2_samettp, newdata2_simttb)
newdata2$type2 <- as.factor(newdata2$type2)
newdata2$pic_id <- sample(1001:1050, replace = T, size = nrow(newdata2))
newdata2$nick_name <- sample(LETTERS, replace = T, size = nrow(newdata2))
newdata2$centersize <- (newdata2$size - sub)/div


newdata3 <- rbind(newdata[,-7], newdata2[,c("test_param", "type2", "size", "centersize", "pic_id", "nick_name", "category")])

newdata3$lps_param <- with(newdata3, interaction(test_param, as.factor(type2)))
newdata3$predictfinal <- predict(model2randomscalesize, newdata3, allow.new.levels = T, type = 'response')
