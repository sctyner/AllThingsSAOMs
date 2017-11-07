# fit full model from Pearson, Steglich, Snijders 2006
library(RSiena)
alc <- as.matrix(read.table("data/s50data/s50-alcohol.dat"))
smok <- as.matrix(read.table("data/s50data/s50-smoke.dat"))
mjane <- as.matrix(read.table("data/s50data/s50-drugs.dat"))

friend.data.w1 <- as.matrix(read.table("data/s50data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("data/s50data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("data/s50data/s50-network3.dat"))
friendshipData <- array(c(friend.data.w1, friend.data.w2, friend.data.w3), dim = c(50, 50, 3))
friendship <- sienaDependent(friendshipData)
alcohol <- varCovar(alc)
smoking <- varCovar(smok)
weed <- varCovar(mjane)
mydata <- sienaDataCreate(friendship, alcohol, smoking, weed)
M1eff <- getEffects(mydata)
M2eff <- includeEffects(M1eff, nbrDist2, interaction1 = "")
M2eff <- includeEffects(M2eff, sameX, interaction1 = "smoking")
M2eff <- includeEffects(M2eff, altX, interaction1 = "smoking")
M2eff <- includeEffects(M2eff, egoX, interaction1 = "smoking")
M2eff <- includeEffects(M2eff, sameX, interaction1 = "alcohol")
M2eff <- includeEffects(M2eff, altX, interaction1 = "alcohol")
M2eff <- includeEffects(M2eff, egoX, interaction1 = "alcohol")
M2eff <- includeEffects(M2eff, sameX, interaction1 = "weed")
M2eff <- includeEffects(M2eff, altX, interaction1 = "weed")
M2eff <- includeEffects(M2eff, egoX, interaction1 = "weed")

myalgorithm <- sienaAlgorithmCreate( projname = 's50' , n3 = 1000)
ests_null <- siena07( myalgorithm, data = mydata, returnDeps = FALSE, 
                      effects = M2eff, batch=TRUE, verbose = FALSE, silent = TRUE)
library(netvizinf)

sims_tabs_model <- saom_simulate(dat = mydata, struct = M2eff, 
                                 parms = c(ests_null$rate, ests_null$theta), N = 1000)
tabs_data_simsdf <- sims_to_df(sims = sims_tabs_model)

tabs_data_simsdfw2 <- filter(tabs_data_simsdf, wave == 1)

fdw2 <- data.frame(friend.data.w2)
fdw2$from <- paste0("V", 1:50)
fdw2 %>% gather(to, val, -from) %>% filter(val > 0) -> wave2dfedges

nodes <- data.frame(id = paste0("V", 1:50), drink = alc[,2], smoke = smok[,2], mj = mjane[,2])
wave2netdf <- merge(wave2dfedges, nodes, by.x = "from", by.y = "id", all = T)
wave2netdf$label <- parse_number(wave2netdf$from)


p3 <- ggplot(data = wave2netdf) + 
  geom_net(aes(from_id = from, to_id = to, label=label), labelcolour = 'white', 
           layout.alg = "fruchtermanreingold", size = 6, fontsize = 4, vjust = .5,
           directed = T, arrowgap = .02, arrowsize = .5, color = 'black') + 
  theme_net() + 
  labs( title = "Wave 2 of Bigger Friends Data")

fdw1 <- data.frame(friend.data.w1)
fdw1$from <- paste0("V", 1:50)
fdw1 %>% gather(to, val, -from) %>% filter(val > 0) -> wave1dfedges

nodesw1 <- data.frame(id = paste0("V", 1:50), drink = alc[,1], smoke = smok[,1], mj = mjane[,1])
wave1netdf <- merge(wave1dfedges, nodesw1, by.x = "from", by.y = "id", all = T)
wave1netdf$label <- parse_number(wave1netdf$from)

p1 <- ggplot(data = wave1netdf) + 
  geom_net(aes(from_id = from, to_id = to, label=label), labelcolour = 'white', 
           layout.alg = "fruchtermanreingold", size = 6, fontsize = 4, vjust = .5,
           directed = T, arrowgap = .02, arrowsize = .5, color = 'black') + 
  theme_net() + 
  labs(title = "Wave 1 of Bigger Friends Data")


tabs_data_simsdfw2 %>% group_by(from, to) %>% count %>% filter(n > 50) %>% ungroup() %>%
  mutate(from = paste0("V", from), to = ifelse(is.na(to), NA, paste0("V", to))) -> average_tab_net
average_tab_net$weight <- average_tab_net$n / max(average_tab_net$n)
average_tab_net$label <- parse_number(average_tab_net$from)
average_tab_net <- arrange(average_tab_net, weight)
ncolors <- length(unique(average_tab_net$n))
colors <- tweenr::tween_color(list("grey40", 'red'), n = ncolors)[[1]]
average_tab_net$ecolors <- rep(colors, table(average_tab_net$n) %>% as.numeric())


p2 <- ggplot(data = average_tab_net) + 
  geom_net(aes(from_id = from, to_id = to, label=label, linewidth = weight), labelcolour = 'white', 
           layout.alg = "fruchtermanreingold", size = 6, fontsize = 4, vjust = .5,
           directed = T, arrowgap = .02, arrowsize = .5, color = 'black', ecolour = average_tab_net$ecolors) + 
  theme_net() + 
  labs(title = "Average Network, Model from Pearson et al.")
