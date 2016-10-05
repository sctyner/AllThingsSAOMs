# simulate 5 networks from M2 for prelim.

# Steps:
# 1 Simulate 1000 2nd waves from M1
# 2 Fit the M1 and M2 to each of the 1000 sims from above
# 3 Compare the fitted values M1 & M2

setwd("~/Desktop/NetworksResearch/NetworksVizInference/")
source("Code/00e_small_friends.R")
eff_models_smallFriends <- readRDS("Data/eff_models_smallFriends.RDS")
m2_eff_struct <- eff_models_smallFriends[[39]]

sim_frm_m2 <- function(eff_struct, rep, my_dat=mysmalldata, ...){
  # browser()
  m2_data <- NULL
  n <- rep
  m2_net <- siena07( myalgorithm2, data = my_dat, returnDeps = TRUE, effects = eff_struct, batch=TRUE, verbose = FALSE, silent = TRUE)
  eff_struct2 <- eff_struct
  eff_struct2$initialValue[eff_struct2$include] <- alt_mod_sv
  sim_algorithm <- sienaAlgorithmCreate( projname = "sim_model", cond = FALSE,
                                         useStdInits = FALSE, nsub = 0 , simOnly = TRUE)
  for (i in 1:n){
    sim_ans <-  siena07( sim_algorithm, data = my_dat, effects = eff_struct2, returnDeps = TRUE)
    net_df <- merge(data.frame(sim_ans$sims[[1000]][[1]][[1]][[1]])[,-3], 
                    data.frame(id = 1:16), by.x = "X1", by.y = "id",
                    all = T)
    for (j in 1:nrow(net_df)){
      if (!(net_df$X1[j] %in% net_df$X2) & is.na(net_df$X2[j])){
        net_df$X2[j] <- net_df$X1[j]
      } else {net_df$X2[j] <- net_df$X2[j]}
    }
    net_df$count <- i 
    print(i)
    m2_data <- rbind(m2_data, net_df)
  }
  return(m2_data)
}  

#create 1000 networks
m2sims20 <- sim_frm_m2(m2_eff_struct, rep = 20)

#write.csv(m2sims20, file = "../../Dissertation - LETS GO/AllThingsSAOMs/VisInfDocs/Data/m2sims20.csv")

