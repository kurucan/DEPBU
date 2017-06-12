source("config_def.R")

algos_allowed <- c("KD-IC-NML", "DETS")
algo.agg <- "DETS"
algo.bottom <- "KD-IC-NML"


# Min bandwith for KDE
# min_bandwith <- 10^-6

# Length of seasonal cycles
m_1 <- 48
m_2 <- 336

# Number of past observations to use for base models
n_past_obs_kd    <- 60 *48
n_past_obs_tbats <- (31 + 28 + 31 + 30)*48

# Size of the sample to generate
M <- 120 * 48 # 5760
  
# probability levels
q_probs <- seq(M)/(M + 1)
taus <- rev(seq(1, 99)/100)





