# Update allocation probability after posterior draws
allocationUpdate <- function(post_y_all, n_samp){
  pt_max0 <- apply(post_y_all, 1, which.max) # Identify trt with max prob
  pt_max <- c(mean(pt_max0==1), mean(pt_max0==2),
              mean(pt_max0==3), mean(pt_max0==4)) # Compute relative freq
  V_0 <- min(sum(pt_max[-1] * (n_samp[-1]+1)/(n_samp[1]+1)),
             max(pt_max[-1])) # Allocation prob for control
  V_t <- c(V_0, pt_max[-1]) # Allocation probs for all treatments
  V_t_norm <- V_t/sum(V_t) # Normalize allocation probabilities
  return(V_t_norm)
}
