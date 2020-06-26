
df_results_pooled
summary(df_results_pooled)



df_qaly_lpw <- expand.grid(Pop = unique(df_results_pooled$Label),  # Disease name 
                        QALY_med = NA,
                        QALY_lo = NA,
                        QALY_hi = NA)  

n_weeks <- paste("loss", 1:52, sep = "")
df_qaly_lpw <-  cbind(df_qaly_lpw, setNames( lapply(n_weeks, function(x) x=NA), n_weeks) )
head(df_qaly_lpw)


df_qaly_lpw$QALY_med <- df_results_pooled$QALY_med[df_results_pooled$delay == 2]
df_qaly_lpw$QALY_lo  <- df_results_pooled$QALY_lo[df_results_pooled$delay == 2]
df_qaly_lpw$QALY_hi  <- df_results_pooled$QALY_hi[df_results_pooled$delay == 2]

head(df_qaly_lpw)

write.csv(df_qaly_lpw,            file = "output/df_qaly_lpw.csv")

