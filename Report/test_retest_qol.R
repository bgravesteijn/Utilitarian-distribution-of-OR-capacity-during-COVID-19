u_d1   <- c(0.38,0.43,0.78,0.94,0.69,0.84,0.75,0.98)
se_d1  <- c(0.02,0.03,0.03,0.03,0.03,0.03,0.04,0.00)
u_d2   <- c(0.49,0.57,0.83,0.96,0.71,0.90,0.89,0.98)
se_d2  <- c(0.02,0.02,0.02,0.01,0.004,0.01,0.01,0.01)



md <- u_d2-u_d1
md_se <- sqrt(se_d1^2+se_d2^2)
md_lo <- sprintf("%.2f",md-qt(df=18, p=0.95)*md_se)
md_hi <- sprintf("%.2f",md+qt(df=18, p=0.95)*md_se)

md_ci <- paste(sprintf("%.2f",md)," (", md_lo, " - ", md_hi,")", sep="")

sum(md_hi<0)

t.test(u_d2, u_d1, paired = TRUE)

library(ggplot2)
library(BlandAltmanLeh)
x <- bland.altman.plot(group1 = u_d2, group2 = u_d1, 
                  graph.sys = "ggplot2")
y <- x+theme_bw()+geom_hline(yintercept = 0)+labs(x="Mean of QoL scored at two sessions",
                                             y="Mean of QoL scored at two sessions")+
  theme(text=element_text(size=16))+geom_point(cex=3)
y
ggsave(y, filename = "figures/main_results/ba_plot_qol.tiff", device = "tiff")

df <- data.frame(prepost=rep(c("Pre","Post"), 4*2),
                 utility=c(u_d1, u_d2),
                 day=c(rep(1,length(u_d1)), rep(2, length(u_d2))),
                 ind=rep(1:length(u_d1), each=2))
diff_qol <- df$utility[df$prepost=="Post"]-df$utility[df$prepost=="Pre"]
t.test(diff_qol[1:4], diff_qol[5:8], paired = TRUE)
