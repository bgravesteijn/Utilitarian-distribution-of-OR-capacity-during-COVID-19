# File to estimate the cost-effectivenss of COVID on IC

# ASSUMPTIONS:
# - length of rehabilitations is on average one week per day on the ICU
# - The current life expectancy is applicable for the IC 

# Load package
library(triangle)
library(ggplot2)
library(dampack)

df_le <- read.csv("data/NLD_bltper_1x1_2018.csv")


# Parameters
n_iter     <- 1000    # number of iterations 
n_patients <- 1000    # number of patients


# create a data frame for the results
df_res_ce <- expand.grid(it    = 1:n_iter,
                      tx    = c("no","yes"),
                      costs = NA,
                      qalys = NA)

set.seed(123)  # set the seeds


# Create vectors from the input parameter distributions 
v_c_icu_d   <- rtriangle(n = n_iter, a = 2000, c = 2555, b = 3000)       # Triangular cost distribution on the ICU per day
v_c_rehab_d <- rtriangle(n = n_iter, a = 300,  c = 460, b = 600)         # Triangular cost distribution for rehab per day
v_surv      <- rbeta(n = n_iter, shape1 = 2905 - 867, shape2 = 867)      # bhttps://allecijfers.nl/nieuws/statistieken-over-het-corona-virus-en-covid19/#Corona_kerncijfers

qol_beta_par <- beta_params(mean = 0.666, sigma = 0.28)  # Estimate the distribution parameters 
v_qol        <- rbeta(n = n_iter, shape1 = qol_beta_par$alpha, shape2 = qol_beta_par$beta)                # https://ccforum.biomedcentral.com/articles/10.1186/cc8848
v_age        <- rnorm(n = n_iter, mean = 62.3, sd = 11.2 )                # https://www.stichting-nice.nl/COVID_rapport.pdf
los_lnorm_par <- lnorm_params(m = 19.7, v = (14.3)^2)
v_los_icu     <- rlnorm(n = n_iter, meanlog = los_lnorm_par $mu, sdlog  = los_lnorm_par $sigma)  # https://www.stichting-nice.nl/COVID_rapport.pdf

df_param_ce <- data.frame(c_icu_d = v_c_icu_d,
                         c_rehab_d = v_c_rehab_d,
                         surv = v_surv,
                         qol = v_qol,
                         age = v_age,
                         los_icu = v_los_icu)

for(i in 1:n_iter){  
  #costs
  c_icu_d   <- v_c_icu_d[i]   # select the parameter value for this iteration
  c_rehab_d <- v_c_rehab_d[i] # select teh parameter value for this interation 
  
  #length of stay
  los_icu <- v_los_icu[i]             # on the ICU  # @BG I think we need a distribution for this - maybe from 
  los_rehab <- los_icu * 7  # 1 dag ic = week revalideren
  
  #Survival/qol
  surv <- v_surv[i]
  qol  <- v_qol [i]
  
  # average age and life expectancy (le)
  age <- v_age[i]
  le  <- df_le$ex[df_le$Age == ceiling(age)]   # remaining life years
  
  # QALYs in survivors
  LY   <- le * surv  
  QALY <- LY * qol
  
  #costs in survivors
  c_surv <- los_icu * c_icu_d + los_rehab * c_rehab_d
  
  #costs in nonsurvivors
  c_nonsurv <- 0.5 * los_icu * c_icu_d
  
  #total costs
  c_tot <- n_patients * surv * c_surv + n_patients * (1 - surv) * c_nonsurv
  
  #total QALY
  qaly_tot <- n_patients * surv * QALY
  
  #total CE
  df_res_ce[df_res_ce$it == i & df_res_ce$tx == "yes", "costs"] <- c_tot
  df_res_ce[df_res_ce$it == i & df_res_ce$tx == "yes", "qalys"] <- qaly_tot
  
  df_res_ce[df_res_ce$it == i & df_res_ce$tx == "no", "costs"] <- 0
  df_res_ce[df_res_ce$it == i & df_res_ce$tx == "no", "qalys"] <- 0
  
}


#quantile(costs/qalys, probs = c(0.5, 0.025, 0.975))



# Make a PSA object
df_psa_data <- make_psa_obj(cost = as.data.frame(cbind(df_res_ce$costs[df_res_ce$tx == "yes"], df_res_ce$costs[df_res_ce$tx == "no"])),
                            effectiveness = as.data.frame(cbind(df_res_ce$qalys[df_res_ce$tx == "yes"], df_res_ce$qalys[df_res_ce$tx == "no"])),
                         parameters = df_param_ce,
                         strategies = c("ICU_treatment", "no_ICU_Treatment"),
                         currency = "Euro")
# custom print and summary methods
print(df_psa_data)
summary(df_psa_data)

# summary() gives mean cost and effect for each strategy
sum_cdiff <- summary(df_psa_data)

# calculate icers
icers <- calculate_icers(sum_cdiff$meanCost,
                         sum_cdiff$meanEffect,
                         sum_cdiff$Strategy)


icers 

# custom plot method; see ?plot.psa for options
plot_ce <- plot(df_psa_data)
plot(df_psa_data)
tiff(filename = "figures/main_results/ce_plane_icu_covid_dampack.tiff")
plot_ce
dev.off()


v_wtp <- seq(from = 0, to = 100000, by = 1000)

ceac_obj <- ceac(wtp = v_wtp, df_psa_data)
ceac_obj
summary(ceac_obj)
plot_ceac <- plot(ceac_obj) +
  labs(title = "CEAC", y = "Pr(Cost-effective) at WTP") +
  xlab(label = "Willingness to Pay (Thousand Euro/QALY)")

tiff(filename = "figures/main_results/ceac_icu_covid.tiff")
plot_ceac
dev.off()

# Create the expected value of perfect informaion objects 
evpi_obj <- calc_evpi(psa = df_psa_data, wtp = v_wtp, pop = 1)
plot_evpi <- plot(evpi_obj) + 
  ylab(label = "EVPI (Euro)") +
  xlab(label = "Willingness to Pay (Thousand Euro/QALY)") # Plot the expected value of perfect information 
plot(evpi_obj)

tiff(filename = "figures/main_results/evpi_covid.tiff")
plot_evpi  # Plot the expected value of perfect information 
dev.off()



### OLD ### 

ce_pl <- ggplot(df_res_ce[df_res_ce$tx == "yes", ], aes(x = qalys, y = costs))+
  geom_point()+
  geom_vline(xintercept = 0, lty = 1)+
  geom_hline(yintercept = 0, lty = 1)+
  geom_abline(slope = 80000, intercept = 0, lty = 2) +
  theme_bw()+
  labs(x = "QALYs", y = "Costs, Euros")+
  theme(text=element_text(size = 16))

ce_pl
tiff(filename = "figures/main_results/ce_plane_icu_covid.tiff")
ce_pl
dev.off()

#####ceac
library(BCEA)
ce <- bcea(e = matrix(df_res_ce$qalys, ncol = 2), 
           c = matrix(df_res_ce$costs, ncol = 2), 
           ref = 2)
ceac.plot(ce, graph = "ggplot2")

tiff(filename = "figures/main_results/ceac_covid_ICU.tiff")

ceac.plot(ce, graph = "ggplot2")
dev.off()

evi.plot(he = ce)


## Our surgeries
load("output/df_res_main.Rdata")
load("output/res_psa.Rdata")

## The number of QALYs loss by delaying with los_ICU of COVID-19
loss_delay <- df_res_main$urg * 12 / 365 * los_icu #loss in QALYs by delaying with los of ICU of COVID-19


