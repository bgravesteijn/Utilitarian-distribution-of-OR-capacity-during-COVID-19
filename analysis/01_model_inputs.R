## This file contains the model inputs for the operation room model 
## from the Erasmus MC

# Load data
#param <- data.frame(readxl::read_xlsx("Data/Model parameters.xlsx"))
#cbs   <- read.csv("Data/CBS lifetable.csv", sep = ";")

# document which paramaters do not have a unit and will be deleted
param_NA <- param[which(is.na(param$Unit)), ]

# Remove parameters without unit
param <- param[!is.na(param$Unit), ]

## median survival times --> hazard to die 
param[param$Unit == "Median survival time (weeks)", c("Med", "Lo", "Hi")]  <- log(2)/param[param$Unit == "Median survival time (weeks)", c("Med", "Hi", "Lo")]

## 5-year survival rate --> hazard to die per week
param[param$Unit == "Probability 5-year survival", c("Med", "Lo", "Hi")]  <- RateToProb(ProbToRate(1 - param[param$Unit == "Probability 5-year survival", c("Med", "Hi", "Lo")]), 1/(5 * 52))

## Days --> weeks
param[param$Unit == "Days", c("Med", "Lo", "Hi")]  <- param[param$Unit == "Days", c("Med", "Lo", "Hi")]/7


# Remove the unit column, since these units are now all adjusted to weekly probabilities or age in years
param <-subset(param, select = -c(Unit)) # Store the weekly probabilities in the dataframe param


#CBS data: prob per year --> prob per week
cbs$Man_kans   <- RateToProb(ProbToRate(p = cbs$Man_kans), 1/52)
cbs$Vrouw_kans <- RateToProb(ProbToRate(p = cbs$Vrouw_kans), 1/52)

#CBS data: prob per sex --> average prob
cbs$prob <- (cbs$Man_kans + cbs$Vrouw_kans) / 2

#CBS data: Leeftijd --> age
cbs$age <- as.numeric(substr(cbs$Leeftijd, start = 1, stop = 2))

# Delay in weeks
delay <- seq(from = 2, to = 52, by = 10)

# Create a dataframe to store results in
n_iter <- 100 # Number of iterations in the PSA

# Create a data frame to store the results
df_res <- expand.grid(iter = 1:n_iter,                  # PSA iteration
                      Pop = unique(param$Population),   # Disease name 
                      delay = delay,                    # time of delay
                      QALY = NA,                        # QALYs
                      AAC = NA,                         # Area above the curve (the loss in QALYs)
                      AAC_delay = NA)                   # AAC/delay       

# Add the surgeries per population
df_res$Intervention <- param$Intervention[match(df_res$Pop, param$Population)]

pop_names <- sort(unique(param$Population))  # substract the disease names and order them in alphabetic order 

# Make a dataframe with PSA parameters 
param_psa <- make_psa_df(param = param, n_iter = n_iter, seed = 19)
save(param_psa,          file = "../output/input_psa_param.Rdata")
