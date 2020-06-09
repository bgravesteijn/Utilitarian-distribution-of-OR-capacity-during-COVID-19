

# loop over every disease 
for (d in pop_names){
  for(it in 1:n_iter){   # for every PSA iteration
    
    # PSA estimates
    # Extract vector of parameters
    p_vector        <- param_psa$psa_est[param_psa$iter == it & param_psa$Population == d]
    names(p_vector) <- param_psa$Param  [param_psa$iter == it & param_psa$Population == d]
    
    ##BUILD IN ERROR MESSAGE 
    if(length(p_vector) > 9){
      print(paste("There are populations with the same name (",d,")"))
      break
    }
    
    # Round age to the nearest number
    p_vector["Age"] <- round(p_vector["Age"], 0)
    
    # Time horizon (in weeks)
    n_years     <- 100 - p_vector["Age"]  # time horizon in years
    n_cycles    <- 52 * n_years           # number of model cycles
    
    
    # create the transition matrix
    m_trans <- make_m_trans(number_states = n_s,
                            state_names = state_names,
                            number_cycles = n_cycles, 
                            parameters = p_vector,
                            cbsdata = cbs)
    
    ##BUILD IN ERROR MESSAGE 
    # @BG I now have in the make_m_trans function already a arror message, why can;t it be lareger thant 0.9?
    if(TRUE %in% (c(m_trans["Preop", "Dead", ], m_trans["Postop","Dead", ]) > 0.9)){
      print(paste("There transition probabilities to the Dead state higher than 0.9"))
      break
    }
    
    
    
    # rewards vectors (useful for state rewards )
    v_utility <- numeric(n_s)
    v_utility <- c(p_vector["QoL_no_tx"], p_vector["QoL_tx"], 0)
    names(v_utility) <- state_names
    
    # The utility vector, where there is no gain in QoL of the surgery
    v_utility_noeff           <- v_utility
    v_utility_noeff["Postop"] <- v_utility_noeff["Preop"]
    
    
    # Start the senario analysis for different delays. 
    for(i in 1:length(delay)){
      
      ## Include delay in transition matrix
      m_trans <- trans_operation(trans_matrix   = m_trans,
                                 weeks_until_op = delay[i],
                                 number_cycles   = n_cycles)
      
      
      # create the Markov trace for the individuals 
      m_trace <- matrix(NA, nrow = n_cycles + 1, ncol = n_s,
                        dimnames = list(paste("cycle", 0:n_cycles, sep = " "), state_names)) 
      
      # initialize the start health state
      m_trace[1, ] <- c(1, 0, 0)  # all individuals start out in the Preop health states
      
      
      ## Calculate Markov trace for this senario of delay
      for (t in 1:n_cycles){     # loop through the number of cycles
        m_trace[t + 1, ] <- t(m_trace[t, ]) %*% m_trans[, , t]    # estimate the Markov trace 
        # for the next cycle (t + 1)
      } # close the loop for the markov modl 
      
      # Calculate the QALYs
      # Vector with the total effects (QALYs per cycle)
      ## @Eline: kan jij misschien deze ifelse statement dubbel checken?
      # @Benjamin: ik denk dat dit doet wat het moet doen. En ik heb delen door n_years naar de regel eronder gehaald bij het berekenen van de QALY
      
      
      
      
      if("Time_noeff_QoL" %in% names(p_vector) &   # If the effect on QoL is lost after a particular time
         delay[i] > p_vector["Time_noeff_QoL"]){ # And if the delay is larger than the time untill the effect is lost
        v_tu  <- (m_trace   %*%  v_utility_noeff)  # use the no-effect vector
      }else{
        v_tu  <- (m_trace   %*%  v_utility)        # use the utility vector wíth effect
      }
      
      # Make discount vector 
      d_e_effect <- 0.015 # discount rate of 0.015 
      times <- seq(0, n_cycles/52, by = 1/52)  # factor to adjust discount weights for cycle length. Every cycle is 3 months
      v_dwe      <- 1 / ((1 + d_e_effect) ^ (times)) 
      
      # Calculate the total QALYs
      QALY <- (t(v_tu) %*%  v_dwe) / n_years
      
      # Store discounted total QALY result 
      df_res[df_res$iter == it & df_res$Pop == d & df_res$delay == delay[i], "QALY"] <- QALY[1]
      
      # Calculate the area above the curve 
      area.total <- df_res[df_res$Pop == d & df_res$iter == it, ]$QALY[1] * df_res[df_res$Pop == d & df_res$iter == it, ]$delay[i]
      auc <- DescTools::AUC(x    = df_res[df_res$Pop == d & df_res$iter==it, ]$delay, 
                            y    = df_res[df_res$Pop == d & df_res$iter==it, ]$QALY, 
                            from = df_res[df_res$Pop == d & df_res$iter==it, ]$delay[1], 
                            to   = df_res[df_res$Pop == d & df_res$iter==it, ]$delay[i], 
                            method = "spline") # Calculate the area under the curve 
      aac <- area.total - auc # area above the curve
      
      # Store the results
      df_res[df_res$iter==it & df_res$Pop == d & df_res$delay == delay[i], "AAC"] <- aac
      df_res[df_res$iter==it & df_res$Pop == d & df_res$delay == delay[i], "AAC_delay"] <- aac/delay[i]
    }
    
    # Display simulation progress
    if(it/(n_iter/10) == round(it/(n_iter/10), 0)) { # display progress every 10%
      cat('\r', paste(it/n_iter * 100, "% PSA of disease", d, "done", sep = " "))
      }
  } # close the number if PSA iterations
} # close the loop for diseases



df_res$AAC_delay[is.nan(df_res$AAC_delay)] <- 0 #Divided by 0 is not relevant --> put to 0




## Pool results PSA
#df_res <- data.table(df_res)
#results_pooled <- df_res[,.(Intervention  = unique(Intervention),
#                            QALY_med      = median(QALY),
#                            QALY_lo       = quantile(QALY, probs = 0.025),
#                            QALY_hi       = quantile(QALY, probs = 0.975),
#                            AAC_med       = median(AAC),
#                            AAC_lo        = quantile(AAC, probs = 0.025),
#                            AAC_hi        = quantile(AAC, probs = 0.975),
#                            AAC_delay_med = median(AAC_delay),
#                            AAC_delay_lo  = quantile(AAC_delay, probs = 0.025),
#                            AAC_delay_hi  = quantile(AAC_delay, probs = 0.975)),
#                         by = .(Pop, delay)]
#
## Order the data in alphabetic order 
#results_pooled <- results_pooled[order(results_pooled$Pop), ]
#
## save files in the output folder 
#save(df_res,         file = "../output/res_psa.Rdata")
#save(results_pooled, file = "../output/psa_pooled.Rdata")
#save(param,          file = "../output/input_param.Rdata")
#
## plot results of all diseases seperately
#plotPopulationOutcomes(data = results_pooled)
#
## Calculate the QALY loss per week (the derivative)
##res_derivative <- calculateDerivative(df_pooled = results_pooled, plot = TRUE, plot_ind = FALSE, #cum = FALSE, folder = "../figures")
## Make a figure of that derivative
##ggsave(filename = "../figures/derivatives.jpg", plot = res_derivative[[1]], device = "jpg")


