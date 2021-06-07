v_costs   <- c(50,   70, 100,  200,   280) 
v_effects <- c(0.01, 0.03, 0.04, 0.10,  0.05)
v_strategy <- c("surgery A", "surgery B", "surgery C", "surgery D", "surgery E")

df_results <- data.frame(strategy = v_strategy, 
               costs = v_costs,
              effect = v_effects)


ICER <- dampack::calculate_icers(cost =  v_costs,
                         effect = v_effects,
                         strategies = v_strategy)


ICER

plot(ICER, label = "all") +
  ylab("Costs (duration surgery)") +
  xlab("Effect (DALY/month")
