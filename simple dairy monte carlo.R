#simple dairy model

library(tidyverse)

samples <- 100

ms_per_cow <- rnorm(samples,370,60) #20

past_grown_per_ha <- rnorm(samples, 15,2) #2

base_utilisation <- rnorm(samples, 0.85, 0.0) #0.1 
#mean 0.85 (@300 MS/cow) is assumed up from 0.8 to take into account increased genetic drive for intake since WFE trial

MS_adj_utilisation <- base_utilisation - 0.001 * (ms_per_cow-300) #see "WFE whole farm efficiency utilisation and production per cow.xlsx"

past_harvest_per_ha <- past_grown_per_ha * MS_adj_utilisation #last factor is lower util for higher yield

SR <- rnorm(samples, past_grown_per_ha/15*2.8, 0)  #0.2

intake_per_ms <- 1/120 #1/assumed response kg MS per t DM consumed

base_intake <- 1.67 #0 if above is 80; 1, if 100; 1.67 if 120 

supp_utilisation <- rnorm(samples, 0.85, 0)

supp_per_ha <- ( (SR * (ms_per_cow * intake_per_ms + base_intake)) - past_harvest_per_ha )/ supp_utilisation

cost_per_cow <- rnorm(samples, 600,0)

supp_cost_per_t <- rnorm(samples, 300,0)

associated_cost_factor <- rnorm(samples, 1, 0) #0.2
  
costs_per_ha <- rnorm(samples, 3000,0) + (SR * cost_per_cow) + associated_cost_factor* (supp_per_ha * supp_cost_per_t)

payout <- rnorm(samples, 6.50, 0) #1

livestock_income_per_cow <- rnorm(samples, 300,0)

rev_per_ha <- SR * (payout * ms_per_cow + livestock_income_per_cow)

profit_per_ha <- rev_per_ha - costs_per_ha

stock_value_per_cow <- 1800

machinery_value_per_ha <- 3000

assets_per_ha <- rnorm(samples, 50000, 0) + SR * stock_value_per_cow + machinery_value_per_ha

operating_ROA <- profit_per_ha / assets_per_ha


output <- tibble(ms_per_cow, past_grown_per_ha, base_utilisation, MS_adj_utilisation, past_harvest_per_ha, SR, intake_per_ms, base_intake,
                        supp_utilisation, supp_per_ha, cost_per_cow, supp_cost_per_t, associated_cost_factor,
                        costs_per_ha, payout, livestock_income_per_cow, rev_per_ha, profit_per_ha,
                        stock_value_per_cow, machinery_value_per_ha, assets_per_ha, operating_ROA)

means <- output %>% summarise(across(everything(), mean))

ggplot()+
 geom_boxplot(data = output, aes(y=operating_ROA))

f_xvar <- "past_harvest_per_ha"
f_yvar <- "SR"

my_plot_function <- function(f_xvar, f_yvar){
  ggplot(data=output, aes(x= !!sym(f_xvar), y= !!sym(f_yvar)))+
      geom_point()+
      geom_smooth()+
      geom_text(x = output %>% select(sym(f_xvar)) %>% pull() %>% quantile(0.5), 
                y = output %>% select(sym(f_yvar)) %>% pull() %>% quantile(0.95),
                #label = "text")
                label = paste0("Correlation ", round( cor(output %>% select(sym(f_xvar)), 
                                                          output %>% select(sym(f_yvar))), 2)))
}

xvar <- "past_harvest_per_ha"
yvar <- "SR"

#!!sym(xvar)

my_plot_function(xvar, yvar)

ggplot(data=output, aes(x=SR, y=supp_per_ha))+
  geom_point()+
  geom_smooth()

ggplot(data=output, aes(x=ms_per_cow, y=MS_adj_utilisation))+
  geom_point()+
  geom_smooth()

ggplot(data=output, aes(x=ms_per_cow, y=profit_per_ha))+
  geom_point()+
  geom_smooth()
cor(output$ms_per_cow,output$profit_per_ha)

ggplot(data=output, aes(x=ms_per_cow, y=operating_ROA))+
  geom_point()+
  geom_smooth()

ggplot(data=output, aes(x=past_harvest_per_ha, y=profit_per_ha))+
  geom_point()+
  geom_smooth()

