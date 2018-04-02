# Remove all variables from Workspace
rm(list=ls())
set.seed(1234)

loss_sim<-5000
Var_sim<-10
percent_risky_loans<-.5

# install package manager if not installed
if (!require("pacman")){
  install.packages("pacman")
}

# Load package Manager
require(pacman)

# Install/load required packages
p_load(ggplot2)

data <- read.csv("./SBA_PD_LGD.csv")
data$estimated_chargoff_amount<-data$lgd_final*data$TotalLoan

###############  Create Portfolio  ###############
set.seed(1)
portfolio<-data[order(data$default_probabilities_5_years, decreasing = TRUE),c("lgd_final","TotalLoan","estimated_chargoff_amount","default_probabilities_1_year","default_probabilities_5_years")][1:floor(nrow(data)*percent_risky_loans),]
portfolio<-portfolio[sample(1:nrow(portfolio), 500, replace=FALSE),]
portfolio_loss<-data.frame(iteration=1:loss_sim, portfolio_dollar_loss_1=0, portfolio_dollar_loss_5=0)

###############  Simulate Losses  ###############
#Monte Carlo Simulation
set.seed(1)
for (i in 1:nrow(portfolio_loss)) {
  cur_portfolio<-portfolio
  
  cur_portfolio$r_prob<-runif(500, min = 0, max = 1)
  cur_portfolio$sim_default_1<-cur_portfolio$default_probabilities_1_year>=cur_portfolio$r_prob
  cur_portfolio$sim_default_5<-cur_portfolio$default_probabilities_5_years>=cur_portfolio$r_prob
  
  cur_portfolio$sim_loss_1<-cur_portfolio$estimated_chargoff_amount*cur_portfolio$sim_default_1
  cur_portfolio$sim_loss_5<-cur_portfolio$estimated_chargoff_amount*cur_portfolio$sim_default_5
  
  portfolio_loss$portfolio_dollar_loss_1[i]<-sum(cur_portfolio$sim_loss_1)
  portfolio_loss$portfolio_dollar_loss_5[i]<-sum(cur_portfolio$sim_loss_5)
}

portfolio_loss$portfolio_percent_loss_1<-portfolio_loss$portfolio_dollar_loss_1/sum(portfolio$TotalLoan)*100
portfolio_loss$portfolio_percent_loss_5<-portfolio_loss$portfolio_dollar_loss_5/sum(portfolio$TotalLoan)*100

# Plot losses
portfolio_dollar_loss_plot_data<-rbind(data.frame(dollar_loss=portfolio_loss$portfolio_dollar_loss_1, Years="1"),data.frame(dollar_loss=portfolio_loss$portfolio_dollar_loss_5, Years="5"))
portfolio_dollar_loss_plot<-ggplot(portfolio_dollar_loss_plot_data, aes(x = dollar_loss, fill = Years)) + geom_density(alpha = 0.5) + xlab("Portfolio $ Loss") + ylab("Probability")
portfolio_dollar_loss_plot

portfolio_percent_loss_plot_data<-rbind(data.frame(percent_loss=portfolio_loss$portfolio_percent_loss_1, Years="1"),data.frame(percent_loss=portfolio_loss$portfolio_percent_loss_5, Years="5"))
portfolio_percent_loss_plot<-ggplot(portfolio_percent_loss_plot_data, aes(x = percent_loss, fill = Years)) + geom_density(alpha = 0.5) + xlab("Portfolio % Loss") + ylab("Probability")
portfolio_percent_loss_plot

###############  Calculate Non-Parametric/Historic VAR  ###############

VAR<-data.frame(confidence_level=seq(from=0, to=1, by=.0001), VAR_1 = 0, VAR_5 = 0, AVAR_1 = 0, AVAR_5 = 0, S_1=0,S_5=0)

for(i in 1:nrow(VAR)){
  cur_confidence_level<-VAR$confidence_level[i]
  if(i%%100==0){
    cat(cur_confidence_level,"\n") 
  }
  VAR$VAR_1[i]<-quantile(portfolio_loss$portfolio_percent_loss_1,probs=cur_confidence_level)
  VAR$VAR_5[i]<-quantile(portfolio_loss$portfolio_percent_loss_5,probs=cur_confidence_level)
  VAR$AVAR_1[i]<-mean(portfolio_loss$portfolio_percent_loss_1[portfolio_loss$portfolio_percent_loss_1>=quantile(portfolio_loss$portfolio_percent_loss_1,probs=cur_confidence_level)])
  VAR$AVAR_5[i]<-mean(portfolio_loss$portfolio_percent_loss_5[portfolio_loss$portfolio_percent_loss_5>=quantile(portfolio_loss$portfolio_percent_loss_5,probs=cur_confidence_level)])
  VAR$S_1[i]<-length(portfolio_loss$portfolio_percent_loss_1[portfolio_loss$portfolio_percent_loss_1<=quantile(portfolio_loss$portfolio_percent_loss_1,probs=cur_confidence_level)])
  VAR$S_5[i]<-length(portfolio_loss$portfolio_percent_loss_5[portfolio_loss$portfolio_percent_loss_5<=quantile(portfolio_loss$portfolio_percent_loss_5,probs=cur_confidence_level)])
}

VAR$confidence_level<-1-VAR$confidence_level

VAR_plot<-ggplot(VAR, aes(x = confidence_level)) + 
  geom_line(aes(y = VAR_1), colour="blue")+
  geom_line(aes(y = VAR_5), colour="red")+
  ylab(label="Value at Risk") + 
  xlab("Confidence Level") +
  ggtitle("VAR, 1 and 5 years") + 
  coord_cartesian(xlim=c(0,.1))

VAR_plot

AVAR_plot<-ggplot(VAR, aes(x = confidence_level)) + 
  geom_line(aes(y = AVAR_1), colour="blue")+
  geom_line(aes(y = AVAR_5), colour="red")+
  ylab(label="Average Value at Risk") + 
  xlab("Confidence Level") +
  ggtitle("AVAR, 1 and 5 years") + 
  coord_cartesian(xlim=c(0,.1))

AVAR_plot

VAR_AVAR_plot_1<-ggplot(VAR, aes(x = confidence_level)) + 
  geom_line(aes(y = VAR_1, colour="Value at Risk"))+
  geom_line(aes(y = AVAR_1, colour="Expected Shortfall"))+
  ylab(label="Loss Percent") + 
  xlab("Confidence Level") +
  ggtitle("Var and AVAR, 1 year") + 
  coord_cartesian(xlim=c(0,.1)) + 
  scale_colour_manual("", breaks = c("Value at Risk","Expected Shortfall"),
                      values = c("Value at Risk"="red", "Expected Shortfall"="blue"))

VAR_AVAR_plot_1

VAR_AVAR_plot_5<-ggplot(VAR, aes(x = confidence_level)) + 
  geom_line(aes(y = VAR_5, colour="Value at Risk"))+
  geom_line(aes(y = AVAR_5, colour="Expected Shortfall"))+
  ylab(label="Loss Percent") + 
  xlab("Confidence Level") +
  ggtitle("Var and AVAR, 5 year") + 
  coord_cartesian(xlim=c(0,.1)) + 
  scale_colour_manual("", breaks = c("Value at Risk","Expected Shortfall"),
                      values = c("Value at Risk"="red", "Expected Shortfall"="blue"))

VAR_AVAR_plot_5

###############  Create Tranches  ###############
portfolio_loss_tranche<-portfolio_loss[c("iteration","portfolio_percent_loss_1","portfolio_percent_loss_5")]

portfolio_loss_tranche$Residual_loss_1<-0
portfolio_loss_tranche$Junior_tranche_loss_1<-0
portfolio_loss_tranche$Senior_tranche_loss_1<-0
portfolio_loss_tranche$Residual_loss_5<-0
portfolio_loss_tranche$Junior_tranche_loss_5<-0
portfolio_loss_tranche$Senior_tranche_loss_5<-0

for(i in 1:nrow(portfolio_loss_tranche)){
  if(portfolio_loss_tranche$portfolio_percent_loss_1[i]<5){
    portfolio_loss_tranche$Residual_loss_1[i]<-portfolio_loss_tranche$portfolio_percent_loss_1[i]
  }else if(portfolio_loss_tranche$portfolio_percent_loss_1[i]<15){
    portfolio_loss_tranche$Residual_loss_1[i]<-5
    portfolio_loss_tranche$Junior_tranche_loss_1[i]<-portfolio_loss_tranche$portfolio_percent_loss_1[i]-5
  }else if(portfolio_loss_tranche$cumulative_loss.1[i]>=15){
    portfolio_loss_tranche$Residual_loss_1[i]<-5
    portfolio_loss_tranche$Junior_tranche_loss_1[i]<-10
    portfolio_loss_tranche$Senior_tranche_loss_1[i]<-portfolio_loss_tranche$portfolio_percent_loss_1[i]-15
  }
  if(portfolio_loss_tranche$portfolio_percent_loss_5[i]<5){
    portfolio_loss_tranche$Residual_loss_5[i]<-portfolio_loss_tranche$portfolio_percent_loss_5[i]
  }else if(portfolio_loss_tranche$portfolio_percent_loss_5[i]<15){
    portfolio_loss_tranche$Residual_loss_5[i]<-5
    portfolio_loss_tranche$Junior_tranche_loss_5[i]<-portfolio_loss_tranche$portfolio_percent_loss_5[i]-5
  }else if(portfolio_loss_tranche$portfolio_percent_loss_5[i]>=15){
    portfolio_loss_tranche$Residual_loss_5[i]<-5
    portfolio_loss_tranche$Junior_tranche_loss_5[i]<-10
    portfolio_loss_tranche$Senior_tranche_loss_5[i]<-portfolio_loss_tranche$portfolio_percent_loss_5[i]-15
  }
}

portfolio_loss_tranche_1<-portfolio_loss_tranche[c("Residual_loss_1","Junior_tranche_loss_1","Senior_tranche_loss_1")]
portfolio_loss_tranche_1$Residual_loss_1<-portfolio_loss_tranche_1$Residual_loss_1/5*100
portfolio_loss_tranche_1$Junior_tranche_loss_1<-portfolio_loss_tranche_1$Junior_tranche_loss_1/10*100
portfolio_loss_tranche_1$Senior_tranche_loss_1<-portfolio_loss_tranche_1$Senior_tranche_loss_1/85*100

portfolio_loss_tranche_5<-portfolio_loss_tranche[c("Residual_loss_5","Junior_tranche_loss_5","Senior_tranche_loss_5")]
portfolio_loss_tranche_5$Residual_loss_5<-portfolio_loss_tranche_5$Residual_loss_5/5*100
portfolio_loss_tranche_5$Junior_tranche_loss_5<-portfolio_loss_tranche_5$Junior_tranche_loss_5/10*100
portfolio_loss_tranche_5$Senior_tranche_loss_5<-portfolio_loss_tranche_5$Senior_tranche_loss_5/85*100

tranches<-data.frame(tranche_loss=seq(from=0, to=100, by=.01), CLD_residual_1=0, CLD_junior_1=0, CLD_senior_1=0, CLD_residual_5=0, CLD_junior_5=0, CLD_senior_5=0)

for(i in 1:nrow(tranches)){
  cur_tranche_loss<-tranches$tranche_loss[i]
  tranches$CLD_residual_1[i] <- nrow(portfolio_loss_tranche_1[portfolio_loss_tranche_1$Residual_loss_1<=cur_tranche_loss,])/nrow(portfolio_loss_tranche_1)
  tranches$CLD_junior_1[i] <- nrow(portfolio_loss_tranche_1[portfolio_loss_tranche_1$Junior_tranche_loss_1<=cur_tranche_loss,])/nrow(portfolio_loss_tranche_1)
  tranches$CLD_senior_1[i] <- nrow(portfolio_loss_tranche_1[portfolio_loss_tranche_1$Senior_tranche_loss_1<=cur_tranche_loss,])/nrow(portfolio_loss_tranche_1)
  
  tranches$CLD_residual_5[i] <- nrow(portfolio_loss_tranche_5[portfolio_loss_tranche_5$Residual_loss_5<=cur_tranche_loss,])/nrow(portfolio_loss_tranche_5)
  tranches$CLD_junior_5[i] <- nrow(portfolio_loss_tranche_5[portfolio_loss_tranche_5$Junior_tranche_loss_5<=cur_tranche_loss,])/nrow(portfolio_loss_tranche_5)
  tranches$CLD_senior_5[i] <- nrow(portfolio_loss_tranche_5[portfolio_loss_tranche_5$Senior_tranche_loss_5<=cur_tranche_loss,])/nrow(portfolio_loss_tranche_5)
}


tranches$CLD_residual_1[1]<-0
tranches$CLD_junior_1[1]<-0
tranches$CLD_senior_1[1]<-0
tranches$CLD_residual_5[1]<-0
tranches$CLD_junior_5[1]<-0
tranches$CLD_senior_5[1]<-0

distribution_1 <-ggplot(tranches, aes(tranche_loss)) + 
  geom_line(aes(y = CLD_residual_1, colour = "Residual Loss")) + 
  geom_line(aes(y = CLD_junior_1, colour = "Junior Tranche")) + 
  geom_line(aes(y = CLD_senior_1, colour = "Senior Tranche")) + 
  xlab("Tranche Loss (%)") + 
  ylab("Cummulative Loss Distribution") +
  ggtitle("Loss Distribution for Investors, 1 year")

distribution_1$labels$colour<-"Tranches"
distribution_1

distribution_5 <-ggplot(tranches, aes(tranche_loss)) + 
  geom_line(aes(y = CLD_residual_5, colour = "Residual Loss")) + 
  geom_line(aes(y = CLD_junior_5, colour = "Junior Tranche")) + 
  geom_line(aes(y = CLD_senior_5, colour = "Senior Tranche")) + 
  xlab("Tranche Loss (%)") + 
  ylab("Cummulative Loss Distribution") +
  ggtitle("Loss Distribution for Investors, 5 year")

distribution_5$labels$colour<-"Tranches"
distribution_5

ggsave("../Figures/VAR_AVAR_plot_1_.05.png", VAR_AVAR_plot_1)
ggsave("../Figures/VAR_AVAR_plot_5_.05.png", VAR_AVAR_plot_5)

ggsave("../Figures/portfolio_dollar_loss_plot_.05.png", portfolio_dollar_loss_plot)
ggsave("../Figures/portfolio_percent_loss_plot_.05.png", portfolio_percent_loss_plot)

ggsave("../Figures/distribution_1_.05.png", distribution_1)
ggsave("../Figures/distribution_5_.05.png", distribution_5)
