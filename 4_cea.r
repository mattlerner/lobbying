library(scales)
library(tidyverse)
library(gt)

options(scipen=999)

########## Cost effectiveness analysis ##########

# Functions

##### Back out parameters for normal distribution
norm.backout <- function(bottom.end, top.end) {
  sd = (top.end - bottom.end)/4
  mu = top.end - (2*sd)
  return(list(mu = mu, sd = sd))
}

##### Back out parameters for log-normal distribution
rlnorm.backout <- function(bottom.end, top.end) {
  mu <- (log(bottom.end) + log(top.end))/2
  sd <- (log(top.end) - mu)/(sqrt(2)*1.39)
  return(list(mu = mu, sd = sd))
}

#### Cost?
full.ce <- function(cost.low, cost.high, prob.low, prob.high, impact.low, impact.high, error.rate, quantiles.only=FALSE) {
  n <- 1000000
  cost.backedout <- rlnorm.backout(cost.low, cost.high)
  cost <- rlnorm(n, cost.backedout$mu, cost.backedout$sd)
  
  ### Delta in probability of policy implementation
  counterfactual.backedout <- norm.backout(prob.low,prob.high)
  counterfactual.prob <- rnorm(n, counterfactual.backedout$mu, counterfactual.backedout$sd)
  quantile(counterfactual.prob, c(0.025, 0.5, 0.975))
  
  ### Impact
  impact.backedout <- rlnorm.backout(impact.low, impact.high)
  error <- (rbinom(n, 1, error.rate)*-2)+1
  impact <- rlnorm(n, impact.backedout$mu, impact.backedout$sd) * error
  quantile(impact, c(0.025, 0.5, 0.975))
  
  ### CE Calculation
  expected.return <- (counterfactual.prob * impact)
  cost.effectiveness <- cost / expected.return
  ce.quantiles <- quantile(cost.effectiveness, c(0.025, 0.25, 0.15, 0.5, 0.75, 0.85, 0.975))
  #plot(density(log(cost.effectiveness), na.rm=TRUE))
  if (quantiles.only == TRUE) {
    return(ce.quantiles)
  } else {
    return(cost.effectiveness)
  }
}

test <- full.ce(20000, 10000000, 0, 0.05, 1, 100000000, 0.1)
quantile(test,c(0.025,0.25,0.5,0.75,0.975))

#### Plot sensitivity as facet grid?
high.end.probability <- c(0.001, 0.01, 0.1)
high.end.impact <- c(100000,100000000,100000000000)
prob.negative <- c(0.001,0.01,0.1)

varying.data.only <- function(highprob, highimpact, probneg) {
  data <- full.ce(20000, 10000000, 0, highprob, 1, highimpact, probneg)
  return(data)
}

expanded <- expand.grid(prob = high.end.probability, impact = high.end.impact, probneg=prob.negative)
frame_ <- data.frame()
for (i in 1:nrow(expanded)) {
  dens <- varying.data.only(expanded[i,'prob'],expanded[i,'impact'],expanded[i,'probneg'])
  dens.frame <- data.frame(prob=expanded[i,'prob'], impact=expanded[i,'impact'], probneg=expanded[i,'probneg'], dens)
  frame_ <- rbind(frame_, dens.frame)
}

# just add fuckin labels and decrease the font size!

# summarized table
description <- frame_ %>% group_by(prob, impact, probneg) %>% summarise(median = round(median(dens, na.rm=TRUE)))

######### Output sensitivity analysis plot
options(scipen=0)
plot.boundaries <- c(-10000000000,-100000000,-1000000,-10000,-100,0,100,10000,1000000,100000000,10000000000)
limits <- c(min(plot.boundaries),max(plot.boundaries))

frame_$probneg.formatted <- paste0("Probability of wrong sign of impact: ", as.character(frame_$probneg))
frame_$impact.formatted <- paste0("DALYS preserved (97.5th %ile):\n", as.character(frame_$impact))
ggplot(frame_) + geom_density(aes(x=dens, color=as.factor(prob)), key_glyph=draw_key_abline) + scale_x_continuous(trans="pseudo_log", name="Dollars", breaks = plot.boundaries, limits=limits)  + theme(text=element_text(size=12,  family="Helvetica"), axis.text.x = element_text(size=8)) + ggtitle("Cost-effectiveness of lobbying (estimated)") + ylab("Density") + facet_grid(impact.formatted ~ probneg.formatted, scales = "free_x") + guides(color=guide_legend(title="Increased probability\nof enactment\n(97.5th %tile)")) + theme(panel.background = element_blank(), panel.grid = element_line(colour="#eeeeee"))

######### Sensitivity analysis - single plot

# (cost.low, cost.high, prob.low, prob.high, impact.low, impact.high, error.rate

v.1 <- full.ce(20000, 10000000, 0, 0.05, 1, 100000000000, 0.1)
original <- full.ce(20000, 1000000, 0, 0.05, 1, 100000000, 0.1)
v.2 <- full.ce(20000, 10000000, 0, 0.05, 1, 100000000, 0.05)
v.3 <- full.ce(20000, 10000000, 0, 0.05, 1, 100000000, 0.1)
v.4 <- full.ce(20000, 10000000, 0, 0.01, 1, 100000000, 0.1)

color.assignment <- c("Original" = "#D53E4F", "Variation 1" = "#3288BD", "Variation 2" = "#66C2A5", "Variation 3" = "#FEE08B", "Variation 4"="#E6F598")
  
plot.boundaries <- c(-100000000,-1000000,-10000,-100,0,100,10000,1000000,100000000)
limits <- c(min(plot.boundaries),max(plot.boundaries))
plot <- ggplot()
plot <- plot + geom_density(aes(x=original, color="Original"), size = 1, key_glyph=draw_key_abline) + scale_x_continuous(trans="pseudo_log", name="Dollars", labels=comma, breaks = plot.boundaries, limits=limits) + theme_minimal() + ggtitle("Cost-effectiveness of lobbying (estimated)") + ylab("Density")
plot <- plot + geom_density(aes(x=v.1, color="Variation 1"), size = 1, key_glyph=draw_key_abline) + scale_x_continuous(trans="pseudo_log", name="Dollars", labels=comma, breaks = plot.boundaries, limits=limits) + theme_minimal() + ggtitle("Cost-effectiveness of lobbying (estimated)") + ylab("Density")
plot <- plot + geom_density(aes(x=v.2, color="Variation 2"), size = 1, key_glyph=draw_key_abline) + scale_x_continuous(trans="pseudo_log", name="Dollars", labels=comma, breaks = plot.boundaries, limits=limits) + theme_minimal() + ggtitle("Cost-effectiveness of lobbying (estimated)") + ylab("Density")
plot <- plot + geom_density(aes(x=v.3, color="Variation 3"), size = 1, key_glyph=draw_key_abline) + scale_x_continuous(trans="pseudo_log", name="Dollars", labels=comma, breaks = plot.boundaries, limits=limits) + theme_minimal() + ggtitle("Cost-effectiveness of lobbying (estimated)") + ylab("Density")
plot <- plot + geom_density(aes(x=v.4, color="Variation 4"), size = 1, key_glyph=draw_key_abline) + scale_x_continuous(trans="pseudo_log", name="Dollars", labels=comma, breaks = plot.boundaries, limits=limits) + theme_minimal() + ggtitle("Cost-effectiveness of lobbying (estimated)") + ylab("Density")
plot <- plot + scale_colour_manual(name="",values=color.assignment) + theme(legend.spacing.y = unit(0.5, 'cm')) + theme(plot.margin=margins, text=element_text(size=12,  family="Helvetica"), axis.text.x = element_text(size=8))

ggsave("sensitivity.analysis.png", plot)