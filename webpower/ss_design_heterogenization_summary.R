library(tidyverse)
library(WebPower)
library(gridExtra)


cols <- RColorBrewer::brewer.pal(6, "Dark2")
theme_set(theme_classic(base_size = 12))
letter <- theme(plot.title = element_text(face="bold"),
                plot.subtitle = element_text(face="bold"),
                axis.title = element_text(face = "bold"))

###########################################
#
# design with 2-5 factors with two levels
# [2 x 2; 2 x 2 x 2; 2 x 2 x 2 x 2; 2 x 2 x 2 x 2 x 2]
#
###########################################


# First we set the values for all given parameters. 
# factors:  number of factors in the factorial design 
# levels:   number of levels per factor (here always = 2)
# f:        effect size f 
# The effect size f is represents the ratio between the average variance of the 
# effect of interest (e.g. main effect of factor 1) and the average variance of 
# all groups. 
# Reference: Zhang, Z., & Yuan, K.-H. (2018). Practical Statistical Power Analysis Using Webpower and R (Eds). Granger, IN: ISDSA Press.

factors <- as.numeric(c(1,2,3,4,5))     
levels <- as.numeric(c(2))
f <- as.numeric(c(0.1, 0.25, 0.45))

# expand.grid() produces all possible combinations of factors, levels and effect size f. 
report <- expand.grid(factors = factors, levels = levels, f = f)


# We add several other variables: 
# ng:       total number of groups within the experiment given a specific number of factors and levels 
# ndf_main: degrees of freedom, calculated for the main effect according to Zhang et al 2018 
# According to standard practice, the type-I error rate alpha is set to 0.05, 
# power to detect the main effect f set to 80%. 

report<- report %>% 
  mutate (ng = factors * levels, 
          ndf_main = (2-1), 
          power = 0.8, 
          alpha = 0.05, 
          n_total = FALSE, 
          design = rep(c("2", "2 x 2", "2 x 2 x 2", "2 x 2 x 2 x 2", "2 x 2 x 2 x 2 x 2"), 3),
          cols=factor(report$f, labels = c(cols[1],cols[4], cols[6]))) 


# The neccessary total number of animals in the experiment is than calculated 
# with the WebPower::wp.kanova()

for (i in 1:nrow(report)) {
  result <-  wp.kanova(
    ndf = report$ndf_main[i], 
    f = report$f[i], 
    ng = report$ng[i], 
    alpha = report$alpha[i], 
    power = report$power[i])
  report$n_total[i] <- result$n

}


report <- report%>% 
  mutate(group_ss = n_total / ng)


#visualization 

# Here, the per group sample size under different design choices is visualized
per_group<-ggplot(report)+ 
  geom_point(aes(x = design, y = group_ss, color = as.factor(f)), 
             size = 4)+
  labs( title = "Group sample sizes in factorial designs",
        subtitle = "1-5 factors with 2 levels, powered for main effect",
        x = "factorial design with factors x levels",
        y = "sample size per group")+
  scale_color_brewer( palette = "Dark2", 
                      name = "effect size F")+
  scale_y_continuous(breaks = c(0,10,25,50, 100, 150, 200))+
  letter 

per_group 


# Here, the total sample size under different design choices is visualized


total_ss<-ggplot(report)+ 
  geom_point(aes(x = design, y = n_total, color = as.factor(f)), 
             size = 4)+
  labs( title = "Total sample sizes in factorial designs",
        subtitle = "2-5 factors with 2 levels, powered for main effect",
        x = "factorial design",
        y = "total sample size")+
  scale_color_brewer( palette = "Dark2", 
                      name = "effect size F")+
  scale_y_continuous(breaks = c(0,50, 100, 200, round(max(report$n_total))))+
  letter 

total_ss


###########################################
#
# design with multiple dosage groups & 
# two additional factors
#
###########################################

# Here we redo the analysis with a difference in the number of levels for the 
# first factor. Conceptually, we were thinking about a design, in which the main
# treatment (main effect of interest) is given in different dosages, or where a 
# positive and negative control group were used as comparison groups to the treatment group. 
# factor1:  number of levels in factor1 (e.g. dosage groups)
# factor2:  number of levels in factor2 (e.g. sex: male/female)
# factor3:  number of levels in factor3 (e.g. age: 8 weeks/12 weeks)

factor1 <- as.numeric(c(2,3,4))
factor2 <- as.numeric(2) 
factor3 <- as.numeric(2)
f <- as.numeric(c(0.1, 0.25, 0.45))

report2 <- expand.grid(factor1 = factor1, 
                      factor2 = factor2, 
                      factor3 = factor3, 
                      f = f)

report2<- report2 %>% 
  mutate (ng = factor1 * factor2 * factor3, 
          ndf_main = (factor1-1), 
          ndf_interaction = (factor1-1) * (factor2-1) * (factor3-1), 
          power = 0.8,  
          alpha = 0.05, 
          n_total = FALSE, 
          design = rep(c("2 x 2 x 2", "3 x 2 x 2", "4 x 2 x 2"), 3),
          cols=factor(report2$f, labels = c(cols[1],cols[4], cols[6]))) 

for (i in 1:nrow(report2)) {
  result <-  wp.kanova(
    ndf = report2$ndf_main[i], 
    f = report2$f[i], 
    ng = report2$ng[i], 
    alpha = report2$alpha[i], 
    power = report2$power[i])
  report2$n_total[i] <- result$n
}

report2 <- report2%>% 
  mutate(group_ss = n_total / ng)


#visualization 
# Here, the per group sample size under different design choices is visualized

per_group_multiple_doses<-ggplot(report2)+ 
  geom_point(aes(x = design, y = group_ss, color = as.factor(f)), 
             size = 4)+
  labs( title = "Group sample sizes in factorial designs",
        subtitle = "multiple treatment groups, powered for main effect",
        x = "factorial design",
        y = "sample size per group")+
  scale_color_brewer( palette = "Dark2", 
                      name = "effect size F")+
  scale_y_continuous(breaks = c(0,10,25,50, 100, 150, 200))+
  letter 

per_group_multiple_doses


# Here, the total sample size under different design choices is visualized

total_ss_multiple_doses<-ggplot(report2)+ 
  geom_point(aes(x = design, y = n_total, color = as.factor(f)), 
             size = 4)+
  labs( title = "Total sample sizes in factorial designs",
        subtitle = "multiple treatment groups, powered for main effect",
        x = "factorial design",
        y = "total sample size")+
  scale_color_brewer( palette = "Dark2", 
                      name = "effect size F")+
  scale_y_continuous(breaks = c(0,50, 100, 500, 200, round(max(report2$n_total))))+
  letter 

total_ss_multiple_doses



fig1 <- grid.arrange(
  total_ss,
  total_ss_multiple_doses
)

fig1
