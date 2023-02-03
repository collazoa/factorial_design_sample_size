# Sample size for 2 x 2 factorial design with different interaction shapes
# reversed interaction 
# partially attenuated interaction 
# fully attentuated interaction 

# function to calculate total sample size for interaction effect 
# d1: Cohen's d for treatment effect within subgroup1 
# d2: Cohen's d for treatment effect within subgroup2 

library(tidyverse)
library(dplyr)

#styling
cols <- RColorBrewer::brewer.pal(6, "Dark2")
theme_set(theme_classic(base_size = 12))
letter <- theme(plot.title = element_text(face="bold"),
                plot.subtitle = element_text(face="bold"),
                axis.title = element_text(face = "bold"))


# Given is 
# J = {0,1} (e.g. treatment, intervention)
# K = {0,1} (e.g. male, female)
# in a 2 x 2 design 
# The interaction effect J x K 
# expressed the difference between treatment effect within 
# each level K 
# (mean[1,0] - mean[0,0]) - (mean[1,1] - mean[0,1]) / 2 * pooled standard deviation 
# to reduce complexity we assume pooledSD = 1



# function to calculate the interaction effect size 
# ref: Sommet et al. 2022

calculate_es_interaction <- function(d1, d2, power = 0.8, sdpooled = 1, alpha = 0.05) {
  es_interaction <- abs((d1 - d2)/(2*sdpooled))
}

# function to calculate the total sample size to power for the interaction effect size 
# ref: Sommet et al. 2022 

calculate_ntotal <- function(d1, d2, power = 0.8, sdpooled = 1, alpha = 0.05) {
  numerator <- (4 * (qnorm(1 - alpha/2)+qnorm(power))^2)
  
  es_interaction <- abs((d1 - d2)/(2*sdpooled))
  
  ntotal <- numerator/(es_interaction^2)
}



# We set values for Cohen's d of the treatment effect for values of factor K equaling 
# K = 1 (d1) and K = 2 (d2)

d1 <- seq(from = -1, to = 1, by = 0.1)
d2 <- seq(from = -1, to = 1, by = 0.1)


# here we combine different values of treatment effect in K = 1 and K = 2 
df <- expand.grid(d1 = d1, d2 = d2)

# We are deleting rows in which the treatment effect of K = 1 equals the treatment effect 
# in K = 2. Under this scenario, no interaction is observable. 
df <- df%>%
        mutate(ntotal = NA, 
               delete = c(df$d1 == df$d2))%>%
        filter(delete == FALSE)

df <- df%>%dplyr::select(d1, d2, ntotal)


# We are adding the values of the interaction effect size and the total sample size 
# calculated with the functions ´calculate_es_interaction´ and ´calculate_es_interaction´
# for now with power = 0.8 . 

for (i in 1:nrow(df)) {
  df$ntotal[i]<-calculate_ntotal(d1 = df$d1[i], df$d2[i])
  df$round_ntotal[i]<-ceiling(df$ntotal[i])
  df$es_interaction[i] <- calculate_es_interaction(d1 = df$d1[i], d2 = df$d2[i])
}


df$log_ntotal <- log10(df$ntotal)


ggplot(data = df) + 
  geom_point(mapping = aes(x = es_interaction, y = round_ntotal), color = cols[1])+
  scale_y_log10()+
  labs(title = "Total sample size to power for interaction effects")+
  xlab("interaction effect size, J x K")+
  ylab("total sample size")+
  letter


logvalues <- c(1.5, 2, 2.5, 3, 3.5, 4)
trans <- ceiling(10^logvalues)

fig2<- ggplot(data = df) + 
        geom_tile(mapping = aes(x = d1, y = d2, fill = log_ntotal))+
        geom_hline(yintercept = 0, linetype = "dashed")+
        geom_vline(xintercept = 0, linetype = "dashed")+
        scale_fill_gradient(low="white", high="blue", name = "log10(n_total)")+ 
        labs(title = "Total sample size depending on treatment effect within subgroups")+
        xlab(expression("Cohen's d "["main"]^"k= male"))+ 
        ylab(expression("Cohen's d "["main"]^"k = female"))+
        theme(axis.line = element_blank(),
              plot.title = element_text(face="bold"),
              plot.subtitle = element_text(face="bold"),
              axis.title = element_text(face = "bold"))
fig2

ggsave("fig2.png", plot = fig2)
