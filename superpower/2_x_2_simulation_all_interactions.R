
# styling 
cols <- RColorBrewer::brewer.pal(6, "Dark2")
theme_set(theme_classic(base_size = 12))




set.seed(100)

B = 1000 

df <- tibble()

create_data <- function(mean_a1_b1, 
                        mean_a1_b2, 
                        min,
                        max,
                        sd = 0.2, 
                        B = 1000, 
                        n = 10) {
index <- 1:length(B)
a1_b1 <- rnorm(B, mean = mean_a1_b1, sd = sd)
a1_b2 <- rnorm(B, mean = mean_a1_b2, sd = sd) 
a2_b1 <- runif(B, min = min, max = max)
a2_b2 <- runif(B, min = min, max = max)
n <- c(rep(n,B))
df <- tibble(index, a1_b1, a1_b2, a2_b1, a2_b2, n, sd)
}

df <- create_data(1,1, 0.2,2)%>%
        mutate(f_a = FALSE, 
               f_b = FALSE, 
               f_ab = FALSE, 
               pwr_a = FALSE, 
               pwr_b = FALSE, 
               pwr_ab = FALSE)


for (i in 1:nrow(df)) {
        design<- ANOVA_design(
        design = "2b*2b", n = df$n[i],
        mu = as.numeric(df[i, 2:5]), sd = df$sd[i],
        labelnames = c("a",
                 "control", "treatment",
                 "b",
                 "male", "female"), 
        plot = FALSE)

        result_exact <- ANOVA_exact(design)

        df$f_a[i] <- result_exact$main_results$cohen_f[1]
        df$f_b[i] <- result_exact$main_results$cohen_f[2]
        df$f_ab[i] <- result_exact$main_results$cohen_f[3]

        df$pwr_a[i] <- result_exact$main_results$power[1]
        df$pwr_b[i] <- result_exact$main_results$power[2]
        df$pwr_ab[i] <- result_exact$main_results$power[3]
      }


df <- df %>%
        mutate(
            effect_b1 = a2_b1 - a1_b1, 
            effect_b2 = a2_b2 - a1_b2
        )

df <- df %>%
        mutate(
            type_int = ifelse(effect_b1 == effect_b2, 
                              "no_interaction", 
                              ifelse(effect_b1 > 0 & effect_b2 > 0 |
                                     effect_b1 < 0 & effect_b2 < 0, 
                                     "attenuated interaction", 
                                     ifelse(effect_b1 > 0 & effect_b2 < 0 |
                                            effect_b1 < 0 & effect_b2 > 0, 
                                      "cross-over interaction", FALSE)))) 

#data exploration 

ggplot(df) + geom_point(aes(x = f_b, y = f_a, color = type_int))

lm(df$f_a ~ df$f_ab)
df_cross_over <- df%>% filter(type_int == "cross-over interaction") 
lm(f_a ~ f_ab, data = df_cross_over)
df_attenuated <- df %>% filter(type_int == "attenuated interaction") 
lm(f_a ~ f_ab, data = df_attenuated)


lm(f_a ~ f_ab, data = df)
lm(f_a ~ f_ab + type_int, data = df)
lm(f_a ~ type_int, data = df)

lm(f_b ~ type_int, data = df)
lm(f_a ~ type_int, data = df)
lm(f_a ~ f_b + type_int, data = df)
lm(f_a ~ f_ab, data = df)
lm(f_b ~ f_ab, data = df)
m1 <- glm(f_a ~ f_b, data = df)
m2 <- glm(f_a ~ f_b + f_ab, data = df)
m3 <- glm(f_a ~ f_b + f_ab + type_int, data = df)

anova(m1,m2)
anova(m2,m3)
AIC(m1,m2,m3)

# visualization 

figa <- ggplot(df) + geom_violin(aes(x = as.factor(type_int), 
                                      y = f_a, 
                                      color = factor(type_int)))+ 
  labs(title = "C: interaction shapes and main effect of factor A", 
       y = "effect size of factor A", 
       x = "")+
  scale_color_manual(values = c(cols[1], cols[4]), 
                     labels = c("attenuated interaction", 
                                "cross-over interaction"))+
  theme(legend.position = "none", 
        plot.title = element_text(face="bold"))

figa


figb <- ggplot(df) + geom_violin(aes(x = as.factor(type_int), 
                                      y = f_b, 
                                      color = factor(type_int)))+ 
  labs(title = "B: interaction shapes and main effect of factor B", 
       y = "effect size of factor B", 
       x = "")+
  scale_color_manual(values = c(cols[1], cols[4]), 
                     labels = c("attenuated interaction", 
                                "cross-over interaction"))+
  theme(legend.position = "none", 
        plot.title = element_text(face="bold"))
figb


figab <- ggplot(df) + 
          geom_violin(aes(x = as.factor(type_int), 
                           y = f_ab, 
                           color = factor(type_int)))+ 
          labs(title = "A: interaction shapes and interaction effect size", 
               y = "interaction effect size", 
               x = "")+
          scale_color_manual(values = c(cols[1], cols[4]), 
                             labels = c("attenuated interaction", 
                                       "cross-over interaction"))+
          theme(legend.position = "none", 
                plot.title = element_text(face="bold"))
figab  


fig4 <- grid.arrange(
  figab,
  figb, 
  figa,
  nrow = 1, ncol = 3, 
  top=textGrob("Interaction shape and effect sizes", 
               gp=gpar(fontsize=16)) 
)

  

figd <- ggplot(df) + 
  geom_jitter(aes(x = f_ab, y = pwr_ab, color = type_int))+
  scale_color_manual(values = c(cols[1], cols[4]), 
                    labels = c("attenuated interaction", 
                               "cross-over interaction"))+
  facet_grid(rows = vars(factor(type_int)))+ 
  labs(y = "power level", 
       x = "interaction effect size f")+
  theme(legend.position = "none")


figd

fige<- ggplot(df)+ 
  geom_histogram(aes(x = pwr_a, fill = as.factor(type_int)), binwidth = 5)+ 
  scale_fill_manual(values = c(cols[1], cols[4]), 
                    labels = c("attenuated interaction", 
                               "cross-over interaction"))+ 
  labs(x = "power for main effect a")+
  facet_grid(rows = vars(factor(type_int)))+ 
  scale_y_log10()+
  theme(legend.position = "none")

fige

figf<- ggplot(df)+ 
  geom_histogram(aes(x = pwr_ab, fill = type_int))+ 
  scale_fill_manual(values = c(cols[1], cols[4]), 
                    labels = c("attenuated interaction", 
                               "cross-over interaction"))+
  facet_grid(rows = vars(factor(type_int)))+
  labs(x = "power for interaction effect")+
  scale_y_log10()+
  theme(legend.position = "none", 
        plot.title = element_text(face="bold"))

fig5 <- grid.arrange(
  figd,
  fige, 
  figf,
  nrow = 1, ncol = 3, 
  top=textGrob("Power for main and interaction effect size", 
               gp=gpar(fontsize=16)) 
)
fig5


ggplot(df) + geom_point(aes(x = pwr_a, y = pwr_ab, color = type_int))
