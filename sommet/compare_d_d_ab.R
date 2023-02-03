
cols <- RColorBrewer::brewer.pal(6, "Dark2")
theme_set(theme_classic(base_size = 12))
letter <- theme(plot.title = element_text(face="bold"))

# functions to calculate total N with power = 80% and alpha-level = 0.05
# for the main effect a (treatment vs. control)

calculate_ntotal_interaction <- function(d_ab, power = 0.8, alpha = 0.05) {
  numerator <- (4 * (qnorm(1 - alpha/2)+qnorm(power))^2)
  ntotal <- numerator/(d_ab^2)
}

calculate_ntotal_main <- function(d_a, power = 0.8, alpha = 0.05) {
  numerator <- (4 * (qnorm(1 - alpha/2)+qnorm(power))^2)
  ntotal <- numerator/(d_a^2)
}




d_a <- seq(0.1, 1.5, by = 0.1)

d_ab2 <- d_a/2
d_ab3 <- d_a/3
d_ab4 <- d_a/4


df <-  tibble(d_a = d_a, 
       d_ab2 = d_ab2, 
       d_ab3 = d_ab3, 
       d_ab4 = d_ab4) %>%
       gather(key = size_i, 
              value = d_i, 
              d_ab2:d_ab4)

df$n_main <- vector(length = nrow(df))
df$n_i <- vector(length = nrow(df))

for (i in 1:nrow(df))  {
  df$n_main[i] <- calculate_ntotal_interaction(d_ab = df$d_a[i]) 
  df$n_i[i] <- calculate_ntotal_interaction(d_ab = df$d_i[i])
}

df$ratio_n <- df$n_i/df$n_main


df <- df %>%
        gather(key = effect, value = n, n_main:n_i)


df$size_i <- factor(df$size_i, 
                    levels = c("d_ab2", "d_ab3", "d_ab4"),
                    labels = c("2", "3", "4")) 
                    

# visualization 


fig1<-ggplot(df) + 
  geom_point(aes(x = d_a, y = n, color = effect))+ 
  facet_grid(rows = vars(factor(size_i)))+ 
  scale_y_log10()+ 
  scale_color_manual(values = c(cols[1], cols[4]), 
                     labels = c("n interaction, 80%", "n main, 80%"))+
  annotation_logticks(sides = "l")+ 
  labs(title = "Comparing n for main and interaction effect for 80% power",
        x = "Cohen's d main effect")

fig1
ggsave("fig1.png", plot = fig1) 

fig2 <- ggplot(df) + 
          geom_line(aes(x = d_a, y = ratio_n, color = size_i)) 
fig2


fig3 = ggplot(df) +
  geom_point(aes(x = d_a, y = n, color = size_i, shape = effect))+ 
  scale_y_log10()+ 
  annotation_logticks(sides = "l")+ 
  labs(title = "Comparing n for main and interaction effect for 80% power",
       x = "Cohen's d main effect")

fig3
ggsave("fig3.png", plot = fig3) 
