
set.seed(100)
s <- 1000


index <- 1:s
a1_b1 <- rnorm(s, 0.7, 0.2)
a1_b2 <- rnorm(s, 0.5, 0.2) 
a2_b1 <- runif(s, 0.9, 2) # range of equally plausible values for males in treatment group
a2_b2 <- runif(s, 0.3, 2.5) # range of equally plausible values for females in treatment group 


dat <- tibble(index = index,
              a1_b1 = a1_b1, 
              a1_b2 = a1_b2, 
              a2_b1 = a2_b1, 
              a2_b2 = a2_b2, 
              n = n, 
              sd = sd) 

dat$d_a <- (a2_b1 + a2_b2)/2 - (a1_b1 + a1_b2)/2 
dat$d_b <- (a2_b2 + a1_b2)/2 - (a1_b1 + a2_b1)/2
dat$d_ab <- ((a2_b2 - a1_b2) - (a2_b1 - a1_b1))/2


dat$ntotal_main_a <- NA
dat$ntotal_interaction_ab <- NA
dat$log_ntotal_main_a <- NA
dat$log_ntotal_interaction_ab <- NA


for (i in 1:nrow(dat)) {
  
  dat$ntotal_main_a[i] <- calculate_ntotal_main(d_a = dat$d_a[i])
  dat$ntotal_interaction_ab[i] <- calculate_ntotal_interaction(d_ab = dat$d_ab[i])
  dat$log_ntotal_main_a[i] <- log10(dat$ntotal_main_a[i])
  dat$log_ntotal_interaction_ab[i] <- log10(dat$ntotal_interaction_ab[i])
}


plot(dat$d_a, dat$d_ab)
plot(dat$d_a, dat$log_ntotal_main_a)
plot(dat$d_ab, dat$log_ntotal_interaction_ab)


dat1 <- dat %>%
          select(index, d_a, d_ab, log_ntotal_main_a, log_ntotal_interaction_ab)%>%
          gather(key = estimate_term, value = d_value, d_a: d_ab)

dat2 <- dat %>%
          gather(key = samplesize_interest, value = total_samplesize, log_ntotal_main_a:log_ntotal_interaction_ab)


dat1 %>%
  group_by(estimate_term)%>%
  summarize(per25_d = quantile(d_value, p = 0.25), 
            per50_d = quantile(d_value, p = 0.5), 
            per75_d = quantile(d_value, p = 0.75))


dat2 %>% 
  group_by(samplesize_interest)%>%
  summarize(mean_logn = mean(total_samplesize))


ggplot(dat1) + geom_point(aes(x = d_value, 
                              y = total_samplesize,
                              color = samplesize_interest, 
                              shape = estimate_term))

