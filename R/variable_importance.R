# Variable importance
#
# # https://stats.stackexchange.com/questions/175111/relative-importance-of-variables-in-cox-regression
# https://www.nejm.org/doi/suppl/10.1056/NEJMoa1800256/suppl_file/nejmoa1800256_appendix.pdf from https://www.nejm.org/doi/full/10.1056/NEJMoa1800256
# library(survival); library(rms)http://hbiostat.org/doc/rms1.pdf p82
# https://stats.stackexchange.com/questions/155246/which-variable-relative-importance-method-to-use?noredirect=1&lq=1
# doi.org/10.1002/9781118307656.ch6
score_f <- function(fit){
  res <- plot(anova(fit), sort='none', pl=FALSE)
  res[res<0] <- 0
  res <- res/sum(res)
  return(res)}
VI_CI_f <- function(dt,f,B=1000, risk_group_name, nvariables  = 5){
  n <- nrow(dt)
  score_m<- matrix(NA, nrow=B, ncol=nvariables)
  f2update <- f
  # #plot(anova(f), what='proportion chisq')
  res1 <- plot(anova(f), what='proportion chisq',sort='none',pl = FALSE)
  Score <- score_f(f2update)
  for(i in 1:B) {
    j <- sample(1:n, n, TRUE)
    bootfit <- update(f2update, data=dt, subset=j)
    score_m[i,] <- score_f(bootfit)
  }
  lim <- t(apply(score_m, 2, quantile, probs=c(0.05,0.5,.95), na.rm = TRUE))
  res <- data.frame(variables = names(res1),low = lim[,1], values = lim[,2], up = lim[,3], risk_group = risk_group_name)
  return(res)
}
nB <- 100

#### 3 stages #####
dt_by_group <- data_demographics_ogtt %>%
  select(any_of(c(parameters$variables,parameters$time,parameters$outcome,"Draw.Date.x","AB_group", c("DPTRS","DPTRS60","M120","CPH","LR","ADA_dysglycemia")))) %>%
  drop_na() %>%
  mutate(risk_group_name = case_when(
    AB_group %in% c("GAD","IAA","IA2A") ~ "Single AB",
    ADA_dysglycemia == "Euglycemia" & !(AB_group %in% c("GAD","IAA","IA2A")) ~ "Stage 1",
    ADA_dysglycemia != "Euglycemia" & !(AB_group %in% c("GAD","IAA","IA2A")) ~"Stage 2"
  )) %>%
  filter(!is.na(risk_group_name))

numbers <- dt_by_group %>%   group_by(risk_group_name) %>%
  summarise(Freq = n())
dt <- dt_by_group %>% filter(risk_group_name == "Single AB")
f <- cph(Surv(time_since_first_screening, T1D.Indicator)  ~ GRS2 + Index60 + Z_BMI +age_at_first_drawing + AB_group, x=TRUE, y=TRUE, data=dt)
res0 <- VI_CI_f(dt,f,B=nB , risk_group_name = "Single AB", nvariables = 5)

res0
dt <- dt_by_group %>% filter(risk_group_name == "Stage 1")
f <- cph(Surv(time_since_first_screening, T1D.Indicator)  ~ GRS2 + Index60 + Z_BMI +age_at_first_drawing + AB_group, x=TRUE, y=TRUE, data=dt)
res1 <- VI_CI_f(dt,f,B=nB , risk_group_name = "Stage 1", nvariables = 5)
res1
dt <- dt_by_group %>% filter(risk_group_name == "Stage 2")
f <- cph(Surv(time_since_first_screening, T1D.Indicator)  ~ GRS2 + Index60 + Z_BMI +age_at_first_drawing + AB_group, x=TRUE, y=TRUE, data=dt)
res2 <- VI_CI_f(dt,f,B=nB , risk_group_name = "Stage 2", nvariables = 5)
res2
res <- rbind(res0,res1,res2)
res <- res %>% mutate(variables = case_when(
  variables == "IA2_titer" ~ "IA2A titer",
  variables == "AB_group" ~ "AB group",
  variables =="age_at_first_drawing" ~ "Age",
  variables =="Z_BMI" ~ "Z-BMI",
  TRUE ~ variables
)) %>%
  mutate(variables = factor(variables, levels = c("GRS2","Index60","Age","AB group","Z-BMI"))) %>%
  mutate(risk_group = factor(risk_group, levels = c("Single AB","Stage 1","Stage 2"))) %>%
  group_by(risk_group) %>%
  mutate(low = ifelse(low < 0, 0 , low)) %>%
  ungroup() %>%
  left_join(numbers, by = c( "risk_group" = "risk_group_name")) %>%
  mutate(risk_group_n = paste0(risk_group, " (n = ", Freq,")"))
res

ggplot(res, aes(x=variables, y=values, colour = risk_group_n,ymin = low, ymax = up)) +
  # geom_segment( aes(xend=variables, yend=0)) +
  geom_point( size=4) +
  geom_errorbar() +
  theme_bw() +
  xlab("") +
  ylab(expression(Proportion~chi^2)) +
  facet_grid(~risk_group_n) +
  theme(legend.position = "none") +
theme(text=element_text(size= 16)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="plain")) 
ggsave(filename = paste0(path_figures,"variables importance all.JPEG"), height = 8, width = 8)

##### split Single AB by glycemic status #######


dt <- dt_by_group %>% filter(risk_group_name == "Single AB") %>% 
  filter(ADA_dysglycemia == "Euglycemia")
f <- cph(Surv(time_since_first_screening, T1D.Indicator)  ~ GRS2 + Index60 + Z_BMI +age_at_first_drawing + AB_group, x=TRUE, y=TRUE, data=dt)
res0E <- VI_CI_f(dt,f,B=nB , risk_group_name = "single AB with euglycemia", nvariables = 5)

dt <- dt_by_group %>% filter(risk_group_name == "Single AB") %>% 
  filter(ADA_dysglycemia != "Euglycemia")
f <- cph(Surv(time_since_first_screening, T1D.Indicator)  ~ GRS2 + Index60 + Z_BMI +age_at_first_drawing + AB_group, x=TRUE, y=TRUE, data=dt)
res0D <- VI_CI_f(dt,f,B=nB , risk_group_name = "single AB with dysglycemia", nvariables = 5)

res <- rbind(res0E,res0D)

numbers <- dt_by_group %>% filter(risk_group_name == "Single AB") %>%  mutate(sub_group = ifelse(ADA_dysglycemia == "Euglycemia","single AB with euglycemia","single AB with dysglycemia")) %>%  group_by(sub_group) %>%
  summarise(Freq = n())

res <- res %>% mutate(variables = case_when(
  variables == "IA2_titer" ~ "IA2A titer",
  variables == "AB_group" ~ "AB group",
  variables =="age_at_first_drawing" ~ "Age",
  variables =="Z_BMI" ~ "Z-BMI",
  TRUE ~ variables
)) %>%
  mutate(variables = factor(variables, levels = c("GRS2","Index60","Age","AB group","Z-BMI"))) %>%
  mutate(risk_group = factor(risk_group, levels = c("single AB with euglycemia","single AB with dysglycemia"))) %>%
  group_by(risk_group) %>%
  mutate(low = ifelse(low < 0, 0 , low)) %>%
  ungroup() %>%
  left_join(numbers, by = c( "risk_group" = "sub_group")) %>%
  mutate(risk_group_n = paste0(risk_group, " (n = ", Freq,")"))

ggplot(res, aes(x=variables, y=values, colour = risk_group_n,ymin = low, ymax = up)) +
  # geom_segment( aes(xend=variables, yend=0)) +
  geom_point( size=4) +
  geom_errorbar() +
  theme_bw() +
  xlab("") +
  ylab(expression(Proportion~chi^2)) +
  facet_grid(~risk_group_n) +
  theme(legend.position = "none") +
  theme(text=element_text(size= 15)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="plain")) 
ggsave(filename = paste0(path_figures,"variables importance single,Stage 1 and 2.JPEG"), height = 8, width = 13)


#### Single AB 3 stages #####

