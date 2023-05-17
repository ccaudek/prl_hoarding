library(here)
library(tidyverse)
library(pROC)

quest <- rio::import(here("data", "processed", "quest", "quest_tot.csv"))
quest <- na.omit(quest)

# ocir_hoarding
quest %>%
  group_by(
    group
  ) %>%
  summarise(m_hoard = mean(ocir_hoarding, na.rm = TRUE), 
            m_check = mean(ocir_checking, na.rm = TRUE), 
            m_obs = mean(ocir_obsessing, na.rm = TRUE), 
            m_wash = mean(ocir_washing, na.rm = TRUE), 
            m_neut = mean(ocir_neutralizing, na.rm = TRUE), 
            m_ord = mean(ocir_ordering, na.rm = TRUE), 
            m_tot = mean(ocir_tot, na.rm = TRUE))

hist(log(quest$ocir_hoarding))

hddm_param <- rio::import(
  here::here("data", "processed", "_prl", "prl_params_cleaned", 
             "prl_params_and_subj_code_2023_03_15.rds")
)

hist(hddm_param$alpha_neg)

df1 <- inner_join(hddm_param, quest, by = "subj_code")
df1 <- na.omit(df1)

mod1 <- lm(cbind(alpha_pos, alpha_neg) ~ group + (ocir_hoarding + ocir_checking+ 
                                 ocir_obsessing+ ocir_washing+ 
                                 ocir_neutralizing+ ocir_ordering
                                 ), data = df1)
summary(mod1)
car::Anova(mod1) # output multivariate analysis

mod2 <- lm(a ~ group + (ocir_hoarding + ocir_checking+ 
                        ocir_obsessing+ ocir_washing+ 
                        ocir_neutralizing+ ocir_ordering
                        ), data = df1)
summary(mod2)

mod3 <- lm(alpha_neg ~ group*bai_tot, data = df1)
summary(mod3)

# df1 |> 
#   ggplot(aes(x = ocir_hoarding, y = alpha_pos)) + 
#   geom_point() + 
#   facet_wrap( ~ group)

df1$is_patients <- ifelse(df1$group == "controls", 0, 1)

# classification analysis (logistic regression)
mod4 <- glm(is_patients ~ (alpha_pos + alpha_neg + a + t + v), family = binomial(),
             data = df1)
summary(mod4)

# mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
# mylogit <- glm(admit ~ gre, data = mydata, family = "binomial")
prob=predict(mod4,type=c("response"))
df1$prob=prob
test_roc <- roc(df1$is_patients ~ prob, plot = TRUE, print.auc = TRUE)
# g_prl <- roc(is_patients ~ prob, data = df1)
# as.numeric(g_prl$auc)
# plot(g_prl)    

# test_prob = predict(mod4, newdata = default_tst, type = "response")
# test_roc = roc(default_tst$default ~ test_prob, plot = TRUE, print.auc = TRUE)
# as.numeric(test_roc$auc)

mod5 <- glm(is_patients ~ (ocir_tot + hrs_tot + bdi_tot), family = binomial(),
            data = df1)
summary(mod5)
prob=predict(mod5,type=c("response"))
df1$prob=prob
test_roc <- roc(df1$is_patients ~ prob, plot = TRUE, print.auc = TRUE)
# g_quest <- roc(is_patients ~ prob, data = df1)
# as.numeric(g_quest$auc)
# plot(g)    

