## ------------------------------------------------------------------
## 30_ddm_quest.R
## ------------------------------------------------------------------
install.packages("callr")

library("here")
library("tidyverse")
library("jtools")
library("kableExtra")
library("ggpubr")
library("rstan")
library("rstanarm")
library("brms")

#------------------------------------------------------------------------------
# read PRL params
#------------------------------------------------------------------------------


prl_params <- readRDS(
  here(
    "data", "processed", "reappraisal", "subj_ddm_params.rds"
  )
)
length(unique(prl_params$subj_name))

prl_params$subj_idx <- NULL


prl_wide <- pivot_wider(
  prl_params, 
  names_from = c("parameter"), 
  values_from = param
)


# import reappraisal analysis ---------------------------------------------

reap_data <- rio::import(
  here("data", "processed", "reappraisal", "reappraisal_analysis.xlsx")
) %>% 
  dplyr::rename(
    subj_name = subj_id
  )
length(unique(reap_data$subj_name))


# import SEQ --------------------------------------------------------------

seq_data <- rio::import(
  here("data", "processed", "reappraisal", "seq_full.rds")
) %>%
  dplyr::rename(
    subj_name = subj_code
  )
length(unique(seq_data$subj_name))

# seq_data <- as.numeric(unlist(seq_data)) %>%
#   as.data.frame()

# import TRIPM ------------------------------------------------------------

tripm_data <- rio::import(
  here("data", "processed", "reappraisal", "tripm_reap.rds")
) %>% 
  dplyr::rename(
    subj_name = subj_code
  )
length(unique(tripm_data$subj_name)) 

# imp_seq <- mice::mice(seq_data, m = 1)
# sheets <- complete(imp_seq)

# combine prl_wide with quest_data
d_prl1 <- inner_join(prl_wide, reap_data, by = "subj_name") 
d_prl_new <- inner_join(d_prl1, seq_data, by = "subj_name") 
# d_prl_new <- inner_join(d_prl2, tripm_data, by = "subj_name") 

length(unique(d_prl_new$subj_name))

hist(d_prl$a)
hist(d_prl$v)
hist(d_prl$t)
hist(d_prl$z)

# imp1 <- mice::mice(d_prl, m = 1)
# d_prl_new <- complete(imp1)


# data analysis -----------------------------------------------------------

# Correlations ------------------------------------------------------------

# c'è una correlazione tra pos_alpha e emotion_reactivity
# c'è una correlazione tra z e emotion_reactivity
# c'è una correlazione tra z e regulation_success
# c'è una correlazione tra t e emotion_reactivity


d_prl_new$subj_name <- NULL
d_prl_new$RIVALUTA_Negativi <- NULL
d_prl_new$GUARDA_Negativi <- NULL
d_prl_new$GUARDA_Neutri <- NULL
d_prl_new$SEQ_T1_Ansia <- NULL
d_prl_new$SEQ_T2_Ansia <- NULL
d_prl_new$SEQ_T3_Ansia <- NULL
d_prl_new$SEQ_T1_Rabbia <- NULL
d_prl_new$SEQ_T2_Rabbia <- NULL
d_prl_new$SEQ_T3_Rabbia <- NULL
d_prl_new$SEQ_T1_Demoralizzazione <- NULL
d_prl_new$SEQ_T2_Demoralizzazione <- NULL
d_prl_new$SEQ_T3_Demoralizzazione <- NULL


mydata.cor = cor(d_prl_new, method = c("spearman"))
mydata = cor(d_prl_new)
                 
install.packages("Hmisc")
library("Hmisc")
mydata.rcorr = rcorr(as.matrix(d_prl_new))
mydata.rcorr



install.packages("corrplot")
library(corrplot)
corrplot(mydata.cor)

corrplot(mydata, type = "upper", order = "original",
         method = "color", tl.pos = "td", tl.cex = 0.5, 
         diag = FALSE, p.mat = mydata, sig.level = 0.01)

# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(mydata.cor, method = "color", col = col(200),  
#          type = "upper", order = "hclust", 
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col = "darkblue", tl.srt = 45, #Text label color and rotation
#          # Combine with significance level
#          p.mat = mydata.cor, sig.level = 0.01,  
#          # hide correlation coefficient on the principal diagonal
#          diag = FALSE 
# )

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- mydata
chart.Correlation(my_data, histogram = TRUE, pch = 19)



cor.test(d_prl_new$pos_alpha, d_prl_new$growth, 
         method = "pearson")


ggscatter(d_prl_new, x = "pos_alpha", y = "regulation_success", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "regulation_success", ylab = "alpha reward")


ggscatter(d_prl_new, x = "alpha", y = "emotion_reactivity", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "emotion_reactivity", ylab = "alpha")


# Regression models -------------------------------------------------------

m1 <- lm(
  cbind(a, v, t, z) ~
    emotion_reactivity + regulation_success,
  data = d_prl_new
)
summary(m1)
car::Anova(m1)


m <- lm(pos_alpha ~  emotion_reactivity + regulation_success, data = d_prl_new)
summary(m)
car::Anova(m)


m2 <- lm(
  cbind(alpha, pos_alpha) ~
    emotion_reactivity + regulation_success,
  data = d_prl_new
)
summary(m2)
car::Anova(m2)
 


m3 <- lm(
  cbind(alpha, pos_alpha, a, t, v, z) ~
    emotion_reactivity + regulation_success,
  data = d_prl_new
)
summary(m3)
car::Anova(m3)


# Bayesian model ----------------------------------------------------------

mod <- brms::brm(
  pos_alpha ~ Meanness + Boldness + Disinhibition, 
  family = skew_normal(),
  backend="cmdstan", 
  data = d_prl_new
)

summary(mod)
bayes_R2(mod)

conditional_effects(mod, effects = "Boldness")

pp_check(mod, nsamples = 200)
# set_cmdstan_path()
# cmdstan_path()
# cmdstan_version(error_on_NA = TRUE)







