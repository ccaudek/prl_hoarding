library(here)
library(tidyverse)


# recode patients questionnaire -------------------------------------------
dat <- rio::import(here("data", "raw", "quest", "patients", "data_hd.xlsx"))

p1 <- data.frame(
  subj_code = dat$CODICE, 
  age = dat$età, 
  education = dat$`anni di istruzione`, 
  gender = dat$`genere (1 = femmina; 2 = maschio)`, 
  job_position = dat$Lavoro, 
  # insight = dat$Insight, 
  # disorder_origin = dat$`esordio sintomatologia accumulo`, 
  # medication = dat$`Trattamento psicofarmacologico`, 
  # sir_tot = dat$`SI-R totale`,
  # sir_waste = dat$`SI-R scarto`,
  # sir_hoarding = dat$`SI-R acquisizione`,
  # sir_obstruction = dat$`Si-R ingombro`,
  hrs_tot = dat$HRS, 
  # cir_kitchen = dat$`CIR cucina`,
  # cir_bedroom = dat$`CIR camera`,
  # cir_livroom = dat$`CIR salotto`, 
  # sci_tot = dat$`SCI TOTALE`, 
  bai_tot = dat$BAI, 
  bdi_tot = dat$`BDI II`, 
  ocir_tot = dat$`OCI-R totale`, 
  ocir_washing = dat$`OCI-R washing`, 
  ocir_obsessing = dat$`OCI-R obsessing`,
  ocir_ordering = dat$`OCI-R ordering`,
  ocir_hoarding = dat$`OCI-R hoarding`, 
  ocir_checking = dat$`OCI-R Checking`, 
  ocir_neutralizing = dat$`OCI-R mental Neutralization`, 
  eat26_tot = dat$`eat 26 (TOTALE)`, 
  bis11_tot = dat$`BIS 11`
) 
glimpse(p1)

p1 <- p1 %>% 
  mutate(
    across(
      c(gender),
      ~ recode(.,
               "1" = "Femmina",
               "2" = "Maschio"
      )
    )
  )
               
p1$group <- "patients"

rio::export(p1, here("data", "processed", "quest", "patients", "pat_quest.csv"))

# recode controls questionnaire -------------------------------------------

dat1 <- rio::import(here("data", "raw", "quest", "controls", "progetto_accumulo_usl.xlsx"))
dat1$`Informazioni cronologiche` <- NULL
dat1$`Pertanto dichiaro:` <- NULL

info_demo <- dat1[, 1:6] %>% 
  rename(
    subj_code = "Inserisca il suo codice anonimo (esempio: ma_ro_1997_05_04_174_m)",
    age = "Età (in anni)", 
    gender = "Genere",                                                                                                                                                                                                                     
    marital_status = "Stato civile",                                                                                                                                                                  
    education = "Titolo di studio", 
    job_position = "Condizione lavorativa"
  )

# recode BAI --------------------------------------------------------------

bai_all <- dat1[, c(7:28)]
bai_all$`Gambe che si illuminano` <- NULL # hanno risposto tutti "Per niente"
col_names <- sprintf("bai_%s", seq(1:21))
colnames(bai_all) <- col_names
# Recode values from alphanumeric to numeric.

bai_all <- bai_all %>%
  mutate_all(
    ~ recode(
      .,
      "Per niente"                                                  = 1,
      "Un po' (Non mi ha infastidito molto)"                        = 2,
      "Abbastanza (Era veramente spiacevole ma potevo sopportarlo)" = 3,
      "Molto (Potevo sopportarlo a malapena)"                       = 4
    )
  )

# Compute total score.
bai_all <- bai_all %>% 
  rowwise() %>%
  mutate(
    bai_tot = sum(c_across(bai_1:bai_21))
  )

# Add subj_code.
bai_all$subj_code <- info_demo$subj_code
glimpse(bai_all)

rio::export(bai_all, here("data", "processed", "quest", "controls", "bai_all.csv"))

# recode biss11 -----------------------------------------------------------

bis11_all <- dat1[, c(29:59)]
bis11_all$`Cambio residenza almeno una volta al giorno` <- NULL # hanno risposto tutti "Per niente"
col_names <- sprintf("bis_%s", seq(1:30))
colnames(bis11_all) <- col_names
# Recode values from alphanumeric to numeric.

bis11_all <- bis11_all %>% 
  mutate(
    across(
      c(bis_5, bis_11, bis_28, bis_2, bis_3, bis_4, bis_17, bis_19, bis_22, bis_25, 
        bis_14, bis_18, bis_27, bis_16, bis_21, bis_23, bis_6, bis_24, bis_26),
      ~ recode(.,
               "Mai/Raramente"       = 1,
               "Talvolta"            = 2,
               "Spesso"              = 3,
               "Quasi sempre/Sempre" = 4
      )
    )
  ) %>%
  mutate(
    across(
      c(bis_9, bis_20, bis_1, bis_8, bis_7, bis_12, bis_13, bis_10, bis_15, bis_29, bis_30),
      ~ recode(.,
               "Mai/Raramente"       = 4,
               "Talvolta"            = 3,
               "Spesso"              = 2,
               "Quasi sempre/Sempre" = 1
      )
    )
  )

# Compute total score.
bis11_all <- bis11_all %>% 
  rowwise() %>%
  mutate(
    bis11_tot = sum(c_across(bis_1:bis_30))
  )

# Add subj_code.
bis11_all$subj_code <- info_demo$subj_code
glimpse(bis11_all)

rio::export(bis11_all, here("data", "processed", "quest", "controls", "bis11_all.csv"))

# recode BDI II -----------------------------------------------------------

bdi_all <- dat1[, c(60:81)]
bdi_all$Lucentezza <- NULL
col_names <- sprintf("bdi_%s",seq(1:21))
colnames(bdi_all) <- col_names
bdi_all[] <- lapply(bdi_all, as.factor)

bdi_all <- mutate_if(
  bdi_all,
  is.factor,
  ~fct_recode(.,!!!c(
    # item 1 
    "0" = "0. Non mi sento triste.", 
    "1" = "1. Mi sento triste per la maggior parte del tempo.", 
    "2" = "2. Mi sento sempre triste.", 
    "3" = "3. Mi sento così triste o infelice da non poterlo sopportare.", 
    # item 2
    "0" = "0. Non sono scoraggiato riguardo al mio futuro.",
    "1" = "1. Mi sento più scoraggiato riguardo al mio futuro rispetto al solito.",
    "2" = "2. Non mi aspetto nulla di buono per me.",
    "3" = "3. Sento che il mio futuro è senza speranza e che continuerà a peggiorare.",
    # item 3
    "0" = "0. Non mi sento un fallito.",
    "1" = "1. Ho fallito più di quanto avrei dovuto.",
    "2" = "2. Se ripenso alla mia vita riesco a vedere solo una serie di fallimenti.",
    "3" = "3. Ho la sensazione di essere un fallimento totale come persona.",
    # item 4
    "0" = "0. Traggo lo stesso piacere di sempre dalle cose che faccio.",
    "1" = "1. Non traggo più piacere dalle cose come un tempo.",
    "2" = "2. Traggo molto poco piacere dalle cose che di solito mi divertivano.",
    "3" = "3. Non riesco a trarre alcun piacere dalle cose che una volta mi piacevano.",
    # item 5
    "0" = "0. Non mi sento particolarmente in colpa.",
    "1" = "1. Mi sento  in colpa per molte cose che ho fatto o che avrei dovuto fare.",
    "2" = "2.  Mi sento molto spesso in colpa.",
    "3" = "3. Mi sento sempre in colpa.",
    # item 6
    "0" = "0. Non mi sento come se stessi subendo una punizione.",
    "1" = "1. Sento che potrei essere punito",
    "2" = "2. Mi aspetto di essere punito.",
    "3" = "3. Mi sento come se stessi subendo una punizione.",
    # item 7
    "0" = "0. Considero me stesso come ho sempre fatto",
    "1" = "1. Credo meno in me stesso.",
    "2" = "2. Sono deluso di me stesso.",
    "3" = "3. Mi detesto.",
    # item 8 
    "0" = "0. Non mi critico né mi biasimo più del solito.",
    "1" = "1. Mi critico più spesso del solito.",
    "2" = "2.  Mi critico per tutte le mie colpe.",
    "3" = "3. Mi biasimo per ogni cosa brutta che mi accade.",
    # item 9
    "0" = "0. Non ho alcun pensiero suicida",
    "1" = "1.Ho pensieri suicidi ma non li realizzerei",
    "2" = "2. Sento che starei meglio se morissi.",
    "3" = "3. Se mi si presentasse l’occasione, non esiterei ad uccidermi.",
    # item 10
    "0" = "0. Non piango più del solito.",
    "1" = "1. Piango più del solito.",
    "2" = "2. Piango per ogni minima cosa.",
    "3" = "3. Ho spesso voglia di piangere ma non ci riesco.",
    # item 11 
    "0" = "0. Non mi sento più agitato o teso del solito.",
    "1" = "1. Mi sento più agitato o teso del solito",
    "2" = "2. Sono così nervoso o agitato al punto che mi è difficile rimanere fermo.",
    "3" = "3. Sono così nervoso o agitato che devo continuare a muovermi o fare qualcosa.",
    # item 12
    "0" = "0. Non ho perso interesse verso le altre persone o verso le attività.",
    "1" = "1. Sono meno interessato agli altri o alle cose rispetto a prima.",
    "2" = "2. Ho perso la maggior parte dell’interesse verso le altre persone o cose.",
    "3" = "3. Mi risulta difficile interessarmi a qualsiasi cosa.",
    # item 13
    "0" = "0. Prendo decisioni come sempre.",
    "1" = "1. Trovo più difficoltà del solito nel prendere decisioni.",
    "2" = "2. Ho molte più difficoltà nel prendere decisioni rispetto al solito.",
    "3" = "3. Non riesco a prendere nessuna decisione.",
    # item 14
    "0" = "0. Non mi sento inutile.",
    "1" = "1. Non mi sento valido e utile come un tempo.",
    "2" = "2. Mi sento più inutile delle altre persone.",
    "3" = "3. Mi sento completamente inutile su qualsiasi cosa.",
    # item 15
    "0" = "0. Ho la stessa energia di sempre.",
    "1" = "1. Ho meno energia del solito.",
    "2" = "2. Non ho energia sufficiente per fare la maggior parte delle cose.",
    "3" = "3. Ho così poca energia che non riesco a fare nulla.",
    # item 16
    "0" = "0.  Non ho notato alcun cambiamento nel mio modo di dormire.",
    "1" = "1a. Dormo un po’ più del solito.",
    "1" = "1b. Dormo un po’ meno del solito.",
    "2" = "2a. Dormo molto più del solito.",
    "2" = "2b. Dormo molto meno del solito.",
    "3" = "3a. Dormo quasi tutto il giorno.",
    "3" = "3b. Mi sveglio 1-2 ore prima e non riesco a riaddormentarmi.",
    # item 17
    "0" = "0. Non sono più irritabile del solito.",
    "1" = "1. Sono più irritabile del solito.",
    "2" = "2. Sono molto più irritabile del solito.",
    "3" = "3. Sono sempre irritabile.", 
    # item 18
    "0" = "0. Non ho notato alcun cambiamento nel mio appetito.",
    "1" = "1a. Il mio appetito è un po’ diminuito rispetto al solito.",
    "1" = "1b. Il mio appetito è un po’ aumentato rispetto al solito.",
    "2" = "2a. Il mi appetito è molto diminuito rispetto al solito.",
    "2" = "2b. Il mio appetito è molto aumentato rispetto al solito.",
    "3" = "3a. Non ho per niente appetito.",
    "3" = "3b. Mangerei in qualsiasi momento.",
    # item 19
    "0" = "0. Riesco a concentrarmi come sempre.",
    "1" = "1. Non riesco a concentrarmi come al solito.",
    "2" = "2. Trovo difficile concentrarmi per molto tempo",
    "3" = "3. Non riesco a concentrarmi su nulla.",
    # item 20
    "0" = "0. Non sono più stanco o affaticato del solito.",
    "1" = "1. Mi stanco e mi affatico più facilmente del solito.",
    "2" = "2. Sono così stanco e affaticato che non riesco a fare molte delle cose che facevo prima.",
    "3" = "3. Sono talmente stanco e affaticato che non riesco più a fare nessuna delle cose che facevo prima.",
    # item 21
    "0" = "0. Non ho notato alcun cambiamento recente nel mio interesse verso il sesso.",
    "1" = "1. Sono meno interessato al sesso rispetto a prima.",
    "2" = "2. Ora sono molto meno interessato al sesso.",
    "3" = "3. Ho completamente perso l’interesse verso il sesso"
    
  )
  )
) 
bdi_all <- mutate_if(bdi_all, is.factor, ~ as.numeric(as.character(.x)))

# Compute total score.
bdi_all <- bdi_all %>% 
  rowwise() %>%
  mutate(
    bdi_tot = sum(c_across(bdi_1:bdi_21))
  )

# Add subj_code.
bdi_all$subj_code <- info_demo$subj_code
glimpse(bdi_all)

rio::export(bdi_all, here("data", "processed", "quest", "controls", "bdi_all.csv"))

# recode eat26 ------------------------------------------------------------

eat26_all <- dat1[, c(87:113)]
eat26_all$`Mi è capitato di stare un anno intero senza mangiare niente` <- NULL # tutti hanno risposto "Mai"
col_names <- sprintf("eat26_%s", seq(1:26))
colnames(eat26_all) <- col_names

eat26_all <- eat26_all %>% 
  mutate(
    across(
      c(eat26_1:eat26_25),
      ~ recode(.,
               "Sempre"        = 3,
               "Molto spesso"  = 2,
               "Spesso"        = 1,
               "Qualche volta" = 0,
               "Raramente"     = 0,
               "Mai"           = 0
      )
    )
  ) %>%
  mutate(
    across(
      c(eat26_26),
      ~ recode(.,
               "Sempre"        = 0,
               "Molto spesso"  = 0,
               "Spesso"        = 0,
               "Qualche volta" = 1,
               "Raramente"     = 2,
               "Mai"           = 3
      )
    )
  )



# Compute total score.
eat26_all <- eat26_all %>%
  rowwise() %>%
  mutate(
    eat26_tot = sum(c_across(eat26_1:eat26_26))
  )

# Pathological avoidance of fattening foods and shape preoccupations; individuals who
# score high on this factor may be described as overestimators of their body size and
# who are dissatisfied with their shape and desire to be smaller.
eat26_all$dieting <- with(
  eat26_all,
  eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 + eat26_14,
  eat26_16 + eat26_17 + eat26_22 + eat26_23 + eat26_24 + eat26_26
)

# bulimia and food preoccupation: similar to the previous factor, but is positively
# related to bulimia and heavier body weight. High scores on this factor may be
# associated with poor outcome.
eat26_all$bulimia <- with(
  eat26_all,
  eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25
)

# factor largely comprised of items relecting self-control about food as well as
# those who acknowledge social pressure to gain weight. High scores on this factor
# are related to lower weight and absence of bulimia.
eat26_all$oral_control <- with(
  eat26_all,
  eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + eat26_20
)

# Add subj_code.
eat26_all$subj_code <- info_demo$subj_code
glimpse(eat26_all)

rio::export(eat26_all, here("data", "processed", "quest", "controls", "eat26_all.csv"))


# recode hrs --------------------------------------------------------------

hrs_all <- dat1[, c(114:119)]
hrs_all$`Ti è capitato di gettare via una cosa in un cestino della spazzatura sporco e poi correre a riprenderla?` <- NULL
col_names <- sprintf("hrs_%s", seq(1:5))
colnames(hrs_all) <- col_names

hrs_all[] <- lapply(hrs_all, as.factor)

hrs_all <- mutate_if(
  hrs_all,
  is.factor,
  ~fct_recode(.,!!!c(
    # item 1 
    "0" = "0 (Per niente difficile)", 
    "1" = "1", 
    "2" = "2 (Lievemente)", 
    "3" = "3", 
    "4" = "4 (Moderatamente)", 
    "5" = "5", 
    "6" = "6 (Difficile)", 
    "7" = "7", 
    "8" = "8 (Molto difficile)",
    # item 2
    "0" = "0 (Non ho difficoltà)", 
    "1" = "1", 
    "2" = "2 (Lieve)", 
    "3" = "3", 
    "4" = "4 (Moderata)", 
    "5" = "5", 
    "6" = "6 (Difficile)", 
    "7" = "7", 
    "8" = "8 (Molto difficile)",
    # item 3
    "0" = "0 Nessun problema", 
    "1" = "1 Problema lieve, occasionalmente (meno di una volta a settimana) acquista cose non necessarie o poche cose che non servono.", 
    "2" = "2 Moderato, regolarmente (una o due volte la settimana) acquista cose non necessarie o alcune cose che non servono.", 
    "3" = "3", 
    "4" = "4 Moderato, regolarmente (una o due volte la settimana) acquista cose non necessarie o alcune cose che non servono.", 
    "5" = "5", 
    "6" = "6 Serio, frequentemente (più volte a settimana) acquista cose non necessarie o molte cose che non servono.", 
    "7" = "7", 
    "8" = "8 Estremo, molto spesso (ogni giorno) acquista cose non necessarie o vasto numero di cose che non servono.",
    # item 4
    "0" = "0 (Per niente)", 
    "1" = "1", 
    "2" = "2 (Lievemente)", 
    "3" = "3", 
    "4" =  "4 (Moderatamente)", 
    "5" = "5", 
    "6" = "6 (Seriamente)", 
    "7" = "7", 
    "8" = "8 (In modo estremo)",
    # item 5
    "0" = "0 (Per niente)", 
    "1" = "1", 
    "2" = "2 (Lievemente)", 
    "3" = "3", 
    "4" =  "4 (Moderatamente)", 
    "5" = "5", 
    "6" = "6 (Seriamente)", 
    "7" = "7", 
    "8" = "8 (In modo estremo)"
  )
  )
) 
hrs_all <- mutate_if(hrs_all, is.factor, ~ as.numeric(as.character(.x)))

# Compute total score.
hrs_all <- hrs_all %>% 
  rowwise() %>%
  mutate(
    hrs_tot = sum(c_across(hrs_1:hrs_5))
  )
hrs_all$subj_code <- info_demo$subj_code
glimpse(hrs_all)

rio::export(hrs_all, here("data", "processed", "quest", "controls", "hrs_all.csv"))

# recode oci-r ------------------------------------------------------------
ocir_all <- dat1[, c(120:138)]
ocir_all$`Sento di dover dormire a testa in giù ogni notte` <- NULL # hanno risposto tutti "Per nulla"
col_names <- sprintf("oci_%s", seq(1:18))
colnames(ocir_all) <- col_names

ocir_all <- ocir_all %>%
  mutate_all(
    ~ recode(
      .,
      "Per nulla"  = 0,
      "Un poco"    = 1,
      "Abbastanza" = 2,
      "Molto"      = 3,
      "Moltissimo" = 4
    )
  )
                                   
ocir_all$checking <- with(
  ocir_all,
  oci_2 + oci_8 + oci_14
)

ocir_all$hoarding <- with(
  ocir_all,
  oci_1 + oci_7 + oci_13
)

ocir_all$washing <- with(
  ocir_all,
  oci_5 + oci_11 + oci_17
)

ocir_all$ordering <- with(
  ocir_all,
  oci_3 + oci_9 + oci_15
)

ocir_all$neutralizing <- with(
  ocir_all,
  oci_4 + oci_10 + oci_16
)

ocir_all$obsessing <- with(
  ocir_all,
  oci_6 + oci_12 + oci_18
)

ocir_all$oci_tot <- with(
  ocir_all,
  checking + hoarding + washing + ordering + neutralizing + obsessing
)

ocir_all$subj_code <- info_demo$subj_code
glimpse(ocir_all)

rio::export(ocir_all, here("data", "processed", "quest", "controls", "ocir_all.csv"))

# recode dass21 -----------------------------------------------------------

dass21_all <- dat1[, c(139:160)]
dass21_all$`Negli ultimi dieci anni ho dormito meno di dieci secondi per notte` <- NULL
col_names <- sprintf("dass_%s", seq(1:21))
colnames(dass21_all) <- col_names

# Recode values from alphanumeric to numeric.
dass21_all <- dass21_all %>%
  mutate_all(
    ~ recode(
      .,
      "Non mi è mai accaduto"                 = 0,
      "Mi è capitato qualche volta"           = 1,
      "Mi è capitato con una certa frequenza" = 2,
      "Mi è capitato quasi sempre"            = 3
    )
  )

# Stress
dass21_all$dass21_s <- 
  with(
    dass21_all,
    dass_1 + dass_6 + dass_8 + dass_11 + dass_12 + dass_14 + dass_18
  )

# Anxiety
dass21_all$dass21_a <- 
  with(
    dass21_all,
    dass_2 + dass_4 + dass_7 + dass_9 + dass_15 + dass_19 + dass_20
  )

# Depression
dass21_all$dass21_d <- 
  with(
    dass21_all,
    dass_3 + dass_5 + dass_10 + dass_13 + dass_16 + dass_17 + dass_21
  )

# Add subj_code.
dass21_all$subj_code <- info_demo$subj_code
glimpse(dass21_all)

rio::export(dass21_all, here("data", "processed", "quest", "controls", "dass21_all.csv"))


# -------------------------------------------------------------------------

d <- full_join(bai_all, bdi_all, by = "subj_code")
d1 <- full_join(d, bis11_all, by = "subj_code")
d2 <- full_join(d1, eat26_all, by = "subj_code")
d3 <- full_join(d2, dass21_all, by = "subj_code")
d4 <- full_join(d3, hrs_all, by = "subj_code")
quest_controls <- full_join(d4, ocir_all, by = "subj_code")
quest_controls$group <- "controls"

quest_controls <- full_join(info_demo, quest_controls, by = "subj_code")

quest_controls_finale <- data.frame(
  subj_code = quest_controls$subj_code,     
  age = quest_controls$age,           
  gender = quest_controls$gender,        
  # marital_status = quest_controls$marital_status,
  education = quest_controls$education,     
  job_position = quest_controls$job_position,
  group = quest_controls$group,
  bai_tot = quest_controls$bai_tot, 
  bdi_tot = quest_controls$bdi_tot,
  bis11_tot = quest_controls$bis11_tot, 
  eat26_tot = quest_controls$eat26_tot, 
  # dieting = quest_controls$dieting,
  # bulimia = quest_controls$bulimia,       
  # oral_control = quest_controls$oral_control, 
  # dass21_stress =  quest_controls$dass21_s,
  # dass21_anxiety = quest_controls$dass21_a,
  # dass21_dep = quest_controls$dass21_d,
  hrs_tot = quest_controls$hrs_tot, 
  ocir_checking = quest_controls$checking,     
  ocir_hoarding = quest_controls$hoarding,
  ocir_washing = quest_controls$washing,
  ocir_ordering = quest_controls$ordering,   
  ocir_neutralizing = quest_controls$neutralizing, 
  ocir_obsessing = quest_controls$obsessing,
  ocir_tot = quest_controls$oci_tot
)

rio::export(quest_controls_finale, here("data", "processed", "quest", "controls", "quest_tot_controls.csv"))


# combine patients and controls quest -------------------------------------

hc_quest <- rio::import(here("data", "processed", "quest", "controls", "quest_tot_controls.csv"))
dda_quest <- rio::import(here("data", "processed", "quest", "patients", "pat_quest.csv"))

quest_tot <- rbind(hc_quest, dda_quest)

rio::export(quest_tot, here("data", "processed", "quest", "quest_tot.csv"))





                      