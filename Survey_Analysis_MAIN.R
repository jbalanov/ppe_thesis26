###setup
library(tidyverse)
library(stargazer)
library(flextable)
library(mediation)
library(DiagrammeR)
setwd("C:\\Users\\Jackie\\Dropbox\\PPE Thesis Jackie\\")

survey <- read.csv("experimental_data_jb_thesis.csv")

###get all incomplete and test responses
survey_clean <- survey %>%
  tail(-2) %>%
  filter(!Status %in% c("Survey Preview", "Survey Test")) %>%
  filter(Inf_Name != "") %>%
  filter(PE_Internal_1 != "")  %>% 
  #remove two other test results 
  tail(-2) %>%
  #i deleted all emails out of the column but kept my own to 
  #delete this rogue test observation!
  filter(email != "jackieba@sas.upenn.edu") 


###reshape data 

#print n's for each condition
survey_clean %>% 
  #make group column
  mutate(group = paste0(Protest,Proximate)) %>%
  dplyr::select(group) %>% 
  group_by(group) %>% 
  summarize(n = n())

#list to "grade" the information questionnaire responses
answer_key <- c(
  Inf_Event_P = "A protest",
  Inf_Event_C = "A press conference",
  Inf_Num = "250",
  Inf_May = "A mayor",
  Inf_Name = "Christa Park",
  Inf_Educ = "Undergraduate",
  Inf_Policy = "Zoning",
  Inf_City_Ph = "Philadelphia, PA",
  Inf_City_B = "Bend, OR",
  Inf_uni = "A university",
  Inf_Issue_Ph = "Gentrification",
  Inf_Issue_B = "Low housing supply"
)

#remove all unecessary metadata columns
scoring <- survey_clean %>%
  dplyr::select(ResponseId, contains("Inf_"), contains("PE_"), Age, Gender,
                Politics, Q53, Protest, Proximate) 

#"grade" responses - get accuracy for each question per group
scoring_pcts <- scoring %>% 
  #replace any empty responses with NA, for questions that had 
  #varying responses by group, useful later for the coalesce function
  mutate(across(everything(), na_if, "")) %>%
  group_by(Protest, Proximate) %>%
  #across all columns in the "answer key" list, get the percent that match
  #the key
  summarize(
    across(all_of(names(answer_key)),
      ~ mean(.x == answer_key[cur_column()], na.rm = TRUE),
      .names = "pct_{.col}"))


#checking what happened with this question that had a particularly 
#lower accuracy for the non-rpotest groups
weird_ans <- scoring %>%
  filter(Inf_Event_C != "A press conference" & Inf_Event_C != "") %>%
  group_by(Inf_Event_C) %>%
  summarise(n = n())


#pe scale and final scoring
scoring_full <- scoring %>% 
  mutate(across(everything(), na_if, "")) %>%
  #new column for each Q, 1 if correct, 0 if incorrect
  mutate(across(all_of(names(answer_key)),
      ~ case_when(is.na(.x) ~ NA_real_,
        .x == answer_key[cur_column()] ~ 1,
        TRUE ~ 0),
      .names = "{.col}_scored")) %>%
  mutate(across(contains("PE_"),
      ~ case_when(
        .x == "Strongly agree" ~ 5,
        .x == "Somewhat agree" ~ 4,
        .x == "Neither agree nor disagree" ~ 3,
        .x == "Somewhat disagree" ~ 2,
        .x == "Strongly disagree" ~ 1,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  rowwise(ResponseId) %>%
  #add and average info score and pe scales
  mutate(score = sum(c_across(contains("_scored")), na.rm = T),
         pe = mean(c_across(contains("PE_")), na.rm = T), 
         pe_int = mean(c_across(contains("_External_")), na.rm = T), 
         pe_ext = mean(c_across(contains("_Internal_")), na.rm = T), 
         condition = as.factor(paste0(Protest, Proximate)))

#main ols models and PE dimension models
infools <- lm(score ~ condition, data = scoring_full)
peols <- lm(pe ~ condition, data = scoring_full)
intols <- lm(pe_int ~ condition, data = scoring_full)
extols <- lm(pe_ext ~ condition, data = scoring_full)

#Table 2
stargazer(infools,
          peols, 
          type = "latex",
          title = "OLS Models - By Condition",
          dep.var.labels = c("Information Retention Score",
                             "Political Efficacy Score"),
          covariate.labels = c(
            "Protest/Proximate",
            "Non-Protest/Proximate",
            "Protest/Non-Proximate"
          ),
          keep.stat = c("n", "adj.rsq"))

#Table 3
stargazer(intols,
          extols, 
          type = "latex",
          title = "OLS Models - Political Efficacy Dimensions",
          dep.var.labels = c("Internal Political Efficacy",
                             "External Political Efficacy"),
          covariate.labels = c(
            "Protest/Proximate",
            "Non-Protest/Proximate",
            "Protest/Non-Proximate"
          ),
          keep.stat = c("n", "adj.rsq"))

#separated models
infoprox <- lm(score ~ Proximate, data = scoring_full)
peprox <- lm(pe ~ Proximate, data = scoring_full)
infoprot <- lm(score ~ Protest, data = scoring_full)
peprot <- lm(pe ~ Protest, data = scoring_full)

#Table 2
stargazer(infoprox,
          peprox, 
          infoprot,
          peprot,
          interact,
          type = "latex",
          title = "OLS Models",
          dep.var.labels = c("Information",
                             "Political Efficacy",
                             "Information",
                             "Political Efficacy"),
          covariate.labels = c("Proximate",
                               "Protest",
                               "Proximate:Protest"),
          keep.stat = c("n", "adj.rsq"))

###multiple conditions permutation test
set.seed(2024)
n_permutations <- 5000
control_cond <- "00"

#labels and recodings
treatment_conds <- scoring_full %>%
  filter(as.character(condition) != control_cond) %>%
  pull(condition) %>%
  unique() %>%
  as.character()

condition_labels <- c(
  "11" = "Protest/Proximate",
  "01" = "Non-Protest/Proximate",
  "10" = "Non-Protest/Non-Proximate"
)

outcome_labels <- c(
  "pe" = "Political Efficacy",
  "score" = "Information Retention"
)

#initialize lists
diffmeans <- list()
permeans <- list()
titles <- list()
count <- 1
for (outcome in c("score", "pe")) {
  cat("\n--- Outcome:", outcome, "---\n")
  for (trt in treatment_conds) {
    
    sub_data <- scoring_full %>%
      filter(as.character(condition) %in% c(control_cond, trt)) %>%
      mutate(condition_char = as.character(condition))  
    
    # Observed difference (treatment - control)
    obs_diff <- mean(sub_data[[outcome]][sub_data$condition_char == trt], na.rm = TRUE) -
      mean(sub_data[[outcome]][sub_data$condition_char == control_cond], na.rm = TRUE)
    
    # Permuted differences
    permuted_diffs <- replicate(n_permutations, {
      shuffled_condition <- sample(sub_data$condition_char)  
      mean(sub_data[[outcome]][shuffled_condition == trt], na.rm = TRUE) -
        mean(sub_data[[outcome]][shuffled_condition == control_cond], na.rm = TRUE)
      
    })
    
    
    p_val <- mean(abs(permuted_diffs) >= abs(obs_diff))
    
    cat(sprintf("  %s vs. control00: obs_diff = %.3f, p = %.4f\n", trt, obs_diff, p_val))
    
    permeans[[count]] <- permuted_diffs
    diffmeans[[count]] <- obs_diff
    titles[[count]] <- paste0(condition_labels[trt], 
                              " Condition - ", outcome_labels[outcome])
    
    count <- count + 1
    
  }
}

#make and save graphs quickly (Appendix C)
for(i in c(1:6)) {
  png(paste0(str_remove_all(titles[[i]], " |/"), ".png"), width = 700, height = 350)
  hist(permeans[[i]],col="gray",las=1
       ,main=titles[[i]], xlab = "Permutations")
  abline(v=diffmeans[[i]],col="red")
  dev.off()
}

###Mediation
model.M <- lm(score ~ Protest * Proximate, data = scoring_full)
model.Y <- lm(pe ~ Protest * Proximate + score, data = scoring_full)

medprot <- mediate(model.M , model.Y,
                   treat = "Protest",
                   mediator = "score",
                   boot = T,
                   sims = 10000)

medprox <- mediate(model.M , model.Y,
                   treat = "Proximate",
                   mediator = "score",
                   boot = T,
                   sims = 10000)


plot(medprox, main = "Mediation Analysis - Proximity", xlab = "Magnitude",
     ylab = "Effects")

plot(medprot, main = "Mediation Analysis - Protest Exposure",
     xlab = "Magnitude",
     ylab = "Effects")


###Figures 1 and 2
grViz("
digraph mediation {

  graph [layout = dot, rankdir = LR]

  node [shape = box, style = rounded, width = 2]

  Prox [label = 'Proximity']
  Score [label = 'Information Retention']
  Eff [label = 'Political Efficacy']

  #a path
  Prox -> Score

  #b path
  Score -> Eff 

  #c path
  Prox -> Eff 

}
")


grViz("
digraph mediation {

  graph [layout = dot, rankdir = LR]

  node [shape = box, style = rounded, width = 2]

  Prot [label = 'Protest Exposure']
  Score [label = 'Information Retention']
  Eff [label = 'Political Efficacy']

  # a path
  Prot -> Score

  # b path
  Score -> Eff 

  # c path
  Prot -> Eff 

}
")

###descriptive stats for sample
table(survey_clean$Gender)

survey_clean <- survey_clean %>%
  mutate(Age = ifelse(Age == "100+", 100, as.numeric(Age))) 

median(survey_clean$Age, na.rm = T)
sd(survey_clean$Age, na.rm = T)

##descriptive stats for answers
mean(scoring_full$score)
mean(scoring_full$pe)
mean(scoring_full$pe_int)
mean(scoring_full$pe_ext)


###Code to make Table 1
chart <- scoring_pcts %>%
  ungroup() %>%
  mutate(
    across(everything(), ~ ifelse(is.nan(.), NA, .)),
    across(everything(), as.numeric),
    across(3:ncol(scoring_pcts), ~. * 100)
  ) %>%
  mutate(
    pct_Inf_Event  = coalesce(pct_Inf_Event_P,  pct_Inf_Event_C),
    pct_Inf_City = coalesce(pct_Inf_City_Ph, pct_Inf_City_B),
    pct_Inf_Issue = coalesce(pct_Inf_Issue_Ph, pct_Inf_Issue_B)
  ) %>%
  select(-ends_with("_B"), -ends_with("_P"), 
         -ends_with("_C"), -ends_with("_Ph")) %>%
  mutate(Protest = ifelse(Protest == 1, "Protest", "Non-Protest"),
         Proximate = ifelse(Proximate == 1, "Proximate", "Non-Proximate")) %>%
  mutate(Condition = paste0(Protest,"/", Proximate)) %>%
  select(Condition,
         pct_Inf_Event,
         pct_Inf_Num,
         pct_Inf_May,
         pct_Inf_Name, 
         pct_Inf_Educ,
         pct_Inf_Policy,
         pct_Inf_City,
         pct_Inf_uni, 
         pct_Inf_Issue)

#a dictionary to make chart labels
chartnames <- c("Condition",
                "What was the event?",
                "About how many community members attended the event?",
                "Which public figure was in attendance?",
                "Who was the student who was interviewed?",
                "What level of education is the student enrolled in?",
                "Which housing policy topic was explicitly mentioned in the article?",
                "What city did the event take place in?",
                "What was the location of the event?",
                "Which of the following is an issue specific to the city mentioned in the article?")

colnames(chart) <- chartnames

chart_long <- chart %>%
  pivot_longer(cols = c(2:10), names_to = "Question") %>%
  pivot_wider(names_from = Condition, values_from = value) %>%
  mutate(across(-1, ~scales::percent(./100, accuracy = 0.01)))

#chart_long %>% write.csv("chartaccuracy.csv")

#library(report)
#cite_packages