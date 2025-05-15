library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
exported_data <- read_csv("rawdata_value.csv")
exported_data <- subset(exported_data, select = -c(StartDate,EndDate,Status,IPAddress,Progress,Finished,RecordedDate, RecipientLastName,RecipientFirstName,
                                                   RecipientEmail,ExternalReference,LocationLatitude,LocationLongitude,DistributionChannel,UserLanguage))
rawdata_value <- subset(exported_data, select =-c(ResponseId,Q1.1,Q2.1))
rawdata_value <- rawdata_value[3:nrow(rawdata_value), ]

# rawdata_value <- rawdata_value %>%
#   mutate(group = case_when(
#     !is.na(Q4.2)  ~ 1,
#     !is.na(Q6.2)  ~ 2,
#     !is.na(Q8.2)  ~ 3,
#     !is.na(Q10.2) ~ 4,
#     !is.na(Q12.2) ~ 5
#   ))

rawdata_value <- rawdata_value %>%
  mutate(group = case_when(
    !is.na(Q4.2)  ~ 'controlled',
    !is.na(Q6.2)  ~ 'errorbar',
    !is.na(Q8.2)  ~ 'shadedbar',
    !is.na(Q10.2) ~ 'fanchartslice',
    !is.na(Q12.2) ~ 'bellcurve'
  ))

# filter treatment groups
controlled_group <- rawdata_value[!is.na(rawdata_value$Q4.2), ]
error_bar <-rawdata_value[!is.na(rawdata_value$Q6.2), ]
shaded_bar <-rawdata_value[!is.na(rawdata_value$Q8.2), ]
fanchart_slice <-rawdata_value[!is.na(rawdata_value$Q10.2), ]
bell_curve <-rawdata_value[!is.na(rawdata_value$Q12.2), ]

# adding labels to the groups
# controlled_group$group <- 1
# error_bar$group <- 2
# shaded_bar$group <- 3
# fanchart_slice$group <- 4
# bell_curve$group <- 5


# remove unnecessary columns
controlled_group = controlled_group[,grepl("^(Q3|Q4|Q5)",names(controlled_group))|names(controlled_group) %in% c("Duration (in seconds)", "group")]
error_bar = error_bar[,grepl("^(Q3|Q6|Q7)",names(error_bar))| names(error_bar) %in% c("Duration (in seconds)", "group")]
shaded_bar = shaded_bar[,grepl("^(Q3|Q8|Q9)",names(shaded_bar))| names(shaded_bar)%in% c("Duration (in seconds)", "group")]
fanchart_slice = fanchart_slice[,grepl("^(Q3|Q10|Q11)",names(fanchart_slice))| names(fanchart_slice) %in% c("Duration (in seconds)", "group")]
bell_curve = bell_curve[,grepl("^(Q3|Q12|Q13)",names(bell_curve))| names(bell_curve) %in% c("Duration (in seconds)", "group")]
# controlled_group = controlled_group[,!sapply(controlled_group, function(x) mean(is.na(x)))>0.9]

group_list <- list(
  controlled = controlled_group,
  errorbar = error_bar,
  shadedbar = shaded_bar,
  fanchartslice = fanchart_slice,
  bellcurve = bell_curve
)


data_summary <- data.frame(
  Group = names(group_list),
  Duration_mean = NA_real_,
  Duration_SD = NA_real_,
  Age_mean = NA_real_,
  Age_SD = NA_real_,
  Gender_male = NA_real_,
  Gender_female = NA_real_,
  Highest_education_mean = NA_real_
)



for (i in seq_along(group_list)) {
  df <- group_list[[i]]
  group_name <- names(group_list)[i]
  # response duration
  durations <- as.numeric(df$`Duration (in seconds)`)/60 # Convert to minutes
  
  data_summary$Duration_mean[i] <- mean(durations, na.rm = TRUE)
  data_summary$Duration_SD[i] <- sd(durations, na.rm = TRUE)
  # age
  age <- as.numeric(gsub("[^0-9.]", "", df$Q3.2))
  data_summary$Age_mean[i] <- mean(age, na.rm = TRUE)
  data_summary$Age_SD[i] <- sd(age, na.rm = TRUE)
  # gender
  data_summary$Gender_male[i]<-sum(df$`Q3.3` == "1", na.rm = TRUE)/length(df$`Q3.3`)
  data_summary$Gender_female[i]<-sum(df$`Q3.3` == "2", na.rm = TRUE)/length(df$`Q3.3`)
  # highest education
  data_summary$Highest_education_mean[i]<-mean(as.numeric(df$`Q3.4`[df$`Q3.4` !="9"]),na.rm = TRUE)
  data_summary$Highest_education_SD[i]<-sd(as.numeric(df$`Q3.4`[df$`Q3.4` !="9"]),na.rm = TRUE)
  # prior trust of official data
  q3_11 <- as.numeric(df$`Q3.11`[df$`Q3.11` !="6"])
  data_summary$prior_trust_mean[i]<-mean(q3_11,na.rm = TRUE)
  data_summary$prior_trust_SD[i]<-sd(q3_11,na.rm = TRUE)
  
  data_summary$prior_trust_6[i]<-sum(df$`Q3.11` == "6", na.rm = TRUE)
}


# alternative way to run anova (EXCLUDE 6)
q3_11_table <- rawdata_value[rawdata_value$`Q3.11` != "6", c("Q3.11", "group")]
anova_test <- aov(Q3.11 ~ group, data = q3_11_table)
summary(anova_test)


q3_11_mean <- q3_11_table %>%
  group_by(group) %>%
  summarise(q3_11_mean_value = mean(as.numeric(Q3.11), na.rm = TRUE), .groups = 'drop')

# time duration ANOVA
time_duration_table <- rawdata_value[,c("Duration (in seconds)", "group")]
time_duration_table$`Duration (in seconds)` <- as.numeric(time_duration_table$`Duration (in seconds)`)/60
anova_test <- aov(`Duration (in seconds)` ~ group, data = time_duration_table)
summary(anova_test)
# post-hoc test
tukey_test <- TukeyHSD(anova_test)
# print(tukey_test)

time_duration_summary <- time_duration_table %>%
  group_by(group) %>%
  summarise(mean_duration = mean(`Duration (in seconds)`, na.rm = TRUE), 
            sd_duration = sd(`Duration (in seconds)`, na.rm = TRUE),
            .groups = 'drop')



# checking cleaned data
# totaleducation <- rawdata_value$`Q3.4`
# clean_edu<-as.numeric(totaleducation[totaleducation !="9"])



# Q3.5 Employee Status
# alternative way of Q3.5
q3_5_table <- rawdata_value[, c("Q3.5", "group")]
q3_5_contingency <- table(q3_5_table$Q3.5, q3_5_table$group)
# print(q3_5_contingency)
# Chi-squared test
chisq.test(q3_5_contingency)
q3_5_prop  <- as.data.frame(q3_5_contingency)
# rename columns
names(q3_5_prop) <- c("Q3.5", "Group", "Count")
# calculate proportions within each group
q3_5_prop <- q3_5_prop %>%
  group_by(Group) %>%
  mutate(Proportion = Count / sum(Count))




# Actual Survey
# ==============================================================================
# ==============================================================================
# categorical
# Qxx.2
# Have you ever been communicated this type of information before?
survey_q2 <- data.frame()
survey_q2_list <- list("Q4.2","Q6.2", "Q8.2", "Q10.2", "Q12.2")

for (i in seq_along(group_list)) {
  df <- group_list[[i]]
  col <- survey_q2_list[[i]]
  temp_table <- df[, c(col, "group")]
  names(temp_table) <- c("q2_value", "group")
  survey_q2 <- rbind(survey_q2, temp_table)
}

# recode the category
survey_q2 <- survey_q2 %>%
  mutate(q2_recode = recode(q2_value,
                            "1" = "n",
                            "2" = "n",
                            "3" = "y",
                            "4" = "y"))

survey_q2_contingency <- table(survey_q2$q2_recode, survey_q2$group)

ggplot(survey_q2, aes(x = q2_recode, fill = group)) +
  geom_bar(position = "dodge") +
  labs(title = "Have you ever been communicated this type of information before?",
       x = "Response",
       y = "Count") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


# chi-squared test
chisq.test(survey_q2_contingency)
# chisq_result <- chisq.test(survey_q2_contingency)
# chisq_result$stdres
# chisq_result$expected

# ==============================================================================
# definitive correct answer
# responses in descending order 
# Qxx.6
# what do you think is the probability of inflation being exactly 2.6%?
survey_q6 <- data.frame()
survey_q6_list <- list("Q4.6","Q6.6", "Q8.6", "Q10.6", "Q12.6")
for (i in seq_along(group_list)) {
  df <- group_list[[i]]
  col <- survey_q6_list[[i]]
  temp_table <- df[, c(col, "group")]
  names(temp_table) <- c("q6_value", "group")
  survey_q6 <- rbind(survey_q6, temp_table)
}
# force order
# survey_q6$group <- factor(survey_q6$group, levels = c("controlled", "errorbar", "shadedbar", "fanchartslice", "bellcurve"))

survey_q6_contingency <- table(survey_q6$q6_value, survey_q6$group)
# whether one proportion differs across groups
less_than_1_per <- survey_q6_contingency[7,]
survey_q6_group_total <- colSums(survey_q6_contingency)
prop.test(less_than_1_per, survey_q6_group_total)

# ==============================================================================
# definitive correct answer
# Qxx.14
# what do you think is  the probability of inflation exceeding 2%?
survey_q14 <- data.frame()
survey_q14_list <- list("Q4.14","Q6.14", "Q8.14", "Q10.14", "Q12.14")
for (i in seq_along(group_list)) {
  df <- group_list[[i]]
  col <- survey_q14_list[[i]]
  temp_table <- df[, c(col, "group")]
  names(temp_table) <- c("q14_value", "group")
  survey_q14 <- rbind(survey_q14, temp_table)
}

survey_q14_contingency <- table(survey_q14$q14_value, survey_q14$group)
chisq.test(survey_q14_contingency)

# whether one proportion differs across groups
around_60_per <- survey_q14_contingency[3,]
survey_q14_group_total <- colSums(survey_q14_contingency)
prop.test(around_60_per, survey_q14_group_total)
pairwise.prop.test(x = around_60_per, 
                   n = survey_q14_group_total,
                   p.adjust.method = "none") #?? different correction method!
#"holm" or "BH" or "bonferroni"




# ==============================================================================
# likert-scale - mean
# Qxx.17_1
# How likely do you think it is that the inflation will be exactly 2.6%?
survey_q17 <- data.frame()
survey_q17_list <- list("Q4.17_1","Q6.17_1", "Q8.17_1", "Q10.17_1", "Q12.17_1")
for (i in seq_along(group_list)) {
  df <- group_list[[i]]
  col <- survey_q17_list[[i]]
  temp_table <- df[, c(col, "group")]
  names(temp_table) <- c("q17_value", "group")
  survey_q17 <- rbind(survey_q17, temp_table)
}
survey_q17$q17_value <- as.numeric(survey_q17$q17_value)-8
# run annova
anova_test <- aov(q17_value ~ group, data = survey_q17)
summary(anova_test)


# ==============================================================================
# q3.5 employee status plot
ggplot(q3_5_prop, aes(x = as.factor(`Q3.5`), y = Proportion, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~ Group) +
  labs(
    title = "Employee Status",
    x = "Q3.5 Response",
    y = "Proportion"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Student",
    "2" = "Employed",
    "3" = "Self-employed",
    "4" = "Retired",
    "5" = "Unemployed",
    "6" = "Other"
  )) +
   scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# response duration plot
ggplot(data_summary, aes(x = Group, y = Duration_mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = Duration_mean - Duration_SD, ymax = Duration_mean + Duration_SD), width = 0.2) +
  labs(title = "Duration by Group",
       y = "Duration (in minutes)",
       x = "Group") +
  theme_minimal()

ggplot(time_duration_summary, aes(x = group, y = mean_duration)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = mean_duration - sd_duration, ymax = mean_duration + sd_duration), width = 0.2) +
  labs(title = "Duration by Group-alternative approach",
       y = "Duration (in minutes)",
       x = "Group") +
  theme_minimal()
