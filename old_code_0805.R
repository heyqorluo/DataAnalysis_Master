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


q3_11_values <- c()
q3_11_groups <- c()
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
  # append the values and group names to the vectors
  q3_11_values <- c(q3_11_values, q3_11)
  q3_11_groups <- c(q3_11_groups, rep(group_name, length(q3_11)))
  
  data_summary$prior_trust_6[i]<-sum(df$`Q3.11` == "6", na.rm = TRUE)
}

q3_11_anova <- data.frame(
  value = q3_11_values,
  group = factor(q3_11_groups))
anova_result <- aov(value ~ group, data = q3_11_anova)
summary(anova_result)

q3_11_mean_summary <-q3_11_anova %>%
  group_by(group) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

# alternative way to run anova (EXCLUDE 6)
q3_11_table <- rawdata_value[rawdata_value$`Q3.11` != "6", c("Q3.11", "group")]
anova_test <- aov(Q3.11 ~ group, data = q3_11_table)
summary(anova_test)

# checking cleaned data
# totaleducation <- rawdata_value$`Q3.4`
# clean_edu<-as.numeric(totaleducation[totaleducation !="9"])



# Q3.5 Employee Status
q3_5_table <- NULL  
for (i in seq_along(group_list)) {
  df <- group_list[[i]]
  group_name <- names(group_list)[i]
  temp_table <- df %>%
    group_by(`Q3.5`) %>%
    summarise(!!group_name := n(), .groups = "drop")
  # mutate(proportion = count_2 / sum(count_2))
  
  if (is.null(q3_5_table)) {
    q3_5_table <- temp_table  # First group initializes the table
  } else {
    q3_5_table <- full_join(q3_5_table, temp_table, by = "Q3.5")
  }
}
# convert to long format for plotting
q3_5_long <- q3_5_table %>%
  pivot_longer(cols = -`Q3.5`, names_to = "Group", values_to = "Count")

# calculate proportion within each group
q3_5_long <- q3_5_long %>%
  complete(`Q3.5`, Group, fill = list(Count = 0))
q3_5_prop <- q3_5_long %>%
  group_by(Group) %>% # make sure it is the percentage of each group
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()


# alternative way of Q3.5
q3_5_table <- rawdata_value[, c("Q3.5", "group")]
q3_5_contingency <- table(q3_5_table$Q3.5, q3_5_table$group)
print(q3_5_contingency)
# Chi-squared test
chisq.test(q3_5_contingency)
prop_df  <- as.data.frame(q3_5_contingency)
# rename columns
names(prop_df) <- c("Q3.5", "Group", "Count")
# calculate proportions within each group
prop_df <- prop_df %>%
  group_by(Group) %>%
  mutate(Proportion = Count / sum(Count))


# q3.5 employee status plot
ggplot(q3_5_prop, aes(x = as.factor(`Q3.5`), y = Proportion, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
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
