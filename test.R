library(readr)
library(dplyr)
library(ggplot2)
exported_data <- read_csv("rawdata_value.csv")
exported_data <- subset(exported_data, select = -c(StartDate,EndDate,Status,IPAddress,Progress,Finished,RecordedDate, RecipientLastName,RecipientFirstName,
                                                   RecipientEmail,ExternalReference,LocationLatitude,LocationLongitude,DistributionChannel,UserLanguage))
rawdata_value <- subset(exported_data, select =-c(ResponseId,Q1.1,Q2.1))
rawdata_value <- rawdata_value[3:nrow(rawdata_value), ]

# filter treatment groups
controlled_group <- rawdata_value[!is.na(rawdata_value$Q4.2), ]
error_bar <-rawdata_value[!is.na(rawdata_value$Q6.2), ]
shaded_bar <-rawdata_value[!is.na(rawdata_value$Q8.2), ]
fanchart_slice <-rawdata_value[!is.na(rawdata_value$Q10.2), ]
bell_curve <-rawdata_value[!is.na(rawdata_value$Q12.2), ]

# summary statistics for controlled group (ultimately we want 6 frequency plot for all 5 groups+ can add them tgt??)
summary_table <- controlled_group %>%
  group_by(`Q3.8`) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# print(summary_table)
frequency_vs_decision= table(controlled_group$Q3.8, controlled_group$Q5.3)
