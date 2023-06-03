

# Import R packages
install.packages("dplyr")
library(dplyr)
install.packages("magrittr")
library(magrittr)
install.packages("Rcpp")
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)

#setwd("C:\\Users\\pshiv\\Downloads\\Assignment1 Sumer2022-1\\Assignment1 Sumer2022")

# Read the file
df<-read.csv("C:\\Users\\pshiv\\Downloads\\Assignment1 Sumer2022-1\\Assignment1 Sumer2022\\healthcare_stroke_dataset.csv", TRUE, ",")

#return the first 5 rows of the dataset
head(df, 5)

# Task 1: Statistical Exploratory Data Analysis

#1-a Print the details of dataframe
print('Print the details of dataframe are:')
sprintf('No. of columns: %i', ncol(df))
sprintf('No. of rows: %i', nrow(df))
print('Column Names and their datatypes:')
sapply(df, class)
print('Display missing values in each column of dataframe:')
colSums(is.na(df))
print('Total missing values:')
sum(is.na(df))

str(df)

#1-b Find the number of rows and columns in dataset

rows<-nrow(df)
cat("Number of rows:", rows)
col<-ncol(df)
cat("\nNumber of colums:", col)

#1-c Print descriptive detail of a column in dataset
summary(df)

#1-d Find all the count of unique values for a 'avg_glucose_level' column in dataset
cat("No. of unique values of avg_glucose_level are:", length(unique(df[["avg_glucose_level"]])))
unique(df$avg_glucose_level)
#https://www.r-bloggers.com/2021/12/how-to-find-unique-values-in-r/#:~:text=Use%20the%20unique()%20function%20to%20retrieve%20unique%20elements%20from,duplicate%20elements%20and%20rows%20deleted.


#1-d Find all percentage of 'Residence_type' for all the values
proportions <- table(df$Residence_type)/length(df$Residence_type)
percentage <- proportions*100
print(percentage)
#https://stackoverflow.com/questions/42379751/how-do-i-find-the-percentage-of-something-in-r

# Task 2: Aggregation & Filtering & Rank
#Task 2-a: Find out the gender with largest number of records 
print('Task 2-a:')
library(dplyr)
freq <- max(table(df$gender))
category <- tail(names(sort(table(df$gender))),1)
sprintf('The gender with the largest no. of records is "%s" with %i records:', category, freq)
#https://stackoverflow.com/questions/12187187/how-to-retrieve-the-most-repeated-value-in-a-column-present-in-a-data-frame


#Task 2-b: Find out the total number of Residence_type "Urban" who are Male
print('Task 2-b:')
pns <- nrow(df[df$Residence_type == "Urban" & df$gender == "Male",])
sprintf('The total number of Residence_type "Urban" who are Male: %i', pns)


# Group by function for dataframe in R using pipe operator 
#2-c 1 question #Find the top 10 ages with highest av_glucose_level
print('Task 2-c 1:')
task2_c <- df %>% arrange(-avg_glucose_level)
print('Top 10 ages with highest avg_glucose_level:')
task2_c <- task2_c[1:10,]
task2_c %>% summarize(age, avg_glucose_level)


#2-d 2nd question top 10 ages with more number of strokes
print('Task 2-d:')   
#
#task2_d %>% group_by(age) %>% summarize(stroke)
#task2_d <- task2_d[1:10,]
task2_d <- df %>% arrange(-stroke)
print('Top 10 ages with more number of strokes:',)
task2_d <- task2_d[1:10,]
task2_d %>% summarize(age, stroke)


##TASK 3: VISUALIZATION

#task 3-a
#Create barplot showing gender with count with residence type
print('Task 3-a:')
library(ggplot2)
print('Gender count with residence type')
ggplot(data = df, aes(x=gender, fill = Residence_type)) + geom_bar(position = 'dodge') + geom_text(stat='count', aes(label=..count..), position = position_dodge(width = 0.9), vjust=-1)
#https://intellipaat.com/community/16343/how-to-put-labels-over-geombar-for-each-bar-in-r-with-ggplot2


#task 3-b
#Display pie chart for the smoking state
print('Task 3-b')
print('pie chart for the smoking status data:')
task_3_b<- data.frame(df %>% group_by(smoking_status) %>% summarise(number_of_rows = n()))
task_3_b
task_3_b<- top_n(task_3_b,10)
pie(task_3_b$number_of_rows, labels = paste(task_3_b$smoking_status, sep = " "), 
    col = rainbow(length(task_3_b$smoking_status)),
    main = "smoking_status")




#Task4 finding an interesting pattern
# atleast two visualization with explanation
print('Task 4: Relationship between Age and BMI value:')
task_4_a <- df[c("id", "age", "heart_disease", "hypertension", "smoking_status", "bmi")]
task_4_a <- na.omit(task_4_a)
task_4_a <- task_4_a %>% mutate(bmi_data = cut(bmi, breaks = c(0,18.5,24.9,29.9, Inf), labels = c("<=18.5 Underweight", "18.5-24.9 Normal", "25-29.9 Pre-obese", ">30 obese")))
task_4_a <- task_4_a %>% mutate(age_data = cut(age, breaks = c(0,18,40,60,80, Inf), labels = c("<=18", "18-40", "40-60", "60-80", ">80")))
library(ggplot2)
print("bmi range for each age group")
#Bar Plot
bar_plot <- ggplot(data=task_4_a, aes(x = age_data, fill = bmi_data, width = 1)) + geom_bar(position = 'dodge', color = "black") + scale_fill_manual(values = c("#97C1A9", "#9AB7D3", "#FFB8B1", "#55CBCD")) + geom_text(stat='count', aes(label=..count..), colour = "black", size=3, position=position_dodge(width=0.9), vjust=-1) 
bar_plot + theme_dark() + labs(title = "BMI classification according to age group", subtitle = "Relationship by BMI range")

# Scatter Plot
scatter_plot <- ggplot(task_4_a, aes(x = age, y=bmi)) + 
  geom_point(aes(color = factor(bmi_data)), size = 1) + 
  scale_x_continuous(limits = c(0, max(task_4_a$age)), breaks = seq(0, max(task_4_a$age), 10)) +
  scale_y_continuous(limits = c(0, max(task_4_a$age)), breaks = seq(0, max(task_4_a$age), 10)) +
  geom_smooth(formula = y ~ x,
              method = "loess",
              colour = "black",
              span = 0.5,
              col = "#EAEAEA",
              se = FALSE,
              size = 1)

scatter_plot + theme_light() + scale_color_manual(values = c('#CD978B','#55CBCD', '#DFCCF1', "#97C1A9")) +
  labs(
    title = "Scatter Plot between age and bmi",
    substitle = "Relationship by bmi range")
