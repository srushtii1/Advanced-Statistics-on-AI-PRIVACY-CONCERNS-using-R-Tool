# Loading dataset 
df <- read.csv("C:/Users/ASUS/Downloads/AI and privacy concerns.csv")
View(df)

head(df) 
dim(df) 
tail(df)
str(df)
#================================================================================
# Install and load the dplyr package if not already installed
# install.packages("dplyr")
library(dplyr)
# Columns to drop (replace these with the actual column names you want to remove)
columns_to_drop <- c("Timestamp", "Name","Email.Id","")

# Remove specified columns
df <- df %>%
  select(-one_of(columns_to_drop))

# View the modified dataset
print(df)
View(df)

#================================================================================
  
colnames(df)
sapply(df, class) 
summary(df)
is.na(df) 
#===============================================================================

#descriptive statistics

mean(df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.)
median(df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.)
max(df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.)
min(df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.)

#measures o dispersion

range(df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.)
var(df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.)
sd(df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.)
summary(df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.)

#===============================================================================
# #label enconding 
# 
# 
# # Read the CSV file
# your_data <- read.csv("College Student Survey (Responses) - Form responses 1 (4).csv")
# your_data
# # Assuming "Category" is the column containing string categories
# your_data$Major.Field.of.Study <- as.integer(factor(your_data$Major.Field.of.Study))
# 
# # Save the modified dataset back to a CSV file
# write.csv(your_data, "your_encoded_dataset.csv", row.names = FALSE)
# 
# head(your_data)

#===============================================================================
# Function to perform label encoding on a column
label_encode <- function(column) {
  as.numeric(factor(column, levels = unique(column)))
}

# Apply label encoding to selected columns (modify 'columns_to_encode' accordingly)
columns_to_encode <- c("Gender", "How.comfortable.are.you.with.the.idea.of.AI.systems.analyzing.and.interpreting.your.personal.data.to.provide.personalized.services.")

for (col in columns_to_encode) {
  df[[col]] <- label_encode(df[[col]])
}

# Print the encoded data
print(df)


#===============================================================================
#ANOVA TEST
#H0 : There is no significant difference in the level 
#     of concern about privacy among individuals with different education levels.

#H1 : There is a significant difference in the level 
#     of concern about privacy among individuals with different education levels.


individuals=df$On.a.scale.of.1.to.10..how.concerned.are.you.about.the.privacy.implications.of.artificial.intelligence.technologies.
education_level=df$What.is.your.highest.level.of.education..
table(individuals,education_level)
res=aov(individuals~education_level)
s=summary(res)
s
pval=s[[1]]$'Pr(>F)'[[1]]
if (pval>0.05){
  print("Accept H0")}else{
    print("Reject H0")}

#===============================================================================

#Hypothesis Test for Canteen Food Quality (Two-sample t-test):

Comfort=df$How.comfortable.are.you.with.the.idea.of.AI.systems.analyzing.and.interpreting.your.personal.data.to.provide.personalized.services.
Gender=df$Gender
# Two-sample t-test
table(Comfort,Gender)
result_ttest <- t.test(Comfort~Gender)
result_ttest
if (result_ttest$p.value >= 0.05){
  print("Accept the null hypothesis")
}else{
  print("Reject the null hypothesis")
}

#==============================================================================

#Chi square test
#H0: There is no association between the level of control individuals
#    want over their data and their support for 	stricter regulations on AI.
#H1: There is an association between the level of control individuals
#    want over their data and their support for stricter regulations on AI.

Control=df$To.what.extent.do.you.believe.individuals.should.have.control.over.the.collection.and.use.of.their.personal.data.by.AI.systems.
regulation=df$In.your.opinion..should.there.be.international.standards.for.AI.to.address.privacy.concerns..or.should.regulations.be.left.to.individual.countries.
table(Control,regulation)
chi_square_res<-chisq.test(Control,regulation,correct = FALSE)
print("Chi_square_test_results:")
print(chi_square_res)
if (chi_square_res$p.value >= 0.05) {
  print("Accept the null hypothesis")
} else {
  print("Reject the null hypothesis")
}

#===============================================================================

