# Rohit Gupta CSIR-IGIB : 
#Bench Marking Script : 

library(ggplot2)

#setwd("C:/Users/user/Desktop/vilompad_output_organisms/")
setwd("/Users/rohitgupta/documents/vilompad_tool/vilompad_output_organisms/")
list.files()

data_benchmark <- read.csv("vilompad_results_benchmarking.csv")

data_benchmark_df <- as.data.frame(data_benchmark)
data_benchmark_df$Mismatch.Value <- NULL

View(data_benchmark_df)

# 2: Using Hue
#ggplot(data_benchmark_df, aes(x=as.factor(Organism), fill=as.factor(Organism))) + 
#  geom_bar( ) +
#  scale_fill_hue(c = 40) +
#  theme(legend.position="none")

# library(scales)
# 
# p <- ggplot(data_benchmark_df, aes(x=Organism, y=Number.of.Genomic.Palindromes.Detected),fill=Organism) + 
#   geom_bar(stat = "identity", width = 0.5)+ scale_fill_manual(values = c("red", "blue", "green"))
#    
# q <- p + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
# 
# q


library(scales)

p <- ggplot(data_benchmark_df, aes(x = Organism, y = Number.of.Genomic.Palindromes.Detected)) + 
  geom_bar(aes(fill = Organism), stat = "identity", width = 0.5, position = "identity") +
  scale_fill_manual(values = c("#F4A582", "light blue", "light green","#4393C3"))

q <- p + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  geom_text(aes(label = Number.of.Genomic.Palindromes.Detected), position = position_stack(vjust = 0.5))  
  

r <- q + ggtitle("Total number of direct palindromes for each palindromes (mismatch = 0) by vilompad")

r


###########################
# Time vs Organism vilompad plot : 

a <- ggplot(data_benchmark_df, aes(x = Organism, y = Time..in.seconds.)) + 
  geom_bar(aes(fill = Organism), stat = "identity", width = 0.5, position = "identity") +
  scale_fill_manual(values = c("#F4A582", "light blue", "light green","#4393C3"))

b <- a + scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  geom_text(aes(label = Time..in.seconds.), position = position_stack(vjust = 0.5))  


z <- b + ggtitle("Time taken by Vilompad to compute palindromes (mismatch = 0) in seconds")

z
###########################
# Line plot : 

w <- ggplot(data=data_benchmark_df, aes(x=Organism, y=Time..in.seconds., group=1)) +
  geom_line()+
  geom_point()

v <- w + ggtitle("Time taken by Vilompad to compute palindromes (mismatch = 0) in seconds")

v

#########################
## Visualizing data for mismatch one : 

data_benchmark_one <- read.csv("vilompad_results_benchmarking_mismatch_one.csv")

data_benchmark_one_df <- as.data.frame(data_benchmark_one)
data_benchmark_one_df$Mismatch.Value <- NULL

View(data_benchmark_one_df)

data_merge <- merge(data_benchmark_df,data_benchmark_one_df,by="Organism")

# Convert data from wide to long form : 

library(data.table)
wide_df <- melt(setDT(wide), id.vars = c("Organism","Time..in.seconds..x","Time..in.seconds..y","Number.of.Genomic.Palindromes.Detected.x","Number.of.Genomic.Palindromes.Detected.y"), variable.name = "Time")

library(tidyr)

# Create a grouped bar plot using ggplot2 without converting the dataframe to long format
ggplot(data_merge, aes(x = Organism)) +
  geom_bar(aes(y = Time..in.seconds..x, fill = "Time..in.seconds..x"), position = position_dodge(width = 0.9)) +
  geom_bar(aes(y = Time..in.seconds..y, fill = "Time..in.seconds..y"), position = position_dodge(width = 0.9)) +
  scale_fill_manual(name = "Variable", values = c("Time..in.seconds..x" = "red", "Time..in.seconds..y" = "blue")) +
  labs(x = "Organism", y = "Time in Seconds", title = "Grouped Bar Plot")


###############

ggplot(data_merge, aes(x = Organism)) +
  geom_col(aes(y = Time..in.seconds..x, fill = "Time..in.seconds..x"), position = position_dodge2(width = 0.9)) +
  geom_col(aes(y = Time..in.seconds..y, fill = "Time..in.seconds..y"), position = position_dodge2(width = 0.4)) +
  scale_fill_manual(name = "Variable", values = c("Time..in.seconds..x" = "red", "Time..in.seconds..y" = "blue")) +
  labs(x = "Organism", y = "Time in Seconds", title = "Grouped Bar Plot")

#############

library(ggplot2)
library(tidyr)

# create data frame with data in wide format
data <- data.frame(Category = c("A", "B", "C"), 
                   Group1 = c(10, 20, 30), 
                   Group2 = c(15, 25, 35))

# reshape data from wide to long format
data_long <- gather(data_merge, key = "Time", value = "Organism", -Category)

# create side by side barplot using ggplot2
ggplot(data_long, aes(x = Category, y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge")

#############

library(ggplot2)

# create data frame with data in wide format
data <- data.frame(Category = c("A", "B", "C"), 
                   Group1 = c(10, 20, 30), 
                   Group2 = c(15, 25, 35))

# convert data to long format and add group column
data_long <- reshape2::melt(data, id.vars = "Category", variable.name = "Group")

# create side by side barplot using ggplot2
ggplot(data_merge, aes(x = Organism, y = value, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge")

#########################
library(tidyr)

# create example wide-form dataframe
df_wide <- data.frame(id = 1:3, var1 = c(4, 5, 6), var2 = c(7, 8, 9))

# convert to long-form dataframe
df_long <- pivot_longer(df_wide, cols = c("var1", "var2"), names_to = "variable", values_to = "value")

# view the resulting long-form dataframe
df_long

#######################

# convert to long-form dataframe
df_long <- pivot_longer(data_merge, cols = c("Number.of.Genomic.Palindromes.Detected.x","Time..in.seconds..x","Number.of.Genomic.Palindromes.Detected.y","Time..in.seconds..y"), names_to = "variable", values_to = "value")

# view the resulting long-form dataframe
df_long

######################
# Plot : 
# create side by side barplot using ggplot2 : 

ggplot(df_long, aes(x = Organism, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") 

####################

data_merge_copy <- data_merge

data_merge_copy$Number.of.Genomic.Palindromes.Detected.x <- NULL
data_merge_copy$Number.of.Genomic.Palindromes.Detected.y <- NULL

data_merge_copy

# convert to long-form dataframe
df_long_time <- pivot_longer(data_merge_copy, cols = c("Time_For_Mismatch_Zero_In_Sec","Time_For_Mismatch_One_In_Sec"), names_to = "variable", values_to = "value")

# view the resulting long-form dataframe
df_long_time

ggplot(df_long_time, aes(x = Organism, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_text(aes(label = value), position = position_stack(vjust = 0.5))+
  ggtitle("Time-wise comparison for Mismatches")

###################
# Number of genomic palindromes side by side stacked bar plot : 

data_merge_copy2 <- data_merge

data_merge_copy2$Time..in.seconds..x <- NULL
data_merge_copy2$Time..in.seconds..y <- NULL

View(data_merge_copy2)

# convert to long-form dataframe
df_long_time2 <- pivot_longer(data_merge_copy2, cols = c("Number.of.Genomic.Palindromes.Detected.x","Number.of.Genomic.Palindromes.Detected.y"), names_to = "variable", values_to = "value")

# view the resulting long-form dataframe
df_long_time2

ggplot(df_long_time2, aes(x = Organism, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_text(aes(label = value), position = position_stack(vjust = 0.5))+
  ggtitle("Number of palindromes-wise comparison for Mismatches")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
################################################################################
# Rectification of the plots : 
# Line plot : 

data_normalized <- read.csv("vilompad_results_benchmarking_normalize_values.csv")
data_normalized_df <- as.data.frame(data_normalized)

data_normalized_df <- arrange(data_normalized_df, desc(Normalize.Value))

e <- ggplot(data=data_normalized_df, aes(x=Organism, y=Normalize.Value, group=1)) +
  geom_line()+
  geom_point()+
  geom_text(aes(label=Genome.Length..in.mb.),hjust=1.1,vjust=1)

  
f <- e + ggtitle("Normalization with respect to number of palindromes with genome size in mb (mismatch = 0) vs organisms")

f
##################################################################################
## Split the files : 

fungi <- read.table("fungi_arabidopsis_thaliana_chromosome_3_result_mismatch_zero.txt", sep="\t", header=TRUE)

split_data <- split(fungi, rep(1:5, each=nrow(fungi)/5))

for (i in 1:5) {
  file_name <- paste0("data_split", i, ".txt")
  write.table(split_data[[i]], file_name, sep="\t", quote=FALSE, row.names=FALSE)
}

####### Number of total bases vs palindrome bases : 

updated_data <- read.csv("vilompad_benchmarking_results_updated.csv")
updated_data <- as.data.frame(updated_data)

updated_data$Mismatch.Value <- NULL
updated_data$Number.of.Genomic.Palindromes.Detected <- NULL
updated_data$Genome.Length..in.mb. <- NULL
updated_data$Time..in.seconds.<- NULL
updated_data$Normalize.Value <- NULL

# convert to long-form dataframe
updated_long <- pivot_longer(updated_data, cols = c("Total.Number.of.Palindrome.Bases","Total.Number.of.Bases.in.the.Genome"), names_to = "variable", values_to = "value")

# Arrange values in descending order : 
updated_long <- updated_long[order(updated_long$value, decreasing = TRUE),]

# Create plot : 
# Create stacked bar plot
v <- ggplot(updated_long, aes(x = Organism, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + ggtitle("Comparison between Total Number of Palindrome Bases vs Total Number of Bases in the Genome of the organism")

########################
# Combine the plots : number of palindromes and total time taken beside each other : 

updated_data_combine <- read.csv("vilompad_benchmarking_results_updated.csv")
updated_data_combine <- as.data.frame(updated_data_combine)

updated_data_combine$Mismatch.Value <- NULL
updated_data_combine$Genome.Length..in.mb. <- NULL
updated_data_combine$Normalize.Value <- NULL 
updated_data_combine$Total.Number.of.Palindrome.Bases <- NULL 
updated_data_combine$Total.Number.of.Bases.in.the.Organism <- NULL

# convert to long dataframe : 
# convert to long-form dataframe
updated_combine_long <- pivot_longer(updated_data_combine, cols = c("Number.of.Genomic.Palindromes.Detected","Time..in.seconds."), names_to = "variable", values_to = "value")

# Arrange values in descending order : 
#updated_long <- updated_long[order(updated_long$value, decreasing = TRUE),]

ggplot(updated_combine_long, aes(x = Organism, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_text(aes(label = value), position = position_stack(vjust = 0.5))+
  ggtitle("Comparison")

##############################################
## Make Boxplot : 

data_boxplots <- read.csv("data_boxplots.csv")
data_boxplots <- as.data.frame(data_boxplots)

box_sars <- ggplot(data_boxplots, aes(element, frequency))
box_sars + geom_boxplot()

#############################################
#data
combined_data_boxplots <- read.csv("combined_data_boxplots.csv")

# convert to long form : 

combined_data_boxplots_long <- pivot_longer(combined_data_boxplots, cols = c("sars_pal_length","sars_pal_frequency","adineta_pal_length","adineta_pal_frequency","fungi_pal_length","fungi_pal_frequency","salmonella_pal_length","salmonella_pal_frequency"), names_to = "variable", values_to = "value")

ggplot(combined_data_boxplots_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(x = "variable", y = "values") +
  scale_fill_manual(values = c("blue", "red","orange","yellow","brown","green","purple","grey"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  ylim(0, 500)


########################

combined_data_boxplots_length <- read.csv("combined_data_boxplot_length.csv")
combined_data_boxplots_frequency <- read.csv("combined_data_boxplot_frequency.csv")

combined_data_boxplots_length <- as.data.frame(combined_data_boxplots_length)
combined_data_boxplots_frequency <- as.data.frame(combined_data_boxplots_frequency)

# convert to long form : 
combined_data_boxplots_length_long <- pivot_longer(combined_data_boxplots_length, cols = c("sars_pal_length","adineta_pal_length","fungi_pal_length","salmonella_pal_length"), names_to = "variable", values_to = "value")

combined_data_boxplots_frequency_long <- pivot_longer(combined_data_boxplots_frequency, cols = c("sars_pal_frequency","adineta_pal_frequency","fungi_pal_frequency","salmonella_pal_frequency"), names_to = "variable", values_to = "value")


# plot : 
ggplot(combined_data_boxplots_length_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(x = "variable", y = "values") +
  scale_fill_manual(values = c("blue", "red","orange","yellow"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  ylim(0, 500)+ 
  geom_jitter(width = 0.2)


ggplot(combined_data_boxplots_frequency_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(x = "variable", y = "values") +
  scale_fill_manual(values = c("blue", "red","orange","yellow"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  ylim(0, 450)+ 
  geom_jitter(width = 0.2)

##################################################################

df <- read.table(text = "       Input Rtime Rcost Rsolutions  Btime Bcost 
1   12-proc.     1    36     614425     40    36 
2   15-proc.     1    51     534037     50    51 
3    18-proc     5    62    1843820     66    66 
4    20-proc     4    68    1645581 104400    73 
5 20-proc(l)     4    64    1658509  14400    65 
6    21-proc    10    78    3923623 453600    82",header = TRUE,sep = "")

dfm <- melt(df[,c('Input','Rtime','Btime')],id.vars = 1)

ggplot(dfm,aes(x = Input,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + 
  scale_y_log10()

##########################################################

# create sample data
set.seed(123)
organisms <- rep(c("Organism1", "Organism2", "Organism3"), each = 30)
lengths <- rep(c(10, 20, 30), each = 10, times = 3)
frequency <- rnorm(90, mean = 50, sd = 10)
df <- data.frame(organisms, lengths, frequency)

# load ggplot2 library
library(ggplot2)

# create boxplot
ggplot(df, aes(x = organisms, y = frequency, fill = factor(lengths))) +
  geom_boxplot() +
  labs(x = "Organisms", y = "Frequency", fill = "Palindrome Length") +
  theme_bw()

###########################################################

combined_length_frequency_data <- read.csv("combined_boxplot_length_frequency_data.csv")

combined_length_frequency_data <- as.data.frame(combined_length_frequency_data)

ggplot(combined_length_frequency_data, aes(x = organism, y = frequency)) +
  geom_bar() +
  labs(x = "Organisms", y = "Frequency") +
  theme_bw() + ylim(0,1000)

################################################################

library(ggplot2)

# create sample dataset
organism <- c("A", "A", "A", "B", "B", "B")
palindrome_length <- c(3, 4, 5, 3, 4, 5)
frequency <- c(10, 15, 8, 5, 20, 12)
df <- data.frame(organism, palindrome_length, frequency)

# create barplot
ggplot(df, aes(x = as.factor(palindrome_length), y = frequency)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ organism, ncol = 2)

###################################################
# create barplot
ggplot(combined_length_frequency_data, aes(x = as.factor(length), y = frequency)) +
  geom_violin() +
  facet_wrap(~ organism, ncol = 4) + ylim(0,500) 

####################################################
## create a violin plot for the same : 
library(ggplot2)

# create sample dataset
organism <- c("A", "A", "A", "B", "B", "B")
palindrome_length <- c(3, 4, 5, 3, 4, 5)
frequency <- c(10, 15, 8, 5, 20, 12)
df <- data.frame(organism, palindrome_length, frequency)

# create violin plot
ggplot(df, aes(x = as.factor(palindrome_length), y = frequency, fill = organism)) +
  geom_violin(scale = "count", draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_boxplot(width = 0.1, fill = "white") +
  scale_fill_manual(values = c("skyblue", "pink")) +
  labs(x = "Palindrome Length", y = "Frequency") +
  ggtitle("Palindrome Length vs. Frequency by Organism") +
  theme_bw()

##############################
library(ggplot2)

# create sample dataset
organism <- c("A", "A", "A", "B", "B", "B")
palindrome_length <- c(3, 4, 5, 3, 4, 5)
frequency <- c(10, 15, 8, 5, 20, 12)
df <- data.frame(organism, palindrome_length, frequency)

# create violin plot
ggplot(df, aes(x = as.factor(palindrome_length), y = frequency)) +
  geom_violin() +
  facet_wrap(~ organism, ncol = 2)

######################################
combined_df <- read.csv("combined_human_org_data_boxplot_length.csv")

combined_df <- as.data.frame(combined_df)

# convert to long form : 
combined_df_long <- pivot_longer(combined_df, cols = c("sars_pal_length","adineta_pal_length","fungi_pal_length","salmonella_pal_length","human_pal_length"), names_to = "variable", values_to = "value")

# Human plot as well : 
ggplot(combined_df_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(x = "variable", y = "values") +
  scale_fill_manual(values = c("blue", "red","orange","yellow","green"))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  ylim(0, 1000)+ 
  geom_jitter(width = 0.2) + 
  labs(y= "Palindrome Length", x = "Name of Organism")+ 
  labs(fill='Legend')

########################################################################
# install.packages("ggplot2")
library(ggplot2)

# Data 
set.seed(1)

age <- factor(sample(c("Child", "Adult", "Retired"),
                     size = 50, replace = TRUE),
              levels = c("Child", "Adult", "Retired"))
hours <- sample(1:4, size = 50, replace = TRUE)
city <- sample(c("A", "B", "C"),
               size = 50, replace = TRUE)

df <- data.frame(x = age, y = hours, group = city)

ggplot(df, aes(x = x, fill = group)) + 
  geom_bar()

###################################

stacked_data <- read.csv("vilompad_benchmarking_results_updated_human.csv")
stacked_data <- as.data.frame(stacked_data)


stacked_data$Mismatch.Value <- NULL
stacked_data$Number.of.Genomic.Palindromes.Detected <- NULL
stacked_data$Genome.Length..in.mb. <- NULL
stacked_data$Time..in.seconds.<- NULL
stacked_data$Normalize.Value <- NULL

# convert to long-form dataframe
stacked_data <- pivot_longer(stacked_data, cols = c("Total.Number.of.Palindrome.Bases","Total.Number.of.Bases.in.the.Genome"), names_to = "variable", values_to = "value")

# Arrange values in descending order : 
stacked_data <- stacked_data[order(stacked_data$value, decreasing = TRUE),]

# Create plot : 
# Create stacked bar plot
x <- ggplot(stacked_data, aes(x = Organism, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + ggtitle("Comparison between Total Number of Palindrome Bases vs Total Number of Bases in the Genome of the organism")

x

#################
stacked_data_updated <- read.csv("vilompad_benchmarking_results_updated_human_updated.csv")
stacked_data_updated <- as.data.frame(stacked_data_updated)


stacked_data_updated$Mismatch.Value <- NULL
stacked_data_updated$Number.of.Genomic.Palindromes.Detected <- NULL
stacked_data_updated$Genome.Length..in.mb. <- NULL
stacked_data_updated$Time..in.seconds.<- NULL
stacked_data_updated$Normalize.Value <- NULL

# convert to long-form dataframe
stacked_data_updated_long <- pivot_longer(stacked_data_updated, cols = c("Total.Number.of.Palindrome.Bases","Total.Number.of.Bases.in.the.Organism"), names_to = "variable", values_to = "value")

# Arrange values in descending order : 
stacked_data_updated_long <- stacked_data_updated_long[order(stacked_data_updated_long$value, decreasing = TRUE),]

# Create plot : 
# Create stacked bar plot
x <- ggplot(stacked_data_updated_long, aes(x = Organism, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + ggtitle("Comparison between Total Number of Palindrome Bases vs Total Number of Bases in the Genome of the organism")

x

############################
