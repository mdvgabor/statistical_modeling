setwd("~/gabor/Egyetem/4. Félév/Statistical Modeling/rscriptek")
library(readxl)
library(psych)
library(ggplot2)

BP_Flats <- read_excel("BP_FLats.xlsx")

# select rows 3 to 20 for columns 2 to 5
BP_Flats[3:20, 2:5]

# select rows 3 to 20 for columns 2, 5 and 8
BP_Flats[3:20, c(2,5,8)]

# Select flats that are more expensive than 15 million HUF
# and show only the Price and Area columns
BP_Flats[BP_Flats$Price_MillionHUF > 15, c("Price_MillionHUF", "Area_m2")]

# select the flats that are more expensive than 15 million and
# smaller than 50 m2
BP_Flats[BP_Flats$Price_MillionHUF > 15 & BP_Flats$Area_m2 < 50,]

# select all the rows of the first 2 columns
BP_Flats[, 1:2]

BP_Flats$Terrace <- as.integer(BP_Flats$Terrace)

# apply functions in R
# they apply a user defined function on a vector and return the result
mean(BP_Flats$Price_MillionHUF)
result <-apply(BP_Flats, 1, mean) #calculates mean for the rows
result <-apply(BP_Flats, 2, mean) # calculates mean for the columns

# matrix in R --> data frame of the same datatype
result_integer <- apply(BP_Flats[4:7],2, as.integer)

result_integer_v2 <- as.data.frame(result_integer)

#or we can do this
BP_Flats[,4:7] <- apply(BP_Flats[,4:7],2,as.integer)

# Python's boolean = R's logical (TRUE/FALSE)
BP_Flats$IsSouth <- as.logical(BP_Flats$IsSouth)
BP_Flats$IsBuda <- as.logical(BP_Flats$IsBuda)

# Python's string = R's character
BP_Flats$District <- as.character(BP_Flats$District)

# ------------------------------------------------------------------------------
# Analyzing with Descriptive Statistics
summary(BP_Flats)

# character (string) datatype: assuemes that data FREE TEXT
# Districts have PREDEFINED CATEGORIES --> FACTOR datatype
BP_Flats$District <- as.factor(BP_Flats$District)

# Exact Freq Distribution
table(BP_Flats$District)

# Calc Relative Freq
prop.table(table(BP_Flats$Floor))

# basic plots
hist(BP_Flats$Price_MillionHUF)
# by defult uses Sturge's Rule, but corrects it to have "nice boundaries"

boxplot(BP_Flats$Price_MillionHUF)

boxplot(BP_Flats$Price_MillionHUF ~ BP_Flats$IsBuda)

by(BP_Flats$Price_MillionHUF, BP_Flats$IsBuda, summary) #returns a table

# ------------------------------------------------------------------------------
# ggplot2 package
# every plot has axes: 'x' and 'y' and it has
# some shapes with x and y coordinates
# shapes have colours and sizes
# all these things can be data dependent

# all these parts of the plot are called LAYERs

# HISTOGRAM with ggplot

# x axis = variable "data"
# y axis = frequencies
# shape  = binned column

ggplot(data = BP_Flats,mapping = aes(x = Price_MillionHUF)) +
  geom_histogram(binwidth = 20, aes(fill = ..count..), color = "red") +
  scale_fill_gradient(name = "Frequency", low = "yellow", high = "red") +
  theme_minimal()

# Boxplot in ggplot
ggplot(data = BP_Flats, mapping = aes(y = Price_MillionHUF, fill = IsBuda)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20,face = "bold"), color = "pink") + 
  labs(y="Price (Million HUF)", fill="Is it in Buda?",
       title = "My most beautiful Plot")
        
# Stacked Column Plot --> 2 nominal variables
ggplot(data = BP_Flats, mapping = aes(x = IsBuda, fill = IsSouth)) +
  geom_bar(position = "fill")


table(BP_Flats[,c(8,9)]) # joint freq distribution
prop.table(table(BP_Flats[,c(8,9)])) # everything is % of total
prop.table(table(BP_Flats[,c(8,9)]),1) # % of row totals
prop.table(table(BP_Flats[,c(8,9)]),2) # % of column totals (on the plot)


# Scatter Plot: Price + Area (2 numeric variables)
ggplot(data = BP_Flats, mapping = aes(x = Area_m2, y = Price_MillionHUF)) +
  geom_point() +
  geom_smooth(method = lm)

# correlation: positive + strong
cor(BP_Flats[,1:2])






