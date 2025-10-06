## ------------------------------------------------------------------------------------------------------------------------
#Load the built-in dataframe
data(mtcars)
df <- mtcars

str(df)
names(df)


## ------------------------------------------------------------------------------------------------------------------------
#Sort Row names in ascending order, A-Z
car_names <- rownames(mtcars)
order(car_names)
ordered_car_names <- order(car_names)
car_names[ordered_car_names]

sorted_names <- car_names[ordered_car_names]
sorted_names

df_sorted <- mtcars[ordered_car_names, ]


## ------------------------------------------------------------------------------------------------------------------------
#Identify the fourth and fourth to the last make and model names

rownames(df_sorted)[4]
rownames(df_sorted)[nrow(df_sorted) - 3]


## ------------------------------------------------------------------------------------------------------------------------
#Determine how many factor levels in cyl column

df$cyl <- factor(df$cyl)
levels(df$cyl)


## ------------------------------------------------------------------------------------------------------------------------
#Create separate dataframes for each cyl level

split_by_cyl <- split(df$mpg, df$cyl)
split_by_cyl


## ------------------------------------------------------------------------------------------------------------------------
#Calculate the average mpg for each cyl level rounding to 2 decimal

avg_mpg_by_cyl <- sapply(split_by_cyl, function(x) round(mean(x, na.rm = TRUE), 2))


avg_mpg_by_cyl



## ------------------------------------------------------------------------------------------------------------------------
#Determine how many factor levels are in transmission gear column

df$gear <- factor(df$gear)
df$am   <- factor(df$am, levels = c(0, 1), labels = c("automatic", "manual"))

levels(df$gear)
length(levels(df$gear))

levels(df$am)
length(levels(df$am))


## ------------------------------------------------------------------------------------------------------------------------
#Which transmission gear has lowest average hp? 

# Load df without factors
data(mtcars)
df <- mtcars

# Create a numeric matrix
hp_gear_am_matrix <- cbind(hp = df$hp, gear = df$gear, am = df$am)

is.matrix(hp_gear_am_matrix)
mode(hp_gear_am_matrix) 
head(hp_gear_am_matrix)

# Matrix back to a data frame
df_mean_hp <- as.data.frame(hp_gear_am_matrix)

# Mean horsepower by gear and am
mean_hp_by_gear_am <- aggregate(hp ~ gear + am, data = df_mean_hp, FUN = mean)

mean_hp_by_gear_am
mean_hp_by_gear_am$hp

# Lowest mean
lowest_hp_row <- mean_hp_by_gear_am[which.min(mean_hp_by_gear_am$hp), ]
lowest_hp_row

lowest_hp_gear <- lowest_hp_row$gear
lowest_hp_am   <- lowest_hp_row$am
lowest_hp_value <- lowest_hp_row$hp


## ------------------------------------------------------------------------------------------------------------------------
#Keep only automatic transmissions
automatic_cars <- df[df$am == 0, ]

#Create Factor
automatic_cars$gear <- factor(automatic_cars$gear)

levels(automatic_cars$gear)
length(levels(automatic_cars$gear))

the_number_of_transmission <- length(levels(automatic_cars$gear))


## ------------------------------------------------------------------------------------------------------------------------

#Keep only manual transmissions
manual_cars <- df[df$am == 1, ]

#Create Factor
manual_cars$gear <- factor(manual_cars$gear)

levels(manual_cars$gear)
length(levels(manual_cars$gear))

the_number_of_transmission_manual <- length(levels(manual_cars$gear))


## ------------------------------------------------------------------------------------------------------------------------
data(mtcars)
df <- mtcars

#Create a new column for efficiency
df$efficiency <- (df$hp / df$wt) * df$mpg
head(df$efficiency)




## ------------------------------------------------------------------------------------------------------------------------
#Create a new column for efficiency rate

df$efficiency_rate <- df$efficiency / df$qsec

head(df$efficiency_rate)


## ------------------------------------------------------------------------------------------------------------------------
#Which make and model has the highest efficiency?
max_efficiency <- max(df$efficiency)
highest_efficiency_row <- which.max(df$efficiency)
highest_efficiency_car <- rownames(df)[highest_efficiency_row]

highest_efficiency_car
max_efficiency

#Which make and model has the highest efficiency rate?
max_efficiency_rate <- max(df$efficiency_rate)
highest_efficiency_rate_row <- which.max(df$efficiency_rate)
highest_efficiency_rate_car <- rownames(df)[highest_efficiency_rate_row]

highest_efficiency_rate_car
max_efficiency_rate



## ------------------------------------------------------------------------------------------------------------------------
# create row for car name
df$car <- rownames(df)
head(df)

# scatter plot
plot(df$efficiency,
     df$efficiency_rate,
     main = "Efficiency vs Efficiency Rate",
     xlab = "Efficiency",
     ylab = "Efficiency Rate",
     pch = 20,
     col = "blue",
     #add space for car name
     xlim = c(0, max(df$efficiency) * 1.3),   
     ylim = c(0, max(df$efficiency_rate) * 1.3))

# label the top car
points(df$efficiency[highest_efficiency_rate_row],
       df$efficiency_rate[highest_efficiency_rate_row],
       col = "darkgreen", pch = 20, cex = 1.5)

text(df$efficiency[highest_efficiency_rate_row],
     df$efficiency_rate[highest_efficiency_rate_row],
     labels = highest_efficiency_rate_car,
     pos = 4, col = "darkgreen", cex = 1)



## ------------------------------------------------------------------------------------------------------------------------
#Convet file
knitr::purl("mtcars_dataframe.qmd", output = "mtcars_dataframe.R")

getwd()

