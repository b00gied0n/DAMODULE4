#########################################################################################
# 0. Packages
#########################################################################################
# WORKING DIRECTORY LOCATION
getwd()


# Install
install.packages("ggplot2")


# Make Available (or Call up)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)




#########################################################################################
# 1. Import
#########################################################################################
# Import CSV
cars <- read_csv("cars.csv")


# Import Excel
carInfo <- read_excel("cars.xlsx")
Hotels <- read_excel("Case Study Hotels.xlsx", sheet = "Hotels_Original")
Meals <- read_excel("Case Study Hotels.xlsx", sheet = "Meal Codes")


# Change from TIBBLE to DF
Hotels <- as.data.frame(Hotels)


# Open Path
newhouses <- read_csv(file.choose())
df <- read_excel(file.choose())


# Export into CSV
write_csv(mtcars, path = "Motorvehicle.csv")


# Export into Excel
write.xlsx(mtcars, file = "Motorvehicle.xlsx", sheetName = "vehicles")




#########################################################################################
# 2. Data Quality
#########################################################################################
# Inspection
head(mtcars) #Returns the first six records, this is the default behaviour
tail(mtcars) #Returns the last six records, , this is the default behaviour
head(mtcars, n=5) #Returns the first five records

# Information
str(Hotels)

# Statistics
summary(Hotels)

# Dupes
# Indicate if any duplicate data is present in the dataset
sum(duplicated(Hotels))


# Missing Data
# Count of NAs
sum(is.na(CustomerInfo3))

# Identify the columns that contains the NAs
colSums(is.na(Hotels_Dist))

# ORDER by Function
x[order(x, decreasing = TRUE)] #vector
mtcars[order(mtcars$hp, decreasing = TRUE), ] #dataframe




#########################################################################################
# 3. Data Cleansing
#########################################################################################
# Duplicated Data - Remove
Hotels_Dist <- 
  distinct(Hotels)


# Remove all na's and UPDATE
Hotels_Cleaned <-
  Hotels_Dist %>% drop_na()


# Replace NA observations with specific values (this is also known as interpolation/imputation)
SalesInfoMarch %>% replace_na(list(Profit = 0))
CustomerInfo3 %>% replace_na(list(Location = "unknown",TotalSales = mean(CustomerInfo3$TotalSales, na.rm = TRUE)))


# JOINS + UNIONS
CustomerInfo %>% 
  inner_join(OrderInfo, by = c("CustomerID" = "CusID"))

CustomerInfo %>% 
  left_join(OrderInfo, by = c("CustomerID" = "CusID"))


union(CustomerInfo, CustomerInfo2)


# DATA TYPE CHANGE
Hotels_Cleaned$reservation_status <- as.factor(Hotels_Cleaned$reservation_status)
vehicle$Cylinders <- as.integer(vehicle$Cylinders)


# Filtering General
mtcars[1:8, c("mpg","hp")]
mtcars[mtcars$hp > 200, c(1,4,2)]


# DPLYR - Filtering and Ordering
mtcars %>% 
  select(everything()) %>%
  filter(hp>200) %>% 
  arrange(desc(hp)) %>% 
  slice(1:5)


# Using AND
mtcars %>% 
  select(model,mpg,cyl,hp) %>% 
  filter(hp>200 & mpg>10) %>% 
  arrange(desc(hp) & desc(mpg)) %>% 
  slice(1:5)


# Grouping / Distributiongs / Statistics
Hotels_Merged %>% 
  select(everything()) %>% 
  group_by(meal_description) %>% 
  summarise(meanadr = mean(adr),rowItems=n())


# example 2
HousePrices %>%
  select(HouseSqft,Taxes,Bedrooms,LastSoldPrice) %>%
  group_by(Bedrooms) %>%
  summarise(meanLastSoldPrice = mean(LastSoldPrice),medianLastSoldPrice = median(LastSoldPrice))


# Dummy Flags (ABC -> #)
# which will create "n-1" columns for a categorical variable with "n" unique levels.
dummyinfo <- dummyVars(" ~ .", data = HousePrices, fullRank = T)


# The output of the dummyVars() function is used to generate the encoded data
# for this activity predict() function is used
# finally encoded data is converted into a dataframe
HousePrices <- data.frame(predict(dummyinfo, newdata = HousePrices))


# Grouping / Binning Data (# -> ABC)
HousePrices <- HousePrices %>% 
  mutate(Taxbin = cut(x = Taxes, breaks = c(0,1500,5000), labels = c("low","high")))


# With this you can remove the column and store variable into another variable.
head(Hotels)
df = subset(Hotels, select = -c(reservation_status))
df <- subset( data, select = -c(column1, column2 ))
head(df)



#########################################################################################
# 4. EDA
#########################################################################################
#1. Statistics - can also use summary()

# Get Stats
mean(cars$mpg)
sum(cars$mpg)
max(cars$mpg)
min(cars$mpg)
IQR(cars$mpg)

# tables or counts
table(cars$Transmission)
table(cars$Transmission, cars$Cylinders)


# 2. Distribution
# Grouping / Distributiongs / Statistics
mtcars %>% 
  select(everything()) %>% 
  filter(hp>200 | mpg>10) %>%
  group_by(cyl) %>% 
  summarise(meanMPG = mean(mpg), medianMPG = median(mpg), rowItems=n())

# example 2
HousePrices %>%
  select(HouseSqft,Taxes,Bedrooms,LastSoldPrice) %>%
  group_by(Bedrooms) %>%
  summarise(meanLastSoldPrice = mean(LastSoldPrice),medianLastSoldPrice = median(LastSoldPrice))



# 3. Correlation
# Creating the correlation coefficient for different pairings of numerical data
cor(x = cars$hp, y = cars$mpg)
cor(cars[,4:6])




#########################################################################################
# 4. Visualisation
# i. SCATTER
plot(x = mtcars$wt,
     y = mtcars$mpg,
     xlab = "Weight",
     ylab = "Milage Per Gallon",
     xlim = c(0,10),
     ylim = c(0,40),		 
     main = "Weight vs MPG")

# To add the trend line
fit <- lm(mpg ~ wt, data = mtcars)
abline(reg = fit, col = "blue", lwd=3)




# ii. HISTOGRAM
hist(x = mtcars$mpg,
     main = "Car MPG Distribution",
     xlab = "Weight",
     col = "green",
     border = "black",
     breaks = 5)



# iii. BOXPLOT
boxplot(mtcars$mpg)

boxplot(mpg ~ cyl, 
        data = mtcars, 
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", 
        main = "Car Mileage Analysis",
        col= c("red","green","blue"))



# iV. LINE 
plot(ap, type = "l")
plot(ap, type = "l", main = 'AirPassengers')


# v. GG PAIRS
ggpairs(HousePrices, title = "Correlation Matrix")



####################
# 5. Machine Learning
####################