library(tidyverse)
library(readxl)

setwd("~/Second Year/Stats/R_Directory")

gumtree <- read_excel("Gumtree_dogs.xlsx")

### ---- Cleaning ----

  # Price
table(gumtree$price)
gumtree$price[gumtree$price == "NA"] <- NA
table(gumtree$price)

class(gumtree$price)

gumtree$price <- as.numeric(gumtree$price)

# Checking plot of price to pass stupidity test:
ggplot(gumtree, aes(price)) +
  geom_histogram(col = "black", fill = "Light Blue")

#The prices displayed in the histogram seem reasonable, as dogs can be very expensive, perhaps up to $10000, but the majority are in the more affordable range.
#To be sure, we check the summary statistics:

summary(gumtree$price)
#Seems reasonable.

  # Cross
table(gumtree$cross)
gumtree$cross[gumtree$cross == "NA"] <- NA
table(gumtree$cross)

class(gumtree$cross)

gumtree$cross <- as.factor(gumtree$cross)

  # Pet offered by
table(gumtree$`Pet Offered By:`)
gumtree$`Pet Offered By:`[gumtree$`Pet Offered By:` == "NA"] <- NA
table(gumtree$`Pet Offered By:`)

class(gumtree$`Pet Offered By:`)

gumtree$`Pet Offered By:` <- as.factor(gumtree$`Pet Offered By:`)

# Checking swummary statistics of Pet Offered By to make sure everything in order:
summary(gumtree$`Pet Offered By:`)


  # Microchipped
table(gumtree$micro)
gumtree$micro[gumtree$micro == "NA"] <- NA
table(gumtree$micro)

class(gumtree$micro)

gumtree$micro <- as.factor(gumtree$micro)

# Checking swummary statistics of microchipped to make sure everything in order:
summary(gumtree$micro)

  # Vaccinated
table(gumtree$vacc)
gumtree$vacc[gumtree$vacc == "NA"] <- NA
table(gumtree$vacc)

class(gumtree$vacc)
#### Check if Anthony converted to factor!
subset(gumtree, !is.na(gumtree$vacc))

  # Desexing Status 
table(gumtree$desex)
gumtree$desex[gumtree$desex == "NA"] <- NA
table(gumtree$desex)

class(gumtree$desex)
#### Check if Anthony converted to factor!
subset(gumtree, !is.na(gumtree$desex))

  # Relinquished
table(gumtree$relinquished)
gumtree$relinquished[gumtree$relinquished == "NA"] <- NA
table(gumtree$relinquished)

class(gumtree$relinquished)

gumtree$relinquished <- as.factor(gumtree$relinquished)

  #Age
table(gumtree$age)
gumtree$age[gumtree$age == "NA"] <- NA
table(gumtree$age)

class(gumtree$age)

gumtree$relinquished <- as.factor(gumtree$relinquished)


# Checking plot of age to pass stupidity test:
ggplot(gumtree, aes(age)) +
  geom_histogram(col = "black", fill = "Light Blue")

#This doesn't look right, check summary:

summary(gumtree$age)

#How bizzare, at least on dog is less than 0 years old, and at least one is 545.8 years old
#Lets filter out those in unreasonable age ranges

gumtree %>%
  filter(age>25) %>%
  select(age)

#One dog is over 25 years of age, and it seems to be 545 years old.

gumtree %>%
  filter(age<0) %>%
  select(age)

#several dogs are less than 0 years old.

#since we have the go ahead from the researcher that these values are wrong, we will remove them from the dataset:
gumtree <- gumtree %>%
  filter(age>0, age<26)

#now we'll check the summary again
summary(gumtree$age)

#These are much more reasonable values.


