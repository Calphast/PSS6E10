library(tidyverse)

# Data intake
airline.test <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/test.csv")
airline.train <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/train.csv")
airline.submit <- read_csv("~/Coding/Kaggle/PSS6E10/PSS6E10/playground-series-s6e10/sample_submission.csv")

# Exploratory data analysis
## satisfaction itself related
satisfaction <- table(airline.train$satisfaction)
barplot(satisfaction)

## Gender related
gender <- table(airline.train$Gender)
barplot(gender)

gender.male <- subset(airline.train, Gender == "Male")
gender.female <- subset(airline.train, Gender == "Female")

# individual tests
gender.male.satisfaction <- table(gender.male$satisfaction)
gender.female.satisfaction <- table(gender.female$satisfaction)

gender.satisfactionmeasure <- matrix(c(gender.male.satisfaction["TRUE"], gender.male.satisfaction["FALSE"],
                                       gender.female.satisfaction["TRUE"], gender.female.satisfaction["FALSE"]),
                                     nrow = 2, byrow=TRUE)
rownames(gender.satisfactionmeasure) <- c("Male", "Female")
colnames(gender.satisfactionmeasure) <- c("True", "False")

gender.male.significance <- prop.test(gender.satisfactionmeasure["Male", "True"], 
                                      sum(gender.satisfactionmeasure["Male", ]), 
                                      p = 0.05,  alternative = "two.sided", correct = FALSE)

gender.female.significance <- prop.test(gender.satisfactionmeasure["Female", "True"], 
                                        sum(gender.satisfactionmeasure["Female", ]),
                                        p=0.05, alternative = "two.sided", correct = FALSE)

print(gender.male.significance)
print(gender.female.significance)
# No measurable diffence

## class related
class <- table(airline.train$Class)
barplot(class)

class.business <- subset(airline.train, Class == "Business")
class.eco <- subset(airline.train, Class == "Eco")
class.ecoplus <- subset(airline.train, Class == "Eco")

# satisfaction per class
class.business.satisfaction <- table(class.business$satisfaction)
class.eco.satisfaction <- table(class.eco$satisfaction)
class.ecoplus.satisfaction <- table(class.ecoplus$satisfaction)

barplot(class.business.satisfaction, main="Business Satisfaction")
barplot(class.eco.satisfaction, main="Eco Satisfaction")
barplot(class.ecoplus.satisfaction, main="Ecoplus Satisfaction")

# Difference in satisfaction between classes?
# Chi square for significance
class.satisfactionmeasure <-matrix(c(class.business.satisfaction["TRUE"], class.business.satisfaction["FALSE"],
                                     class.eco.satisfaction["TRUE"], class.eco.satisfaction["FALSE"],
                                     class.ecoplus.satisfaction["TRUE"], class.ecoplus.satisfaction["FALSE"]),
                                   nrow = 3, byrow = TRUE)
rownames(class.satisfactionmeasure) <- c("Business", "Eco", "Ecoplus")
colnames(class.satisfactionmeasure) <- c("True", "False")

chisq.test(class.satisfactionmeasure)

# Holm-Bonferroni correction
class.successes <- class.satisfactionmeasure[, "True"]
class.total <- rowSums(class.satisfactionmeasure)

class.post_hoc <- pairwise.prop.test(class.successes, class.total, p.adjust.method = "holm")
print(class.post_hoc) # Note that there is a difference between business vs eco/ecoplus

# Measuring each population (Business)
class.business.significance <- prop.test(x = class.satisfactionmeasure["Business", "True"], n=sum(class.satisfactionmeasure["Business", ]),
          p=0.05, alternative = "two.sided", correct = FALSE)

class.eco.significance <- prop.test(x = class.satisfactionmeasure["Eco", "True"], n=sum(class.satisfactionmeasure["Eco", ]),
                                         p=0.05, alternative = "two.sided", correct = FALSE)

class.ecoplus.significance <- prop.test(x = class.satisfactionmeasure["Ecoplus", "True"], n=sum(class.satisfactionmeasure["Ecoplus", ]),
                                         p=0.05, alternative = "two.sided", correct = FALSE)

print(class.business.significance)
print(class.eco.significance)
print(class.ecoplus.significance)

# Eco/ecoplus -> more likely to False, Business -> more likely to True

## Type of Travel related
travel_type <- table(airline.train$`Type of Travel`)
barplot(travel_type)

travel_type.business <- subset(airline.train, `Type of Travel` == "Business travel") 
travel_type.personal <- subset(airline.train, `Type of Travel` == "Personal Travel") 

# satisfaction per travel type
travel_type.business.satisfacton <- table(travel_type.business$satisfaction)
travel_type.personal.satisfacton <- table(travel_type.personal$satisfaction)

barplot(travel_type.business.satisfacton, main="Business Travel Satisfaction")
barplot(travel_type.personal.satisfacton, main="Personal Travel Satisfaction")

# Difference between travel types
travel_type.satisfactionmeasure <- matrix(c(travel_type.business.satisfacton["TRUE"], travel_type.business.satisfacton["FALSE"],
                                            travel_type.personal.satisfacton["TRUE"], travel_type.personal.satisfacton["FALSE"]),
                                          nrow = 2, byrow = TRUE)
rownames(travel_type.satisfactionmeasure) <- c("Business Travel", "Personal Travel")
colnames(travel_type.satisfactionmeasure) <- c("True", "False")

chisq.test(travel_type.satisfactionmeasure)

# Holm-Bonferroni correction
travel_type.successes <- travel_type.satisfactionmeasure[, "True"]
travel_type.total <- rowSums(travel_type.satisfactionmeasure)

travel_type.post_hoc <- pairwise.prop.test(travel_type.successes, travel_type.total, p.adjust.method = "holm")
print(travel_type.post_hoc) # Note that is a difference between Business vs Personal travel

# Individual tests
# Measuring each population (Business)
travel_type.business.significance <- prop.test(x = travel_type.satisfactionmeasure["Business Travel", "True"], n=sum(travel_type.satisfactionmeasure["Business Travel", ]),
                                         p=0.05, alternative = "two.sided", correct = FALSE)

travel_type.personal.significance <- prop.test(x = travel_type.satisfactionmeasure["Personal Travel", "True"], n=sum(travel_type.satisfactionmeasure["Personal Travel", ]),
                                               p=0.05, alternative = "two.sided", correct = FALSE)

print(travel_type.business.significance)
print(travel_type.personal.significance)

# Personal wayyyyy less significant, business not as much

# Age related 
hist(airline.train$Age)
boxplot(Age ~ satisfaction, data = airline.train,
        main = "Age vs Satisfied")

# Difference between groups
t.test(Age ~ satisfaction, data = airline.train, var.equal = TRUE) 
# very significant, people who are younger tend to dislike more.

# Flight Distance related
hist(airline.train$`Flight Distance`)
boxplot(`Flight Distance`~ satisfaction, data = airline.train)

# Difference between groupsd
t.test(`Flight Distance`~ satisfaction, data = airline.train, var.equal = TRUE)
# very significant, lower flight disnaces tend to dislike more

# Inflight wifi related
hist(airline.train$`Inflight wifi service`)
boxplot(`Inflight wifi service`~ satisfaction, data = airline.train)

t.test(`Inflight wifi service`~ satisfaction, data = airline.train, var.equal = TRUE)
# same as above, those lower inflight -> less satisfied

# departure/arrival time
hist(airline.train$`Departure/Arrival time convenient`)
boxplot(`Departure/Arrival time convenient`~ satisfaction, data = airline.train)

t.test(`Departure/Arrival time convenient`~ satisfaction, data = airline.train, var.equal = TRUE)
# those who are more likely to convinient arrive, less satisfied eh?

# ease of online booking
hist(airline.train$`Ease of Online booking`)
boxplot(`Ease of Online booking` ~ satisfaction, data = airline.train)

t.test(`Ease of Online booking` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# gate location
hist(airline.train$`Gate location`)
boxplot(`Gate location` ~ satisfaction, data = airline.train)

t.test(`Gate location` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# food and drink
hist(airline.train$`Food and drink`)
boxplot(`Food and drink` ~ satisfaction, data = airline.train)

t.test(`Food and drink` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# online boarding
hist(airline.train$`Online boarding`)
boxplot(`Online boarding` ~ satisfaction, data = airline.train)

t.test(`Online boarding` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# seat comfort
hist(airline.train$`Seat comfort`)
boxplot(`Seat comfort` ~ satisfaction, data = airline.train)

t.test(`Seat comfort` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# inflight entertainment
hist(airline.train$`Inflight entertainment`)
boxplot(`Inflight entertainment` ~ satisfaction, data = airline.train)

t.test(`Inflight entertainment` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# on-board service
hist(airline.train$`On-board service`)
boxplot(`On-board service` ~ satisfaction, data = airline.train)

t.test(`On-board service` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# leg room service
hist(airline.train$`Leg room service`)
boxplot(`Leg room service` ~ satisfaction, data = airline.train)

t.test(`Leg room service` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# baggage handling
hist(airline.train$`Baggage handling`)
boxplot(`Baggage handling` ~ satisfaction, data = airline.train)

t.test(`Baggage handling` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# checking services
hist(airline.train$`Checkin service`)
boxplot(`Checkin service` ~ satisfaction, data = airline.train)

t.test(`Checkin service` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# cleanliness
hist(airline.train$`Cleanliness`)
boxplot(`Cleanliness` ~ satisfaction, data = airline.train)

t.test(`Cleanliness` ~ satisfaction, data = airline.train, var.equal = TRUE)
# lower rating -> less satisfied

# departure delay in minutes
hist(log1p(airline.train$`Departure Delay in Minutes`))
boxplot(log1p(`Departure Delay in Minutes`) ~ satisfaction, data = airline.train)

t.test(log1p(`Departure Delay in Minutes`) ~ satisfaction, data = airline.train, var.equal = TRUE)
# higher time -> less satisfied

# arrival delay in minutes
hist(log1p(airline.train$`Arrival Delay in Minutes`))
boxplot(log1p(`Arrival Delay in Minutes`) ~ satisfaction, data = airline.train)

t.test(log1p(`Arrival Delay in Minutes`) ~ satisfaction, data = airline.train, var.equal = TRUE)
# higher time -> less satisfied

## gender related

