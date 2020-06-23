# Olga Loginova
# HarvardX: PH125.9x - Capstone Part 2
# CYO Project -  European Video Games Bestsellers

#### Introduction ####

################################################################
# Data Set
################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caretEnsemble)) install.packages("caretEnsemble", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if (!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if (!require(wordcloud)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")

# Video Games dataset:
# https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings

# Reading the data from the file
data <- read.csv("../CYO/Video_Games_Sales_as_at_22_Dec_2016.csv", stringsAsFactor = FALSE)

# Brief summary to get familiarised with the data set
nrow(data)
head(data)
duplicated(data) %>% 
  sum()
str(data)

# Converting the string vectors of Year_of_Release into numeric
data$Year_of_Release <- as.numeric(data$Year_of_Release)

# Removing lines with 0 sales in EU
data <- data %>% filter(EU_Sales != 0)

# Checking missing years
y_data <- data %>% filter(is.na(Year_of_Release))
head(y_data)

# Extracting Year_of_Release from Name
pattern <- "\\s(\\d{4})$"
for (i in which(y_data$Name %in% str_subset(y_data$Name, pattern))) 
  y_data[i, 3] <- as.numeric(str_sub(y_data[i, 1],-4,-1))

# Adding missing years to y_data
y_data[which(y_data$Name %in% c("Clockwork Empires", "Move Fitness")), 3] <- 2014
y_data[which(y_data$Name %in% c("Dead Island: Riptide", "Dead Space 3", "Tomb Raider (2013)")), 3] <- 2013
y_data[which(y_data$Name %in% c("Jewel Link Chronicles: Mountains of Madness")), 3] <- 2012
y_data[which(y_data$Name %in% c("LEGO Harry Potter: Years 5-7", "Happy Feet Two", "Test Drive Unlimited 2", 
                                "Battle vs. Chess", "The Lord of the Rings: War in the North", 
                                "Dream Trigger 3D", "Jonah Lomu Rugby Challenge",
                                "Rocksmith", "TERA", 
                                "The History Channel: Great Battles - Medieval", 
                                "Tropico 4")), 3] <- 2011
y_data[which(y_data$Name %in% c("Call of Duty: Black Ops", "Bejeweled 3", 
                                "Big Beach Sports 2", "BioShock 2", "Singularity", 
                                "Dance! It's Your Stage", "Ferrari: The Race Experience", 
                                "Get Fit with Mel B", "WRC: FIA World Rally Championship", 
                                "World of Tanks", "Yakuza 4")), 3] <- 2010
y_data[which(y_data$Name %in% c("Runaway: A Twist of Fate", "Wet")), 3] <- 2009
y_data[which(y_data$Name %in% c("Silent Hill: Homecoming", "LEGO Batman: The Videogame", 
                                "LEGO Indiana Jones: The Original Adventures", 
                                "Advance Wars: Days of Ruin", "Robert Ludlum's The Bourne Conspiracy", 
                                "Disgaea 3: Absence of Detention", "GRID", 
                                "PES 2009: Pro Evolution Soccer", "Shaun White Snowboarding", 
                                "Street Fighter IV")), 3] <- 2008
y_data[which(y_data$Name %in% c("Rock Band", "The Golden Compass", 
                                "Luxor: Pharaoh's Challenge", "Mountain Bike Adrenaline", 
                                "Shrek the Third", "Star Trek: Conquest")), 3] <- 2007
y_data[which(y_data$Name %in% c("Call of Duty 3", "Cabela's Alaskan Adventure", 
                                "Dragon Ball Z: Budokai Tenkaichi 2 (JP sales)", 
                                "Madden NFL 07", "Mega Man X Collection", 
                                "Nicktoons: Battle for Volcano Island", "Star Trek: Legacy", 
                                "Teen Titans", "Tom Clancy's Rainbow Six: Critical Hour")), 3] <- 2006
y_data[which(y_data$Name %in% c("Combat Elite: WWII Paratroopers", "College Hoops 2K6",
                                "The Chronicles of Narnia: The Lion, The Witch and The Wardrobe", 
                                "Disney's Cinderella: Magical Dreams", "Drill Dozer", "Gun", 
                                "Unreal Championship 2: The Liandri Conflict")), 3] <- 2005
y_data[which(y_data$Name %in% c("Def Jam: Fight for NY", "McFarlane's Evil Prophecy", 
                                "The Chronicles of Riddick: Escape from Butcher Bay", 
                                "The King of Fighters: Maximum Impact - Maniax", "Virtua Quest", 
                                "WarioWare: Twisted!", "Yu Yu Hakusho: Dark Tournament")), 3] <- 2004
y_data[which(y_data$Name %in% c("Drake of the 99 Dragons", "NBA Street Vol. 2", "NHL Hitz Pro")), 3] <- 2003
y_data[which(y_data$Name %in% c("eJay Clubworld", "Final Fantasy XI", "NASCAR: Dirt to Daytona", 
                                "Godzilla: Destroy All Monsters Melee", "NBA Starting Five", 
                                "Haven: Call of the King", "Hitman 2: Silent Assassin", 
                                "Home Run", "Street Hoops", 
                                "James Cameron's Dark Angel", "Jet X20", 
                                "MLB SlugFest 20-03", "Pac-Man Fever", 
                                "Robotech: Battlecry", "Star Wars Jedi Knight II: Jedi Outcast", 
                                "Super Duper Sumos", "Tom and Jerry in War of the Whiskers", 
                                "Tribes: Aerial Assault")), 3] <- 2002
y_data[which(y_data$Name %in% c("Frogger's Adventures: Temple of the Frog", "Suikoden III",
                                "Alone in the Dark: The New Nightmare", 
                                "Cubix Robots for Everyone: Clash 'n' Bash", 
                                "Harvest Moon: Save the Homeland", 
                                "Metal Gear Solid 2: Substance", "Rayman Arena", "Transworld Surf", 
                                "Twisted Metal: Small Brawl")), 3] <- 2001
y_data[which(y_data$Name %in% c("Action Man-Operation Extreme", "Smashing Drive", 
                                "The Dukes of Hazzard II: Daisy Dukes It Out", 
                                "WCW Backstage Assault")), 3] <- 2000
y_data[which(y_data$Name %in% c("Homeworld Remastered Collection", "Legacy of Kain: Soul Reaver", 
                                "RollerCoaster Tycoon", "Namco Museum")), 3] <- 1999
y_data[which(y_data$Name == "Triple Play 99"), 3] <- 1998
y_data[which(y_data$Name == "Donkey Kong Land III"), 3] <- 1997
y_data[which(y_data$Name == "Super Puzzle Fighter II"), 3] <- 1996
y_data[which(y_data$Name == "Indy 500"), 3] <- 1995
y_data[which(y_data$Name %in% c("Ghostbusters II", "Sonic the Hedgehog")), 3] <- 1991
y_data[which(y_data$Name == "Splatterhouse"), 3] <- 1988
y_data[which(y_data$Name == "Wheel of Fortune"), 3] <- 1986
y_data[which(y_data$Name %in% c("Karate", "Sabre Wulf")), 3] <- 1984
y_data[which(y_data$Name == "Flag Capture"), 3] <- 1983
y_data[which(y_data$Name %in% c("Adventure", "Dragster", "Fishing Derby", 
                                "Maze Craze: A Game of Cops 'n Robbers")), 3] <- 1980
y_data[which(y_data$Name %in% c("Space Invaders", "Breakaway IV", "Hangman", "Slot Machine")), 3] <- 1978
y_data[which(y_data$Name%in% c("Air-Sea Battle", "Circus Atari", "Combat")), 3] <- 1977
y_data[which(y_data$Name == "Super Breakout"), 3] <- 1976

# Filling in missing years in data (by joining with y_data)
data <- data %>% filter(!is.na(Year_of_Release)) %>% full_join(., y_data)
head(data)

# Checking NA in Critic Scores
data %>% filter(is.na(Critic_Score)) %>% nrow()

# Checking empty records in Genre
data %>% filter(Genre == "") %>% nrow()

# Filtering out empty genres and NA critic scores
data <- data %>% filter(Genre != "" & !is.na(Critic_Score))

# Adding Platform producer to Platform 
producer <- function(x) {
  if(x %in% c("PS", "PS2", "PS3", "PS4", "PSP", "PSV") == TRUE) {return("Sony")}
  else if(x %in% c("3DS", "DS", "GBA", "GC", "N64", "Wii", "WiiU") == TRUE) {return("Nintendo")}
  else if(x %in% c("PC", "X360", "XB", "XOne") == TRUE) {return("Microsoft")}
  else {return("Other")}
  }
data$Producer <- sapply(data$Platform, producer)

# Adding Bestseller parameter to the data
bestseller <-function(s){ifelse(s >= 0.5, "Bestseller", "Regular")}
data$Bestseller <- sapply(data$EU_Sales, bestseller)

# Dropping unused columns and renaming columns for the sake of simplicity
data <- data %>% select(-NA_Sales, -JP_Sales, -Other_Sales, -Global_Sales, -Critic_Count, -User_Score, -User_Count, -Publisher, -Developer, -Rating, -Platform)
data <- data %>% rename(Year = Year_of_Release, Sales = EU_Sales)
nrow(data)

# Splitting to train and test sets. Test set will be 20% of the data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = data$Bestseller, times = 1, p = 0.2, list = FALSE)
train_set <- data[-test_index,]
test_set <- data[test_index,]

#### Data Analysis and Models ####

################################################################
# Data Analysis
################################################################

# Statistics of unique values in different columns of the train dataset.
train_set %>% 
  summarize(n_games = n_distinct(Name),
            n_genre = n_distinct(Genre),
            n_producer = n_distinct(Producer))

# Checking duplicate names
train_set %>% filter(Name %in% train_set[which(duplicated(train_set)),1])

# Proportion of Bestsellers
train_set %>% ggplot(aes(Bestseller, group = Bestseller, fill = Bestseller)) +
  geom_bar() +
  ylab("Number of Games") +
  ggtitle("The Proportion of Bestsellers")
mean(train_set$Bestseller == "Bestseller")

## Sales
# Names vs. Sales for 50 topselling games
train_set %>% arrange(desc(Sales)) %>%
  head(50) %>%
  ggplot(aes(x = reorder(Name, Sales), y = Sales)) + 
  geom_bar(stat = "identity", fill = "grey") + 
  xlab("Names") + 
  ylab("Sales") +
  coord_flip() + 
  theme_bw() +  
  ggtitle("Most Selling Games") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Removing the Sales outlier 
train_set <- train_set %>% filter(Sales < 20)

## Critic Scores
train_set %>%
  ggplot(aes(Bestseller, Critic_Score, fill = Bestseller)) +
  geom_boxplot()

## Year 
# Checking year values by a table 
table(train_set$Year)

# Removing early years
train_set <- train_set %>% filter(Year >= 1996)

# General year plot 
train_set %>% ggplot(aes(Year)) +
  geom_bar()

# Year-Count plot with Bestseller breakdown
train_set %>% group_by(Year, Bestseller) %>% summarise(Count = n()) %>%
  ggplot(aes(Year,Count, col=Bestseller)) +
  geom_line()

# Year-Critic_Score plot with Bestseller breakdown
train_set %>% ggplot(aes(Critic_Score, Year, color = Bestseller)) +
  geom_point() + 
  scale_x_continuous() + 
  scale_y_continuous(breaks = c(seq(1996, 2016, 1))) 

## Genres
# General Genres Wordcloud
genres <- train_set %>% 
  group_by(Genre) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))
layout(matrix(c(1,2), nrow =2) , heights = c(1,4))
par(mar = rep(0,4))
plot.new()
text(x = 0.5, y = 0.5, "All Genres by Count")
wc_all <- wordcloud(words = genres$Genre, freq = genres$Count, random.order = FALSE, random.color = FALSE,
                    rot.per = 0.35, colors = brewer.pal(7,"Greens"), scale = c(4,.2), font = 2, main = "Genres by Count")

# Bestseller Genres
best_genres <- train_set %>% filter(Bestseller == "Bestseller") %>% group_by(Genre) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))
layout(matrix(c(1,2), nrow = 2) , heights = c(1,4))
par(mar = rep(0,4))
plot.new()
text(x = 0.5, y = 0.5, "Bestseller Genres by Count")
wc_best <- wordcloud(words = best_genres$Genre, freq = best_genres$Count, random.order = FALSE, random.color = FALSE,
                     rot.per = 0.35, colors = brewer.pal(7,"Greens"), scale = c(4,.2), font = 2, main = "Genres by Count")

## Producer
train_set %>% group_by(Producer) %>%
  summarise(Sum = sum(Sales)) %>%
  arrange(desc(Sum)) %>%
  head(10) %>%
  ggplot(aes(reorder(Producer, -Sum), Sum)) + 
  geom_bar(stat = "identity", fill = "grey") + 
  xlab("Producer") + 
  ylab("Sales") + 
  ggtitle("Sales by Producers") + 
  theme_bw() 

## Removing outlier in Producers
train_set <- train_set %>% filter(Producer != "Other")

################################################################
# Models
################################################################

# Converting outcome into a factor of 0s and 1s
train_set$Bestseller <- as.factor(+(train_set$Bestseller == "Bestseller"))
test_set$Bestseller <- factor(+(test_set$Bestseller == "Bestseller"))

## Logistic Regression Model ##
fit_glm <- train(Bestseller ~ Critic_Score + Genre + Year + Producer, method = "glm", data = train_set)
pred_glm <- predict(fit_glm, test_set)

# Confusion Matrix
cm_glm <- confusionMatrix(pred_glm, as.factor(test_set$Bestseller))
cm_glm 

# Accuracy (same as mean(pred_glm == test$Bestseller))
acc_glm <- cm_glm$overall["Accuracy"]
acc_glm

# Balanced Accuracy 
bacc_glm <- cm_glm$byClass["Balanced Accuracy"]
bacc_glm

# Saving values in a data frame
results <- data_frame(Model = "Generalised Linear Model", 
                      Accuracy = format(round(acc_glm, 6), nsmall = 6), 
                      F1_Score = format(round(bacc_glm, 6), nsmall = 6))

## kNN Model ##
set.seed(1, sample.kind = "Rounding")  
control <- trainControl(method = "cv", number = 10, p = .9)
fit_knn <- train(Bestseller ~ Critic_Score + Genre + Year + Producer,
                  method = "knn",
                  data = train_set,
                  tuneGrid = data.frame(k = seq(3, 51, 2)),
                  trControl = control)

# Optimal k value
fit_knn$bestTune

# Checking the best k in the plot
ggplot(fit_knn)

# Prediction
pred_knn <- predict(fit_knn, test_set)

# Confusion Matrix
cm_knn <- confusionMatrix(pred_knn, as.factor(test_set$Bestseller))
cm_knn 

# Accuracy (same as mean(pred_knn == test$Bestseller))
acc_knn <- cm_knn$overall["Accuracy"]

# Balanced Accuracy 
bacc_knn <- cm_knn$byClass["Balanced Accuracy"]

# Adding values to the results data frame
results <- bind_rows(results, 
                     data_frame(Model = "kNN Model", 
                                Accuracy = format(round(acc_knn, 6), nsmall = 6), 
                                F1_Score = format(round(bacc_knn, 6), nsmall = 6)))
results 

## Classification Tree Model ##
# Fitting a classification tree
set.seed(1, sample.kind = "Rounding")  
fit_rpart <- train(Bestseller ~ Critic_Score + Genre + Year + Producer, data = train_set,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))

# Best cp
fit_rpart$bestTune

# Checking the best value of cp in the plot
ggplot(fit_rpart)

# Inspecting final model
fit_rpart$finalModel
# Apparently, there's just a root in the fit

# Checking root imortance
varImp(fit_rpart)

# Prediction
pred_rpart <- predict(fit_rpart, test_set)

# Confusion Matrix
cm_rpart <- confusionMatrix(pred_rpart, as.factor(test_set$Bestseller))
cm_rpart 

# Accuracy (same as mean(pred_rpart == test$Bestseller))
acc_rpart <- cm_rpart$overall["Accuracy"]

# Balanced Accuracy 
bacc_rpart <- cm_rpart$byClass["Balanced Accuracy"]

# Adding values to the results data frame
results <- bind_rows(results, 
                     data_frame(Model = "Classification Tree Model", 
                                Accuracy = format(round(acc_rpart, 6), nsmall = 6), 
                                F1_Score = format(round(bacc_rpart, 6), nsmall = 6)))
results 

## Random Forest Model ##
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method="cv", number = 5)
fit_rf <- train(Bestseller ~ Critic_Score + Genre + Year + Producer, data = train_set,
                method = "rf",
                ntree = 150, nodesize = 1,
                tuneGrid = data.frame(mtry = seq(1:7)),
                trControl = control)

# The optimal value of cp
fit_rf$bestTune

# Checking the best value of cp in the plot
ggplot(fit_rf)

# Determining the importance of various predictors (first rows).
varImp(fit_rf) 

# Prediction
pred_rf <- predict(fit_rf, test_set)

# Confusion Matrix
cm_rf <- confusionMatrix(pred_rf, as.factor(test_set$Bestseller))
cm_rf 

# Accuracy (same as mean(pred_rf == test$Bestseller))
acc_rf <- cm_rf$overall["Accuracy"]

# Balanced Accuracy 
bacc_rf <- cm_rf$byClass["Balanced Accuracy"]

# Adding values to the results data frame
results <- bind_rows(results, 
                     data_frame(Model = "Random Forest Model", 
                                Accuracy = format(round(acc_rf, 6), nsmall = 6), 
                                F1_Score = format(round(bacc_rf, 6), nsmall = 6)))
results

## Ensemble ##
p_knn <- predict(fit_knn, test_set, type = "prob")
p_rf <- predict(fit_rf, test_set, type = "prob")
p_rf <- p_rf/rowSums(p_rf)
p <- (p_knn + p_rf)/2
pred <- factor(apply(p, 1, which.max)-1) 
cm <- confusionMatrix(pred, as.factor(test_set$Bestseller))
acc <- cm$overall["Accuracy"]
bacc <- cm$byClass["Balanced Accuracy"]
results <- bind_rows(results, 
                     data_frame(Model = "kNN + RF Ensemble", 
                                Accuracy = format(round(acc, 6), nsmall = 6), 
                                F1_Score = format(round(bacc, 6), nsmall = 6)))
results
################################################################
# Results
################################################################

# Resulting tibble with the metrics by all the approaches
datatable(results, rownames = FALSE, filter = "top", options = list(pageLength = 5, scrollX = T)) 