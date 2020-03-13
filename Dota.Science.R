### DOTA PROJECT ###

## Obtaining Dataframe

library(tidyverse)
library(ggplot2)
library(jsonlite)
library(reshape2)
library(caret)
library(pROC)
library(broom)
library(Amelia)
# Reading the data from JSON file
# The issues with the file: it was badly formatted; had to add commas to every line,
# as well as square brackets at the beginning and the end of the file.
# added commas with bash: 'sed -e 's/$/,/' matches.json > matches_reworked.json'. Similary,
# the same was done to modify the 'heroes.json' file and was renamed 'heroes_reworked.json'.

JSON_list_matches <- fromJSON('matches_reworked.json',flatten=TRUE)
# The heroes_reworked.json file contains information about the heroes, mostly we will only
# use it to identify the hero IDs from the player data.
JSON_list_heroes <- fromJSON('heroes_reworked.json',flatten=TRUE)

MatchData <- as.data.frame(JSON_list_matches)
Heroes <- as.data.frame(JSON_list_heroes)

head(Heroes)

# function to flatten the nested player data frames
Player.flatten <- function(player_list){
flat <- dcast(melt(as.data.frame(player_list),
                   id.var="player_slot"),
              1~player_slot+variable)
return(flat)
}

head(MatchData)
# Extracting the list of nested data frame from the global data
Player_stats <- MatchData$players


# Flatten the columns to obtain the data structure that we want
Player_df <- as.data.frame(do.call(rbind, lapply(Player_stats,FUN=Player.flatten)))[-1]
# [-1] because the first column is a column of 1s that comes from the Player.flatten function
head(Player_df)

# Now lets combine the Player_df with Match Data to replace the original 'players' column
# that contained the nested data frames.

MatchDataClean <- cbind(MatchData[-7],Player_df) # [-7] to remove the old 'players' variable

## Wrangling / Cleaning

# We are interested in the Experience (XP) a player gets and Gold. Therefore, we can
# go ahead and remove any variables that we won't be needing.

summary(MatchDataClean)

# We can see that the dire_score and radiant_score have 31454 NA's each. which is almost
# half of the rows. Fortunately for us, we won't be using the columns for our analysis,
# therefore we can keep these rows but remove the columns later.

colnames(MatchDataClean)

write_csv(MatchDataClean,'MatchDataClean.csv')
# saving the data to a separate file, for ease of use
MatchDataClean <- read.csv('MatchDataClean.csv')

# Let's look at potential outliers from the duration column and visualise them
summary(MatchDataClean$duration)

ggplot(MatchDataClean,aes(x=duration))+
geom_histogram(binwidth=100,fill='blue',colour='black')+
labs(x='Match duration (in seconds)',y='Frequency')

# We can see that the matches are mostly of the same duration and that a very little
# amount of matches actually last longer than 6000 seconds. Let's check how many
# matches actually were longer than 6000 seconds, because these are very long matches and will
# our analyses.

long_games <- MatchDataClean %>%
filter(duration >= 6000)

long_games$duration
# There are only 9 games which are seriously long, we can delete these rows, deleting them
# won't affect the accuracy of our analysis.

MatchDataShort <- MatchDataClean %>%
filter(duration <= 6000)

# We can now start selecting the columns that we will need for our initial analysis.

XP_advantage_cols <- MatchDataShort %>%
select(ends_with('xp_per_min'))

Gold_advantage_cols <- MatchDataShort %>%
select(ends_with('gold_per_min'))

#### XP, Gold and victory

Victory <- MatchDataShort%>%
select(radiant_win)


# changing from boolean to actual factors for modelling
Victory$radiant_win <- as.factor(ifelse(Victory$radiant_win==TRUE,'win','loss'))



# Now let's create columns of Gold and XP advantage for Radiant side



Victory$XP_radiant <- rowSums(XP_advantage_cols[,1:5])*(MatchDataShort$duration/60)
Victory$XP_dire <- rowSums(XP_advantage_cols[,6:10])*(MatchDataShort$duration/60)
# xp per min * minutes played

Victory$Gold_radiant <- rowSums(Gold_advantage_cols[,1:5])*(MatchDataShort$duration/60)
Victory$Gold_dire <- rowSums(Gold_advantage_cols[,6:10])*(MatchDataShort$duration/60)
# gold per min * minutes played

head(Victory)

# We still have 5 missing values for radiant_win, which a very small amount
# compared to the number of observations. We can go ahead and remove them.

Victory <- na.omit(Victory)

Victory[is.na(Victory),] # 0 missing values
## ML model

##  Data Partition with seed to be able to reproduce
set.seed(10122019)
# train test splitting: getting our train rows from createDataPartition and
# using them to split our data frame

splitRows <- createDataPartition(Victory[,'radiant_win'],p=.75,list=FALSE,times=1)
trainVictory <- Victory[splitRows,]
testVictory <- Victory[-splitRows,]

objControl <- trainControl(method='cv', number=3, returnResamp='none',
                         summaryFunction = twoClassSummary, classProbs = TRUE)

# Categorical model so we use ROC

objModel <- train(trainVictory[,c('XP_radiant','Gold_radiant','XP_dire','Gold_dire')],
                trainVictory[,'radiant_win'],
                method='gbm',
                trControl=objControl,
                metric = "ROC",
                preProc = c("center", "scale"))

summary(objModel)


# Here we see that our model argues that XP is close to irrelevant when it comes to victory.
# (approx. 99% vs. less than 1%). Let's evaluate the model:

predictions <- predict(object=objModel,
                     testVictory[,c('XP_radiant','Gold_radiant','XP_dire','Gold_dire')],
                     type='raw')

print(postResample(pred=predictions,obs=testVictory[,'radiant_win']))

# Our model has an accuracy of 98.3% - incredible.
# We can deduce that Gold is more important than Experience for this patch.


##### Let's try the items and see how much item picks for the top 3 most picked
##### heroes influence the outcome of the game

Hero_picks <- MatchDataClean %>%
select(ends_with('hero_id'))

Hero_picks_vector <- c(
Hero_picks$`0_hero_id`,
Hero_picks$`1_hero_id`,
Hero_picks$`2_hero_id`,
Hero_picks$`3_hero_id`,
Hero_picks$`4_hero_id`,
Hero_picks$`128_hero_id`,
Hero_picks$`129_hero_id`,
Hero_picks$`130_hero_id`,
Hero_picks$`131_hero_id`,
Hero_picks$`132_hero_id`
)



Top_picks <- as.data.frame(table(Hero_picks_vector))%>%
arrange(desc(Freq))

most_picked_id <- Top_picks[1:10,] # Most picked id = 86
most_picked_id

head(MatchDataClean)

MatchDataMostPicked <- MatchDataClean %>%
select(ends_with('hero_id'),
       contains('item'),
       radiant_win
       ) %>%
filter(`0_hero_id`==86|
       `1_hero_id`==86|
       `2_hero_id`==86|
       `3_hero_id`==86|
       `4_hero_id`==86|
       `128_hero_id`==86|
       `129_hero_id`==86|
       `130_hero_id`==86|
       `131_hero_id`==86|
       `132_hero_id`==86
  )

head(MatchDataMostPicked)


# get the cell which contains the value, find how far in columns, items are and grab them

ItemBuilds86 <- data.frame(radiant_win=MatchDataMostPicked$radiant_win)

dim(MatchDataMostPicked)

Items <- MatchDataMostPicked %>%
select(contains('item'))

for(i in 1:dim(MatchDataMostPicked)[1]){
col_id <- which(MatchDataMostPicked[i,1:10]==86)
# get column number for which player is playing hero 86
# obtaining the items for that specific player:
# each player has 6 items
ItemBuilds86[i,2] <- Items[i,(col_id-1)*6+1]
ItemBuilds86[i,3] <- Items[i,(col_id-1)*6+2]
ItemBuilds86[i,4] <- Items[i,(col_id-1)*6+3]
ItemBuilds86[i,5] <- Items[i,(col_id-1)*6+4]
ItemBuilds86[i,6] <- Items[i,(col_id-1)*6+5]
ItemBuilds86[i,7] <- Items[i,(col_id-1)*6+6]
}

head(ItemBuilds86)

ItemBuilds86 <- ItemBuilds86%>%
rename(Item_1 = 'V2',
       Item_2 = 'V3',
       Item_3 = 'V4',
       Item_4 = 'V5',
       Item_5 = 'V6',
       Item_6 = 'V7')



# Now we want to see if players play the Hero in the most successful way: comparing
# the most played items, as well as the most successful items.

TopItems86 <- as.data.frame(table(c(ItemBuilds86$Item_1,
    ItemBuilds86$Item_2,
    ItemBuilds86$Item_3,
    ItemBuilds86$Item_4,
    ItemBuilds86$Item_5,
    ItemBuilds86$Item_6)
    ))

# I used RDota2 to find the ids for items
install.packages('RDota2')
library(RDota2)
key_actions(action = 'register_key', value = 'F377D81D0C9F9534A8C74FE065B4A495')
item_names <- get_game_items()$content # list df of items and relevant information
head(item_names)

item_names <- item_names %>%
select(id,localized_name) %>%
filter(id<278) # Newly added items that weren't available at the time our data was taken

TopItems86 <- TopItems86%>%
arrange(desc(Freq)) # ranking items

item_names[item_names$id == 3,2]

top6 <- TopItems86[2:7,] # Top 6 most picked items on this hero - 0 just means item slot empty

# Now let's reveal which are the actual items:
for(i in 1:6){
top6[i,3] <- item_names[item_names$id == top6$Var1[i],2]
}

top6 <- top6 %>%
rename(Name = 'V3',
       ID = 'Var1')

top6

## Comparing How well the Item Builds did

ItemBuilds86[is.na(ItemBuilds86),]
# Theres 1 missing value for radiant_win, it's 1 out of 13000 observations,
# we can remove it.


ItemBuilds86 <- na.omit(ItemBuilds86)
ItemBuilds86

# Creating a list of vectors containing the items, fusing the builds
build <- c()
build_list <- list()
for(i in 1:dim(ItemBuilds86)[1]){
for(n in 2:7){
  build[n-1] <- ItemBuilds86[i,n]
}
build_list[[i]] <- c(build[order(build)],ItemBuilds86$radiant_win[i])
# adding the outcome of match at the end of the vector for following analysis
}


# initializing vectors
skips <- c()
unique_builds <- c()
build_count <- c()
win <- c()
for(i in 1:dim(ItemBuilds86)[1]){
if(i %in% skips){# skipping the row if build is the same
  next()
}
counts <- 0 # counting how many times build was chosen
success <- 0 # counting how many wins for the i-th build
for(n in 1:dim(ItemBuilds86)[1]){
  same <- setequal(build_list[[i]][1:6],build_list[[n]][1:6]) # comparing vectors (builds)
  if(same==TRUE){
    counts<-counts+1
    skips <- c(skips,n) # creating  a vector of already matched builds
  }
  ifelse((same==TRUE & build_list[[n]][7]==1),success <- success+1,success <- success+0)
}
unique_builds <- c(unique_builds,i)
build_count <- c(build_count,counts)
win <- c(win,success)
}# finally, creating the vectors that will make up the final data frame

Build_success_df <- data.frame(build_ID=unique_builds,build_count=build_count,win_count=win)

write.csv(Build_success_df,'Build_success.csv')
# took a long time to run this loop, no need to do it again
Build_success_df <- read.csv('Build_success.csv')

Build_success_df$win_rate <- Build_success_df$win_count/Build_success_df$build_count
Build_success_df$pick_rate <- Build_success_df$build_count/sum(Build_success_df$build_count)


Most_picked_builds <- Build_success_df %>%
arrange(desc(pick_rate))

Best_builds_20 <- Most_picked_builds[1:10,c('build_ID','win_rate')]%>%
arrange(desc(win_rate))
# out of the most picked builds, I checked
# which were the most successful.
ID <- Best_builds_20[1,1]

items <- build_list[[ID]][1:6]

best_items <- data.frame(ID=items)

for(i in 1:6){
if(best_items$ID[i]==0){
  best_items[i,2] <- 'Empty Slot' # the item slot is empty (i.e. no items)
  next()
}
best_items[i,2] <- item_names[item_names$id == best_items$ID[i],2]
}

best_items <- best_items%>%
rename(Item='V2')
