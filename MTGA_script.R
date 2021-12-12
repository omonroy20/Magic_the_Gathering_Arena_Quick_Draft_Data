library(rvest)
cnames <- c("Name", "Color", "Rarity", "NumSeen", "ALSA", "NumPicked",
            "ATA", "NumGP", "GPWR", "NumOH", "OHWR", "NumGD", "GDWR", "NumGIH", 
            "GIHWR", "NumGND", "GNDWR", "IWD") # Names of the columns for data
# Now we'll import the .csv from data gathered in 17lands.com
mtg <- read.csv("card_list.csv", header = F, col.names = cnames)
mtg <- mtg[-which(duplicated(mtg) == TRUE), ] # Removes duplicate rows
mtg <- mtg[-2, ] # Removes empty row
# We'll now use a for loop and Regex to remove unwanted strings in select columns
for(i in 1:dim(mtg)[2]) {
  if(sum(i == c(9, 11, 13, 15, 17)) == 1) {
    mtg[, i] <-  as.numeric(substr(mtg[, i], start = 1, stop = 5)) / 100
  }
}
mtg$IWD <- as.numeric(gsub("[^0-9.-]", "", mtg$IWD)) # More regex to extract numbers
# Here, I'll be removeing data that contains cards with more than one
# color and also without color (colorless). The reasoning is that we want the data
# to only contain monocolored cards as many cards with multiple colors. For example,
# this card:
# https://gatherer.wizards.com/Pages/Card/Details.aspx?name=Chatterfang%2C+Squirrel+General
# is supposed to be primarily green, however due to the black mana symbol in the text,
# it would be counted as "GB" in this data, which is somewhat wrong and convolutes 
# the data. With colorless cards, we're removing them because we're only examining 
# the main 5 colors in the game. 
mtga <- mtg[-which(nchar(as.character(mtg$Color)) > 1), ] # Removes multicolor cards
mv <- which(nchar(as.character(mtga$Color)) < 1) # Index of colorless cards
mtga$Color <- as.character(mtga$Color)
mtga <- mtga[-mv, ] # Removes colorless cards
mtga$Color <- as.factor(mtga$Color)
# Finally, we'll remove variables from the data that don't
# tell us anything about the success of a card.
mtga <- mtga[, -c(4, 5, 6, 7, 8, 10, 12, 14, 16, 17)] 
head(mtga, 10)

# 2) Visualizations Using Boxplots

library(ggplot2)
summary(mtga) # Summary statistics of each variable
table(mtga$Color) # Checks amount of cards per colors

# Now we'll build boxplots to see a visualization of the 
# success rate of each color depending on the turn
# the cards were drawn/time of usage. Each graph will have
# self-explanatory axis names and titles. 
ggplot(mtga, aes(x = Color, y = IWD)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate Improvement When Drawn") +
  ggtitle("Win Rate Chances When Drawing Cards of Certain Colors")

# We can see that the overall win rate chances improve more when drawing
# a blue card compared to the other 4 colors, with red and white having
# the lowest win rate improvement.

ggplot(mtga, aes(x = Color, y = GPWR)) + 
  geom_boxplot(outlier.colour = "blue", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate When Maindecked") +
  ggtitle("Win Rate When Adding Cards of Certain Color into the Maindeck")

# This graph seemingly contradicts the last graph, showing us that adding
# blue cards gives the lowest win rate compared to the other 4 colors, with 
# red, white, and especially black leading the numbers here. Perhaps those
# blue cards just weren't drawn at all or weren't used to their full effect...

ggplot(mtga, aes(x = Color, y = GDWR)) + 
  geom_boxplot(outlier.colour = "green", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate When Drawn Turn 1 or Later") +
  ggtitle("Win Rate When Drawing Cards of Certain Color Turn 1 or Later")

# Here, we can see a bit of a composite scenario of the previous two graphs
# where black has the highest win rate followed by blue, but the other 3 colors 
# seem to be equally matched. It tells us that there is a major difference in
# simply adding a card to a deck and actually drawing it in an actual game. 

ggplot(mtga, aes(x = Color, y = OHWR)) + 
  geom_boxplot(outlier.colour = "yellow", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate When Drawn Turn 1 or Later") +
  ggtitle("Win Rate When Card of Certain Color is in Opening Hand")

# We get strange results here with white having the highest win rate when a 
# white card is in the hand right off the bat with black and red slightly
# behind it and blue at the bottom. Seems to indicate that white cards
# are at their best in the early game, while black is effective at any point
# of the game, and blue does not fare well in the beginning. At least, that's what
# I believe is happening in this data. Could also just simply be that
# games of quick draft are varied wildly. 

ggplot(mtga, aes(x = Color, y = GIHWR)) + 
  geom_boxplot(outlier.colour = "purple", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate in Hand (Opener or Drawn)") +
  ggtitle("Win Rate When Card of Certain Color is in Hand at any Time")

# When it comes to data with having the specific card in your hand at any time, 
# it does appear that this boxplot confirms what we've seen in the previous graphs:
# that black seems to be the superior color of choice in quick draft. White also
# seems to be performing well, perhaps that's from white being strong at the 
# early game as we inferred in the previous graph. Surprisingly, blue
# just seems to be a middling color, matched with green and red. Could
# be that, once again, blue is probably not strong in the early game. 

# One noticable thing from the previous graphs is that they had to omit a ton
# observations due to NA's. SO here, I'll remove them to see if they make a difference. 
mtga_nona <- mtga[-which(rowSums(is.na(mtga)) > 0), ]

ggplot(mtga_nona, aes(x = Color, y = IWD)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate Improvement When Drawn") +
  ggtitle("Win Rate Chances When Drawing Cards of Certain Colors")
ggplot(mtga_nona, aes(x = Color, y = GPWR)) + 
  geom_boxplot(outlier.colour = "blue", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate When Maindecked") +
  ggtitle("Win Rate When Adding Cards of Certain Color into the Maindeck")
ggplot(mtga_nona, aes(x = Color, y = GDWR)) + 
  geom_boxplot(outlier.colour = "green", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate When Drawn Turn 1 or Later") +
  ggtitle("Win Rate When Drawing Cards of Certain Color Turn 1 or Later")
ggplot(mtga_nona, aes(x = Color, y = OHWR)) + 
  geom_boxplot(outlier.colour = "yellow", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate When Drawn Turn 1 or Later") +
  ggtitle("Win Rate When Card of Certain Color is in Opening Hand")
ggplot(mtga_nona, aes(x = Color, y = GIHWR)) + 
  geom_boxplot(outlier.colour = "purple", outlier.shape = 8,
               outlier.size = 1.5) +
  ylab("Win Rate in Hand (Opener or Drawn)") +
  ggtitle("Win Rate When Card of Certain Color is in Hand at any Time")

# There doesn't seem to be much noticable difference from the graphs using
# the data with the NA's, so we'll continue to use that data.

# Here, we're just gonna use histograms on the predictor variables
# to see their distribution.
par(mfrow = c(2, 3))
hist(mtga$IWD)
hist(mtga$GPWR)
hist(mtga$GDWR)
hist(mtga$OHWR)
hist(mtga$GIHWR)

# Looks like they all follow normal distribution.

# 3) Statistical Tests

# Since we've shown that the variables are indeed normal dist., we'll be
# using one-way ANOVA, TukeyHSD, and pairwise t-tests to see
# if the differences in sucess rate of each color are significant (< 0.05).

iwd_aov <- aov(mtga$IWD ~ mtga$Color)
summary(iwd_aov)
# ANOVA shows significance in Color.
TukeyHSD(iwd_aov)
# We see signicance in almost every color pairing except G-B, W-G, 
# and W-R. Looking at the boxplot, we can see that
# these results do appear to be the case although W-R seem
# decptively different at first glance.
pairwise.t.test(mtga$IWD, mtga$Color)
# The t-tests seem to confirm the results with one important
# exception: W-G. Here, that pairing is significant, granted
# it barely is since the value is at 0.036. This discrepancy
# can possibly be due to the differences in the test methods.

gp_aov <- aov(mtga$GPWR ~ mtga$Color)
summary(gp_aov)
# ANOVA shows significance in Color.
TukeyHSD(gp_aov)
# We see signicance in almost every color pairing except R-B, W-B, 
# and W-R. Looking at the boxplot, we can see that
# these results do appear to be the case.
pairwise.t.test(mtga$GPWR, mtga$Color)
# The t-tests seem to confirm the results with one important
# exception: W-R. Here, that pairing is significant, granted
# it barely is since the value is at 0.031. Once again, this discrepancy
# can possibly be due to the differences in the test methods.

gd_aov <- aov(mtga$GDWR ~ mtga$Color)
summary(gd_aov)
# Shows significance in Color.
TukeyHSD(gd_aov)
# We see signicance in only color pairings with B, which makes 
# sense as the boxplots clearly show black being superior 
# in this category.
pairwise.t.test(mtga$GDWR, mtga$Color)
# Shows similar results to the TukeyHSD across the board.

oh_aov <- aov(mtga$OHWR ~ mtga$Color)
summary(oh_aov)
# Shows significance in Color.
TukeyHSD(oh_aov)
# We see signicance in almost every color pairing except R-B, W-B, 
# and R-G. Looking at the boxplot, we can see that
# these results do appear to be the case.
pairwise.t.test(mtga$OHWR, mtga$Color)
# The t-tests seem to confirm the results with one important
# exception: R-B. Here, that pairing is significant, however
# its significance is at 0.03.

gih_aov <- aov(mtga$GIHWR ~ mtga$Color)
summary(gih_aov)
# Shows significance in Color.
TukeyHSD(gih_aov)
# We see signicance in only color pairing with B, which makes 
# sense as the boxplots clearly show black being superior 
# in this category as similary seen with GDWR. 
pairwise.t.test(mtga$GIHWR, mtga$Color)
# Shows similar results to the TukeyHSD across the board, although,
# W-U comes very close to being significant being at 0.05047.

# 4) RandomForest Model

# Now that we see that there are plenty of significant differences 
# in each color (with black having a major advantage), now we want to
# see if we can build a model that can predict card colors by simply
# using their success rate, and we can accomplish this with 
# RandomForest which should be able to differentiate between the 
# the characteristics in success between each card and color. 
library(randomForest)
set.seed(1) # Set seed for replication
# We want a training and testing set with a 60/40 split.
train <- sample(dim(mtga)[1], dim(mtga)[1] * 0.60, replace = F)
mtg_train <- mtga[train, ] # Training set
mtg_test <- mtga[-train, ] # Testing set
mtg_rf <- randomForest(Color ~ ., mtg_train[, -1], mtry = 6,
                       importance = T, na.action = na.omit)
# Using the model, we'll the testing eubset to make predictions.
mtg_pred <- predict(mtg_rf, mtg_test)
varImpPlot(mtg_rf) # Shows the most important variable from top to bottom
# Shows a table with the avccuacy. Successful prediction are from 
# top-left to bottom-right with the other results being wrong prediction.
t_mtg <- table(mtga$Color[-train], mtg_pred)
t_mtg # Confusion matrix of predictions
accuracy <- sum(diag(t_mtg)) / sum(t_mtg)
accuracy # Calculation of the prediction accuracy.

# As we can see, the model doesn't really make many accurate predictions
# despite the statistical tests showing plenty of differences in each
# color's win rates. But to be fair, the majority of the significance came from 
# black being the most successful while most other color pairings were in 
# similar standing. While the model failed, we can still come out from this with
# the knowledge that black is very successful, at least in the quick draft format. 