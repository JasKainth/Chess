# Chess
Creating an exploratory analysis of outcomes for chess games

## Summary  

In this project, we create an exploratory analysis of a dataset on Lichess Chess games. The dataset was obtained from [Kaggle](https://www.kaggle.com/datasnaek/chess).  

We take a look at win rates and how they are affected by openings, Elo rating and if the game is rated or unrated. 

## chess.html  

This file is a knit version of the .Rmd file. Due to the large size, Github is unable to show the file. To view the file, either download the file locally and open it or it can also be viewed [here](https://htmlpreview.github.io/?https://github.com/JasKainth/Chess/blob/main/chess.html).  

## Exploratory Data Analysis  

For the EDA, we take a look at the relations which arise from the columns provided and the winner of the game. We look at the openings, Elo rating, the level of the players, whether the game is rated or not, increment (format) and the total moves of theory.  

### Opening  

The openings were looked at as a whole and also broken down for each level. The levels are the following (looking at the rating of the higher player):  
* Beginner: Elo Rating < 1200
* Intermediate: 1200 < Elo Rating < 1800
* Expert: Elo Rating > 1800  

It should be noted that these levels are quite vague and the thresholds were created in an attempt to both create levels where the level of play may differ but also not having any levels with too few data. We also make sure the Elo difference between the players is less than 300 because otherwise, it may be a total mismatch.  

Below is a plot showing the most popular openings, both as a whole and also broken down by the levels.  

![opening_plot](https://github.com/JasKainth/Chess/blob/main/opening.jpg)

### Elo Rating  

We take a look at how often the higher-level player wins. We break this up by if the higher level player is white or black and then also facet the plots based on the difference of Elo rating. 

Below is a plot showing this relationship, and as we see the win percentage of the expected winner grows a great deal between each plot.  

![elo_plot](https://github.com/JasKainth/Chess/blob/main/elo_difference.jpg)

### Format  

We also consider the format of the games. We categorized the top 22 unique increments (the increments with at least 100 games) into their respective formats using the [Lichess time control table](https://www.reddit.com/r/chess/comments/bqp8yo/lichess_standard_time_control_categories/). The only formats found in these time controls were Rapid and Classical, so we break down the most popular openings for each format.  

Below is a plot showing the openings. It should be noted that each top 10 is comprised of the same openings, just in a different order.  

![format_plot](https://github.com/JasKainth/Chess/blob/main/format.jpg)


## Statistical Analysis  

In this section, we will attempt to predict the winner of the game. We will use [Tidymodels](https://www.tidymodels.org/) to create the models. We will attempt multiple classification techniques to find the best model for our data. 

### Multinomial Regression  

The first model which we create is a multinomial regression. For this model we use the Elo ratings, the opening played, the time control & whether the game is rated or not as predictors.  

The plot below shows the results of our model, the x-axis representing the predicted result and the facets representing the expected results.  

![multinomial_plot](https://github.com/JasKainth/Chess/blob/main/multinomial_outcomes.jpg)


We find that when the expected winner is white, our model performs quite well. However, when the expected winner is black, it does not do as well (however, the accuracy is still above 50%). One interesting point is that our model never predicts the outcome to be a draw.