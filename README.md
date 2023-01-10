# Betting Strategy using System Fibonacci

## Introduction

The following study aims to test the Fibonacci strategy in betting. First, let's give a summary of the Fibonacci sequence. The Fibonacci sequence was created by the Italian mathematician Leonardo 'Fibonacci' in the 13th century.

The idea behind the sequence is pretty simple. The first two terms of the sequence are '1', and '1' and the next ones will be the sum of the previous two figures. So the start of the sequence is 1, 1, 2, 3, 5, 8, 13... and so on.

The betting strategy using the Fibonacci sequence would be for betting with only two possible outcomes. For example, the coin result (heads or tails). It can also be the odd or even result of a dice. In the case of the Casino, it can be used when betting red or black on a roulette wheel.

To explain how the system works, let's take an example of heads and tails. To start, we'll bet $1, the first element of the Fibonacci sequence. Our bet will be Heads. If the result of the coin is Tails, it means that we lose, then in our next bet we will increase the bet for the next element of the sequence in this case it would be $1.

On the other hand, if the result is heads, that is, we win the bet, for the next bet we will decrease the bet amount for two previous elements of the sequence. As we have the first element, we continue betting $1. However, for example, if we had betted  $5, we would return two elements, so in the next bet we would bet $2 (2, 3, 5). This is how the Fibonacci System works.

## Objective

Our objective will be to verify in which of the scenarios it is possible to win with the strategy and which not. Also, check the risk linked to this strategy.

The application was designed to simulate bets according to the player's parameters. We want the reader/player to be able to make several possible simulations, for example, the probability of winning the bet or the percentage they would win on there own bet, among others.

We also created two stopping criteria and created a simulation of the process, similar to a Monte Carlo idea. At the end, the reader can download the simulation data if it is of interest.

## Organize Code

The code is organized into three R scripts. The 'app.R' and 'functions_article.R' scripts are for the panel in Rshiny. The 'fibonacci_script.R' is the simulation done in R without using Rshiny.

## Rshiny Results

The work below shows a series of simulations using some betting strategies.

https://alexandretcosta.shinyapps.io/fibonacci_betting/

## Source

https://insidersbettingdigest.com/guides/the-fibonacci-betting-system/#:~:text=The%20Fibonacci%20betting%20system%20is,red%20or%20black%20in%20roulette.
