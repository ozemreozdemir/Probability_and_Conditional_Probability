#######################################################
## OZKAN EMRE OZDEMIR
## HOMEWORK 2 : Monty Hall Probabilities
## 04/14/16
## Class:  Methods for Data Analysis
#######################################################
## Clear objects from Memory :
rm(list=ls())
##Clear Console:
cat("\014")
##
#######################################################
# Simulation 1 : Chances of Winning!!                 #
# "Jesus Loves Winners!"                              #
#######################################################
#      door       door       door             
#      ___        ___        ___              
#     | 1 |      | 2 |      | 3 |
#     | ? |      | ? |      | ? |
#     |___|      |___|      |___|
#######################################################
#Let's define out winning function

winning = function(number_of_plays,number_of_doors,switch_door=TRUE){
        winning_door = sample(number_of_doors, number_of_plays, replace = T)
        selected_door = sample(number_of_doors, number_of_plays, replace = T)
        if (switch_door){
                check_switch_and_win = (selected_door!=winning_door)
                #probability of winning = total number of winning / total number of play
                switch_and_win=sum(check_switch_and_win)/number_of_plays
                #variance of switching and winning :
                var_switch_and_win = var(check_switch_and_win*1)
                list(switch_and_win = switch_and_win,var_switch_and_win = var_switch_and_win)
        }else{
                check_dontswitch_and_win = (selected_door==winning_door)
                #probability of winning = total number of winning / total number of play
                dontswitch_and_win=sum(check_dontswitch_and_win)/number_of_plays
#                return(dontswitch_and_win)
                #variance of not switching and winning :
                var_dontswitch_and_win = var(check_dontswitch_and_win*1)
                list(dontswitch_and_win = dontswitch_and_win,var_dontswitch_and_win = var_dontswitch_and_win)
        }
}

#######################################################
# Simulation 2 : Chances of surely losing             #
# I don't mind losing but don't call me Shirley!"     #
#######################################################
# Let's define our losing function

losing = function( number_of_plays,number_of_doors, switch_door=TRUE){
        winning_door = sample(number_of_doors, number_of_plays, replace = T)
        selected_door = sample(number_of_doors, number_of_plays, replace = T)
        losing_door <- c(rep("FALSE",number_of_plays))
        if (switch_door){
                check_switch_and_win = (selected_door!=winning_door)
                #calculate the probability of losing
                check_switch_and_lose = (check_switch_and_win==losing_door)
                switch_and_lose=sum(check_switch_and_lose)/number_of_plays
                #variance of switching and losing :
                var_switch_and_lose = var(check_switch_and_lose*1)
                list(switch_and_lose = switch_and_lose,var_switch_and_lose = var_switch_and_lose)
        }else{
                check_dontswitch_and_win = (selected_door==winning_door)
                #calculate the probability of losing
                check_dontswitch_and_lose = (check_dontswitch_and_win==losing_door)
                dontswitch_and_lose=sum(check_dontswitch_and_lose)/number_of_plays
                #variance of switching and losing :
                var_dontswitch_and_lose = var(check_dontswitch_and_lose*1)
                list(dontswitch_and_lose = dontswitch_and_lose,var_dontswitch_and_lose = var_dontswitch_and_lose)
        }
}
#######################################################
## Let's test our probabilities                       #
#######################################################
## There are 3 doors and lets say thegame is played 1000 times, 
## Let's check the winning probability and variances for 
##switching 
winning(1000,3,TRUE)
##and not swithing
winning(1000,3,FALSE)
##Now, let's check the losing probability and variances for 
##swithing 
losing(1000,3,TRUE)
##and not swithing
losing(1000,3,FALSE)
###Let's check the relatinonship between the number of plays and the probabilities of winning
number_of_plays = 1000
number_of_doors = 3
## When the door is switched
prob_by_num_plays_switching_winning = sapply(1:number_of_plays, function(x) winning(x, number_of_doors, T))

plot(1:number_of_plays, prob_by_num_plays_switching_winning["switch_and_win",], type="p",ylim=c(0, 1),
     main = "Probability of Switching and Winning vs Number of Plays", xlab = "Number of Play", ylab= "Probability" )
grid()
abline(h=2/3)

## When the door is not switched
prob_by_num_plays_not_switching_winning = sapply(1:number_of_plays, function(x) winning(x, number_of_doors, F))

plot(1:number_of_plays, prob_by_num_plays_not_switching_winning["dontswitch_and_win",], type="p",ylim=c(0, 1),
     main = "Probability of Not Switching and Winning vs Number of Plays", xlab = "Number of Play", ylab= "Probability" )
grid()
abline(h=1/3)
#######################################################
##                       End                          #
#######################################################