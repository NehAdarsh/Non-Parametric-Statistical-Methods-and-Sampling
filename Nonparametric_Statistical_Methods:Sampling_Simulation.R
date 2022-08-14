# Steps to perform:
# a. State the hypotheses and identify the claim.
# b. Find the critical value(s).
# c. Compute the test value.
# d. Make the decision.
# e. Summarize the results.

###################################################################################################################################
###################################################################################################################################

## Example 1 : We will use the sign method to solve this probelm

'An athletic director suggests the median number for the paid attendance at 20 local football games is 3000. The data for a 
random sample are shown. At α = 0.05, is there enough
evidence to reject the claim? If you were printing the programs for the games, would you use this
figure as a guide?'

'6210 3150 2700 3012 4875
3540 6127 2581 2642 2573
2792 2800 2500 3700 6030
5437 2758 3490 2851 2720'

#State the hypothesis
#H0 : Median = 3000
#H1 : Median != 3000

# Significance level
alpha = 0.05

# claim : number for the paid attendance at 20 local football games is 3000
median = 3000

#creating vector : of random sample data
data = c(6210, 3150, 2700, 3012, 4875, 3540, 6127, 2581, 2642, 2573, 2792, 2800, 2500, 3700, 6030, 
            5437, 2758, 3490, 2851, 2720)

diff = data - median            
 
#Determining the number for the paid attendance more than 3000
#exclude : 0 values
#if value is greater than median : + sign
#if value is less than median : - sign
pos = length(diff[diff>0])

#Determining the number for the paid attendance less than 3000
neg = length(diff[diff<0])

#Running the test
res = binom.test(x = c(pos, neg), alternative = 'two.sided')
print(res)

#P-value
p_val = res$p.value

#Null hypothesis decision
ifelse(res$p.value > alpha, 'Fail to reject the Null Hypothesis', 'Reject the Null Hypothesis')

#conclusion : Not enough evidence to say that the median number for the paid attendance at 20 local football games is not 3000.

###################################################################################################################################
###################################################################################################################################

# Example 2 : We will use the sign method to solve this probelm


#A lottery outlet owner hypothesizes that she sells 200 lottery tickets a day.
#She randomly sampled 40 days and found that on 15 days she sold fewer than 200 tickets. At α = 0.05, 
#is there sufficient evidence to conclude that the median is below 200 tickets?


#State the hypothesis
#H0 : Median = 200 
#H1 : Median < 200 

# Significance level
alpha1 = 0.05

# claim : number for the paid attendance at 20 local football games is 3000
median1 = 200

#Running the test
res1 = binom.test(15, 40) 
res1
#Null hypothesis decision
ifelse(res1$p.value > alpha, 'Fail to reject the Null Hypothesis', 'Reject the Null Hypothesis')

#conclusion : Not enough evidence to say that the median the median is below 200 tickets.

###################################################################################################################################
###################################################################################################################################

# Example 3 : We will use the Wilcoxon rank sum test.


#A random sample of men and women in prison was asked to give the
#length of sentence each received for a certain type of crime. At α = 0.05, test the claim that there is
#no difference in the sentence received by each gender. The data (in months) are shown here.

#Males   8 12 6 14 22 27 32 24 26
#Females 7 5 2 3 21 26 30 9 4
#Males   19 15 13
#Females 17 23 12 11 16

#State the hypothesis
#H0 : No difference in the sentence received by each gender 
#H1 : There is a difference in the sentence received by each gender

# Significance level
alpha = 0.05

#creating vector : of random samples data
males = c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
females = c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

#run the test
res2 = wilcox.test(x = males, y =females, alternative = 'two.sided', correct = FALSE)
print(res2)

#p-value
res2$p.value

#Null hypothesis decision
ifelse(res2$p.value > alpha, 'Fail to reject the Null Hypothesis', 'Reject the Null Hypothesis')

#conclusion : Not enough evidence to say that there is a difference in the sentence received by each gender.

###################################################################################################################################
###################################################################################################################################

# Example 4 : We will use the Wilcoxon rank sum test.


#For the years 1970–1993 the National League (NL) and the American
#League (AL) (major league baseball) were each divided into two divisions: East and West. Below
#are random samples of the number of games won by each league’s Eastern Division. At α = 0.05, is
#there sufficient evidence to conclude a difference in the number of wins?

#NL 89 96 88 101 90 91 92 96 108 100 95
#AL 108 86 91 97 100 102 95 104 95 89 88 101

#State the hypothesis
#H0 : No difference in the number of wins 
#H1 : There is a difference in the number of wins

# Significance level
alpha = 0.05

#creating vector : of random samples data
nl = c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
al = c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

#run the test
res3 = wilcox.test(x = nl, y =al, alternative = 'two.sided', correct = FALSE)
print(res3)

#p-value
res3$p.value

#Null hypothesis decision
ifelse(res3$p.value > alpha, 'Fail to reject the Null Hypothesis', 'Reject the Null Hypothesis')

#conclusion : Not enough evidence to say that there is a difference in the number of wins.


###################################################################################################################################
###################################################################################################################################


# Example 6 : Use Kruskal-Wallis test

'Through the Organization for Economic Cooperation and
Development (OECD), 15-year-olds are tested in member countries in mathematics, reading, and
science literacy. Listed are randomly selected total mathematics literacy scores (i.e., both genders)
for selected countries in different parts of the world. Test, using the Kruskal-Wallis test, to see if
there is a difference in means at α = 0.05.'

#Western Hemisphere Europe Eastern Asia
#       527          520        523
#       406          510        547
#       474          513        547
#       381          548        391
#       411          496        549


#State the hypothesis
#H0 : There is no difference in mathematical literacy scores of all groups
#H1 : There is a difference in mathematical literacy scores of all groups

# Significance level
alpha = 0.05

#creating dataframe of all the groups
groupA = data.frame(test = c(527, 406, 474, 381, 411), group = rep('groupA', 5))
groupB = data.frame(test = c(520, 510, 513, 548, 496), group = rep('groupB', 5))
groupC = data.frame(test = c(523, 547, 547, 391, 549), group = rep('groupC', 5))

#merging all the dataframes into one
df = rbind(groupA, groupB, groupC)

#running the test
result = kruskal.test(test ~ group, data = df)
print(result)

#p-value
result$p.value

#Null hypothesis decision
ifelse(result$p.value > alpha, 'Fail to reject the Null Hypothesis', 'Reject the Null Hypothesis')

#conclusion : Not enough evidence to say that there is a difference in means of all three samples.

###################################################################################################################################
###################################################################################################################################

# Perform these steps.
# Find the Spearman rank correlation coefficient.
# State the hypotheses.
# Find the critical value. Use α = 0.05.
# Make the decision.
# Summarize the results.

# Example 7 : Use Kruskal-Wallis test

'Six cities are randomly selected, and the number of
daily passenger trips (in thousands) for subways and commuter rail service is obtained. At α = 0.05,
is there a relationship between the variables? Suggest one reason why the transportation authority
might use the results of this study.'

# City   1 2 3 4 5 6
# Subway 845 494 425 313 108 41
# Rail   39 291 142 103 33 38

# State the hypothesis
# H0 : No relationship between variables
# H1 : There is a relationship between the variables

# Significance level
alpha = 0.05

#create vectors for each of the variables
city = c(1,2,3,4,5,6)
subway = c(845, 494, 425, 313, 108, 41)
rail = c(39, 291, 142, 103, 33, 38)

#combining the vectors into a single dataframe
df1 = data.frame(city = city, subway = subway, rail =rail)

result1 = cor.test(x = df1$subway, y = df1$rail, method = 'spearman')
print(result1)

#p value and test value
result1$p.value
result1$estimate

#Null hypothesis decision
ifelse(result1$p.value > alpha, 'Fail to reject the Null Hypothesis', 'Reject the Null Hypothesis')

#conclusion : Not enough evidence to say that there is a relationship between the variables.


###################################################################################################################################
###################################################################################################################################

' Use random numbers to simulate the experiments. The number in parentheses is 
the number of times the experiment should be repeated.'

#Example 8 : 
' A caramel corn company gives four different prizes, one in each box. 
They are placed in the boxes at random. Find the average number of boxes a person needs to
buy to get all four prizes. (40)'

#Random numbers & average
random = runif(40, min=1, max=4)  # define the range between 1 to 3
sum(random)/40


###################################################################################################################################
###################################################################################################################################

#Example 8 : 
'To win a certain lotto, a person must spell the word big. Sixty percent of the
tickets contain the letter b, 30% contain the letter i, and 10% contain the letter g. Find the average
number of tickets a person must buy to win the prize. (30)'

#List all possible outcomes : 60, 30, 10
#Assign the probabilities to each outcome: 60/100 30/100 10/100
#Set up a correspondence between the random numbers and the outcomes. Use random numbers 1 through 6 to represent a 60% being selected, 
       #7 through 9 to represent a 30% being selected, and 9 and 0 to represent 10%
#Select 30 random numbers and tally the results.
ran = runif(30, min=1, max=10) # define the range between 1 to 10
#Compute the average:
sum(ran)/30

###################################################################################################################################
###################################################################################################################################

