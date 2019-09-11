# StackExchange Data Analysis

## Data Source: 

This dataset was queried from [stackexchange.com](https://meta.stackexchange.com/questions/2677/database-schema-documentation-for-the-public-data-dump-and-sede).

It consists of two different csv files. 
- One containing the questions posts
- Another containing the answer posts

## Contributors:

This project was worked upon by the following 4 members together as a team:
- Siddharth Suresh
- Sneha Choudhary
- Suchetha Sharma
- Vidhi Gupta

## Context:

The intent of the project was to use regression modeling to:

1. Determine the parameters that contributed to a higher probability of getting an accepted answer for a question
2. Predict the score of a posted Answer

## Conclusions from the analysis:

1. **Determine the parameters that contributed to a higher probability of getting an accepted answer for a question**

- Increase in Score, ViewCount, PostLength_Words, InactiveSince_Days increases the odds of getting an accepted answer.
- Increase in AnswerCount, CommentCount and MeanAnswerTime_Days decreases the odds of getting an accepted answer.
- If a question is ever marked favorite, it increases the odds of getting an accepted answer.
- Questions belonging to <bayesian> , <neural-network> and <random> tag have higher odds of getting an accepted answer.
- Questions belonging to <regression> and <machine-learning> have lower odds of getting an accepted answer.

2. **Predicting the score of a posted Answer**

The entire Dataset was split into Training (70%) and Testing (30%) Dataframe. Min-Max accuracy method was implemented which is the average between the minimum and the maximum prediction of the model.

The result achieved was 91.6% implying that the model is statistically significant.