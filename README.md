# Predicting Mobility in Illinois Using Twitter Data During the COVID-19 Pandemic

## Overview

This repository stores data, code and analysis for the CAPP 30255 final project for Tammy Glazer, James Jensen, and Alec MacMillen.

Our project examines metrics relating to human mobility during the COVID-19 pandemic of 2020. As COVID-19 spread throughout the United States, various federal, state and local officials implemented policy responses including "shelter-in-place orders" (SIPOs) to attempt to reduce the spread of the disease. Although many Americans complied with SIPOs in the spirit of attempting to reduce community spread, some citizens resisted the restrictions imposed by such policies and continued their regular activities. We are interested in using Twitter data to predict human mobility at the county-day unit of observation, under the hypothesis that popular sentiment about COVID-19 and policy response measures expressed on Twitter might provide meaningful insight into which areas are particularly compliant or non-compliant with restrictions on mobility.

We construct a binary outcome variable based on the daily grade assigned to a county by Unacast that measures reductions in mobility against a baseline measure. Counties with lower relative mobility receive higher grades (A, B, or C) than counties with higher relative mobility (D, E, F). Our binary indicator takes a value of 1 for county-day observations with higher grades and 0 for county-day observations with lower grades. We use a suite of features including demographic, geographic, political, weather and text-based Twitter data to predict whether a given county-day observation falls into the higher- or lower-scoring group. We believe our results could be used to inform case studies about the qualities of counties that practice stronger social distancing, producing insights that could be replicated elsewhere. 

Broadly speaking, our analysis takes the following steps:

1. Gather feature data, including:
- Mobility data from Unacast for each county of Illinois' 102 counties for all days February 24 - May 17
- Twitter data on historical tweets for all Illinois county-days (manually scraped using the Twitter API and Python GetOldTweets library)
- Demographic data from the Census Bureau's 2016 5-year ACS
- 2016 political and election data from MIT's Election Lab
- Daily weather data from gridMET
- Policy response variables including indicators for an active SIPO or school closure at the county level

2. Generate features from the non-text data.

3. Run baseline models (logistic regression and random forests) for predicting mobility grades *without* textual Twitter data.

4. Produce a neural network using the Python package Keras for predicting mobility using tweets and . The network architecture has the following traits:
- One hidden layer
- ReLU activation
- Binary cross-entropy loss
- Adam optimizer

5. Compare the performance of the models built in steps (3) and (4).

We find that our best-performing baseline model produces 86.76% accuracy in predicting mobility grades for a county-day. Incorporating Twitter data improves our predictive accuracy to 87.53%.

After producing our first-run models in (3) and (4), we decided to perform robustness checks by repeating the model constructed in (4) using custom embeddings. However, our results suggested that this network architecture was overfitting to the training data, so we reverted to using the GloVe embeddings.

As a further robustness check, we used the Python package "Botometer" from the Network Science Institute at Indiana University to investigate whether the user accounts in the tweets we scraped at the county level exhibited activity similar to "bot" accounts (i.e., automated accounts not associated with a human). An audit of a 0.1% sample of the 217,000+ tweets we scraped found that approximately 16% came from bot-like accounts.

### Repository Structure

#### Data

Output of data gathering and cleaning scripts.

#### Data Preparation

Scripts for merging and cleaning non-text features; scraping Twitter data.

#### Modeling

Scripts and results for our baseline (step 3) and neural-network (step 4) models.