'''
get_tweets_by_county.py
Last modified: 6/8/2020

Purpose: use the "get old tweets" library to pull historical tweets
from each of Illinois' counties during the period of analysis
'''

import sys
import twitter
import pandas as pd
import util
import GetOldTweets3 as got
import datetime
import time
from datetime import timedelta, date

# Create API object for accessing the Twitter API when necessary
API_OBJECT = twitter.Twitter(auth=twitter.OAuth(
    util.ACCESS_TOKEN, util.ACCESS_SECRET, util.CONSUMER_KEY, util.CONSUMER_SECRET), retry=True)

# Save locations
INPUT_FILE = "../Data/illinois_pop_centroids.csv"
OUTPUT_FILE = "../Data/tweets_"

# Correlated keywords for narrowing Twitter results
KEYWORDS = [['coronavirus', 'covid', 'sars-cov-2'],
    ['epidemic', 'pandemic', 'outbreak', 'virus'],
    ['shelter', 'social', 'distancing', 'stay home', 'safer'],
    ['lockdown', 'quarantine', 'pritzker', 'lightfoot', 'reopen', 'shutdown']]


def daterange(start_date, end_date):
    for n in range(int((end_date - start_date).days)):
        yield start_date + datetime.timedelta(n)


def get_old_tweets():
    """ 
    Load the dataframe of county population centroids, then, for each county,
    use GetOldTweets3 to pull historical tweets from locations near the latitude
    and longitude coordinates of the county population centroids.

    For large counties (population > 100,000), tweets will be limited to search
    terms that are related to coronavirus and the responses to it. For smaller 
    counties, a random sample of tweets will be pulled (because there aren't
    enough twitter users or tweets to construct an adequately-sized sample).
    """
    obs = []
    centroids = pd.read_csv(INPUT_FILE, header='infer')
    for county in centroids.iterrows():
        i = 1
        j = 1
        print("Loading tweets for", county[1]['CONAME'], "...")
        lat = str(county[1]['LATITUDE'])
        lon = str(county[1]['LONGITUDE'])
        # Build latitude and longitude coordinates for GetOldTweets search
        coords = lat + "," + lon
        print("Centroids:", lat, "N", lon, "W")

        if county[1]['POPULATION'] > 100000:
            # For high-population counties, limit returned tweets to those corresponding to 
            # coronavirus-related terms
            for subset in KEYWORDS:
                print("     Searching for tweets in term subset", j, ". Building tweet criteria...")

                tweetCriteria = got.manager.TweetCriteria().setQuerySearch(" OR ".join(subset))\
                                                           .setNear(coords)\
                                                           .setWithin("15mi")\
                                                           .setSince('2020-01-26')\
                                                           .setUntil('2020-06-07')\
                                                           .setMaxTweets(int(5000))

                print("     Constructing tweet manager...")
                while True:
                    try:
                        tweets = got.manager.TweetManager.getTweets(tweetCriteria)
                    except:
                        time.sleep(i*10)
                        i += 1
                        print("     Sleeping", i*10, "seconds...")
                        continue
                    break
                print("     Appending tweets to list...")
                for tweet in tweets:
                    obs.append([county[1]['CONAME'], tweet.id, tweet.username, tweet.date, tweet.text])
                j += 1
                time.sleep(120)
        else:
            # For small population counties, just scrape all their tweets - there aren't
            # enough twitter users or tweets to subset the universe of tweets to only
            # coronavirus-related topics
            print("     Getting all tweets for low-population county...")
            # Do a monthly scraping
            dates = [('2020-01-26', '2020-02-29'), ('2020-03-01','2020-03-31'),
                ('2020-04-01', '2020-04-30'), ('2020-05-01','2020-05-31')]
            for date_range in dates:
                print("         Tweets from", date_range[0], "to", date_range[1])
                tweetCriteria = got.manager.TweetCriteria().setNear(coords)\
                                                           .setWithin("5mi")\
                                                           .setSince(date_range[0])\
                                                           .setUntil(date_range[1])\
                                                           .setMaxTweets(int(1000))

                print("     Constructing tweet manager...")
                while True:
                    # Try-except block to handle any HTTP errors
                    try:
                        tweets = got.manager.TweetManager.getTweets(tweetCriteria)
                    except:
                        time.sleep(i*5)
                        i += 1
                        print("     Sleeping", i*5, "seconds...")
                        continue
                    break
                print("     Appending tweets to list...")
                # For all tweets, create a list object that will correspond to 
                # observations in the output dataframe
                for tweet in tweets:
                    obs.append([county[1]['CONAME'], tweet.id, tweet.username, tweet.date, tweet.text])
                time.sleep(60)

        # Build dataframe of tweets for the county, write to CSV, and clear observations
        # to make room for tweets from next county
        print("     Constructing full tweets df for", county[1]['CONAME'], "County.")
        df = pd.DataFrame(obs, columns=['county', 'id', 'username', 'date', 'text'])
        outfile = OUTPUT_FILE + county[1]['CONAME'] + ".csv"
        df.to_csv(outfile, index=False)
        obs = []
        print("----------------------------------------------")
    


def go():
    '''
    Helper function for calling script from command line. 
    '''
    get_old_tweets()
    


if __name__ == "__main__":
    go()


