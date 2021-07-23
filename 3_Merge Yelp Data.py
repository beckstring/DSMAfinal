import pandas as pd
import numpy as np
import re
import matplotlib.pyplot as plt

# Load full DSMA data set
dsma_df = pd.read_csv('DailyLevel_data_v2.csv')
dsma_df['date'] = pd.to_datetime(dsma_df['date'])
dsma_df = dsma_df.rename(columns={"date": "date_tip"})

print(dsma_df.head())

# Load full twitter data set
#!pip3 install pickle5
#import pickle5 as pickle
import pickle
with open('twitter_restaurant_results_full_data.pkl', "rb") as fh:
  twitter_df = pickle.load(fh)

twitter_df = twitter_df.rename(columns={"count_pos": "twitter_count_pos", "count_neu": "twitter_count_neu", "count_neg" : "twitter_count_neg"})
twitter_df = twitter_df.drop(columns=['hashtag'])
twitter_df['date_tip'] = pd.to_datetime(twitter_df['date_tip'])

print(twitter_df.head())

# Load review df
review_df = pd.read_pickle('restaurant_review_results.pkl')
# Keep only relevant columns
review_df = review_df[['business_id','date_tip','yelp_count_pos','yelp_count_neu', 'yelp_count_neg', 'avg_stars', 'onestar_count', 'fivestar_count']]
# Convert date column to date object
review_df['date_tip'] = pd.to_datetime(review_df['date_tip'])
print(review_df.head())

# Add data from
merged_df1 = pd.merge(left=dsma_df, right=twitter_df, left_on=['business_id','date_tip'], right_on=['business_id','date_tip'])
merged_df2 = pd.merge(left=merged_df1, right=review_df, left_on=['business_id','date_tip'], right_on=['business_id','date_tip'])

merged_df2 = merged_df2.rename(columns={"date_tip": "date"})

merged_df2.to_csv('dsma_df_merged.csv')

print(merged_df2.head())
