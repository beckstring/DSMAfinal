
import tweepy
from textblob import TextBlob
import pandas as pd
import numpy as np
import re
import matplotlib.pyplot as plt
plt.style.use('fivethirtyeight')
import time


# Load access keys
log = pd.read_csv('key_twitter.csv')

# Load full data set

df = pd.read_csv('data_combined_public3_addrows.csv')

df = df.sort_values(by='date_tip')

# Check if correct dataset is loaded
print(df.head())

# Load business table

business_df = pd.read_csv('businesstable.csv')

# Check if correct dataset is loaded
print(business_df.head())

# Add names from business df to df
merged_df = pd.merge(left=df, right=business_df, left_on='business_id', right_on='business_id')

# Check if merging was successful
print(merged_df.head())

# Create sub-df that will later contain our values of interest
twitter_df = merged_df[['business_id','name','date_tip']]

# Check sub-df
print(twitter_df)


#Preprocessing for hashtag search
twitter_df['hashtag'] = twitter_df.apply(lambda x: x.str.replace(' ', ''))['name']

# Add columns for the tweet counts we want to find out with the sentiment analysis
twitter_df['count_pos'] = 0
twitter_df['count_neu'] = 0
twitter_df['count_neg'] = 0

# Check whether structure of data frame is as expected
twitter_df.head()

# Twitter API credentials
# Twitter needs these to deal with the requests
consumerKey = log['key'][0]
consumerSecret = log['key'][1]
accessToken = log['key'][2]
accessTokenSecret = log['key'][3]

# Create the authentication object
authenticate = tweepy.OAuthHandler(consumerKey, consumerSecret)

# Set the access token and access token secret
authenticate.set_access_token(accessToken, accessTokenSecret)

# Create the APO object while passing in the auth information
api = tweepy.API(authenticate, wait_on_rate_limit = True)

test_df = twitter_df.copy()
#test_df = test_df[2866:]

print(test_df.head(40))

empty_results = []
count = 0

error_list = []

for index in range(len(test_df)):
  try:
    # latest tip date
    date = test_df.iloc[index][2]
    # hashtag of the restaurant
    hashtag = test_df.iloc[index][3]

    # Create a dataframe with a column called Tweets that contains the tweets for all restaurants
    df_tweet = pd.DataFrame( [tweet.text for tweet in tweepy.Cursor(api.search,q="#{}".format(hashtag),count=100, lang="en", since="{}".format(date)).items(100)], columns=['Tweets'])

    # Show the first 5 rows
    print(df_tweet.head())


    #Clean the text

    #Create a function to clean the tweets

    def cleanTxt(text):
      text = re.sub(r'@[A-Za-z0-9]+', '', text) # Remove @mentions
      text = re.sub(r'#', "", text) #Removing the '#' symbol
      text = re.sub(r'RT[\s]+', '', text) # Removing RT
      text = re.sub(r'https?:\/\/\S+', '', text) # Remove the hyper link

      return text

    # Cleaning the text
    df_tweet['Tweets'] = df_tweet['Tweets'].apply(cleanTxt)

    #Show the cleaned text
    print(df_tweet)


    # Create a function to get the subjectivity

    def getSubjectivity(text):
      return TextBlob(text).sentiment.subjectivity

    # Create a function to get the polarity
    def getPolarity(text):
      return TextBlob(text).sentiment.polarity

    #Create two new columns
    df_tweet['Subjectivity'] = df_tweet['Tweets'].apply(getSubjectivity)
    df_tweet['Polarity'] = df_tweet['Tweets'].apply(getPolarity)

    #Show the new dataframe with the new columns
    print(df_tweet)

    # Create a function to compute the negative, neutral and positive analysis

    def getAnalysis(score):
      if score < 0:
        return 'Negative'
      elif score == 0:
        return 'Neutral'
      else:
        return 'Positive'

    df_tweet['Analysis'] = df_tweet['Polarity'].apply(getAnalysis)

    # Show the dataframe
    print(df_tweet.head())

    # Count positive, neutral values
    count_pos = sum(df_tweet['Analysis'] == "Positive")
    count_neu = sum(df_tweet['Analysis'] == "Neutral")
    count_neg = sum(df_tweet['Analysis'] == "Negative")
    print(count_pos, count_neu, count_neg)



    # Add values to original df

    # Add positive count to original df
    test_df.iat[index,4] = count_pos

    # Add neutral count to original df
    test_df.iat[index,5] = count_neu

    # Add positive count to original df
    test_df.iat[index,6] = count_neg

    # Save tweets if there were any found
    if len(df_tweet) > 0:
      # Save the pulled tweets as pickle in case we need it again
      df_tweet.to_pickle('{}_{}.pkl'.format(hashtag,index))
      # Print filename
      print('Saved results for item {0} as {1}.pkl'.format(index, hashtag))
      # Increase count if tweets were found
      count += 1


    else:
      # Add hashtag name to empty results if noting was found
      empty_results.append(hashtag)
      print('No results for {0} (item {1})'.format(hashtag,index))

    # Sleep for a few seonds that API doesn't block requests

    print(date, hashtag)
    print('{} restaurants checked'.format(index))
    print('Found tweets for {} restaurants'.format(count))
    print('Could not find tweets for {} restaurants'.format(len(empty_results)))
    print('Success rate equals {}%'.format(round(((count / len(test_df))*100), 2)))
    time.sleep(1.5)

    if index % 100 == 0:
        # Save final results
        test_df.to_pickle('{}_{}.pkl'.format('twitter_restaurant_results', index))
  except Exception as e:
    print(e)
    error_list.append((test_df.iloc[index][3],e))
# Save final results
test_df.to_pickle('{}.pkl'.format('twitter_restaurant_results_full_data'))

print(error_list)
# Provide information how many restaurant names did yield results
print('\n')
print('Searched for {} restaurants \n'.format(len(test_df)))
print('Found tweets for {} restaurants \n'.format(count))
print('Could not find tweets for {} restaurants \n'.format(len(empty_results)))
print('Success rate equals {}% \n'.format(round((count / len(test_df)*100), 2)))