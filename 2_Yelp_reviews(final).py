
from textblob import TextBlob
import pandas as pd
import numpy as np
import re
import matplotlib.pyplot as plt

plt.style.use('fivethirtyeight')
import statistics


# Load full data set
df = pd.read_csv('data_combined_public3_addrows.csv')

# Format dates
df['date_tip'] = pd.to_datetime(df['date_tip'])

# Check if correct dataset is loaded
print(df.dtypes)

# Add columns for the review counts we want to find out with the sentiment analysis
df['yelp_count_pos'] = 0
df['yelp_count_neu'] = 0
df['yelp_count_neg'] = 0
df['avg_stars'] = 0
df['onestar_count'] = 0
df['fivestar_count'] = 0

print(df.head())

# Load full data set
df_review = pd.read_csv('reviewtable.csv')

df_review['date'] = pd.to_datetime(df_review['date'], yearfirst=True)

print(df_review.head())

for index in range(len(df)):
    # business_id of the restaurant
    business_id = df.iloc[index][0]
    # latest tip date
    date = df.iloc[index][2]

    # Filter review table for business and date of interest
    mask = ((df_review['date'] <= date) & (df_review['business_id'] == business_id))
    df_yelp = df_review[mask]


    # Clean the text

    # Create a function to clean the reviews

    def cleanTxt(text):
        text = re.sub(r'@[A-Za-z0-9]+', '', text)  # Remove @mentions
        text = re.sub(r'#', "", text)  # Removing the '#' symbol
        text = re.sub(r'RT[\s]+', '', text)  # Removing RT
        text = re.sub(r'https?:\/\/\S+', '', text)  # Remove the hyper link

        return text


    # Cleaning the text
    df_yelp['text'] = df_yelp['text'].apply(cleanTxt)


    # Show the cleaned text
    # print(df_yelp)

    # Create a function to get the subjectivity

    def getSubjectivity(text):
        return TextBlob(text).sentiment.subjectivity


    # Create a function to get the polarity
    def getPolarity(text):
        return TextBlob(text).sentiment.polarity


    # Create two new columns
    df_yelp['Subjectivity'] = df_yelp['text'].apply(getSubjectivity)
    df_yelp['Polarity'] = df_yelp['text'].apply(getPolarity)


    # Show the new dataframe with the new columns
    # print(df_yelp)

    # Create a function to compute the negative, neutral and positive analysis

    def getAnalysis(score):
        if score < 0:
            return 'Negative'
        elif score == 0:
            return 'Neutral'
        else:
            return 'Positive'


    df_yelp['Analysis'] = df_yelp['Polarity'].apply(getAnalysis)

    # Show the dataframe
    # print(df_yelp.head())

    # Count positive, neutral values
    count_pos = sum(df_yelp['Analysis'] == "Positive")
    count_neu = sum(df_yelp['Analysis'] == "Neutral")
    count_neg = sum(df_yelp['Analysis'] == "Negative")

    if len(df_yelp['stars']) > 0:
        avg_star = statistics.mean(df_yelp['stars'])
        onestar_count = sum(df_yelp['stars'] == 1)
        fivestar_count = sum(df_yelp['stars'] == 5)

    # Add values to original df

    # Add positive count to original df
    df.iat[index, 16] = count_pos

    # Add neutral count to original df
    df.iat[index, 17] = count_neu

    # Add positive count to original df
    df.iat[index, 18] = count_neg

    # Add avg stars to original df
    df.iat[index, 19] = avg_star

    # Add onestar review count to original df
    df.iat[index, 20] = onestar_count

    # Add fivestar review to original df
    df.iat[index, 21] = fivestar_count

    count_pos = 0
    count_neu = 0
    count_neg = 0
    avg_star = 0
    onestar_count = 0
    fivestar_count = 0

    print("Updated data for {} restaurants".format(index))

# Save final results
df.to_pickle('{}.pkl'.format('restaurant_review_results'))

