'''
The following script is used to provide a final cleaning pass on our
merged dataset, adjusting the data types for our columns that represent
the unit of analysis. It also takes in the running variable (grade_distance)
and uses it to create our target column (outcome). The target column is a 
binary indicator, determining if an Illinois county for a given day is
compliant with the shelter in place order based on the grade they receive 
from UNACAST on their total mobility. 

Specifically, the script does the following:
1. clean the country string column
2. narrows the dataset to the most relevant columns
3. creates the target column: 1s repesent compliance, 0s non-compliance
'''

import pandas as pd
import numpy as np

df = pd.read_csv('merged_features_outcomes.csv')
df['county.x'] = df['county.x'].apply(lambda x: x.capitalize())
df['county.x'] = df['county.x'].astype(str) + ' County'

cols = ['county.x', 'date', 'device_count', 'devices_leaving_home', 'median_home_dwell_time', 'median_non_home_dwell_time',
	'share_asian', 'share_black', 'share_white', 'share_hhinc_100k', 'share_poverty', 'share_public_transit',
	'precip', 'tmin', 'tmax', 'county_sip', 'popestimate2019', 'cases', 'deaths', 'share_over_65', 'trump_vote_share',
	'county_republican', 'grade_distance', 'school_closure','outcome']


df['outcome'] = np.where(df['grade_distance'].isin(['A','B','C']), 1, 0)
df = df[cols]

df.to_csv('clean_merged_with_outcome.csv')
print('done')