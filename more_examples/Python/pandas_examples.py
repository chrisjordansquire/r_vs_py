

import numpy as np
import scikits.statsmodels.api as sm
import matplotlib.pyplot as plt
import scipy.stats as sps
import pandas as pa



"""
Load in the data as in the other files, but then we stick it
into a pandas DataFrame. We then use the new 0.4 pandas to
generate some simple summary statistics to understand the data.

The major new feature in pandas 0.4 is the multi-index, and associated
functionality to manipulate them. In a nutshell:

    *The multi-index is a tuple of objects to index on. This is convenient
        for categorical or binned variables. (e.g. race, education level,
        age range, weight range, etc.)
    *These multi-indexes can be used to index both rows and columns.
    *The pivot command lets you take a column of data and convert it
        to an index in a multi-index.
    *The stack/unstack commands let you re-organize indices between
        indexing rows or columns.
    *The groupby functionality plays well with the new multi-indices,
        exactly as one would hope. In particular, you can also
        use groupby on these new multi-indexes.

This new multi-index functionality is very nice for replicating the
sort of split-apply-combine functionality that can be accomplished
in SQL or using the plyr package in R.

Some potential gotchas I ran into while creating this were:

    *Thus far, groupby can't respect any non-lexigraphic ordering
        on grouping variables. So when grouping on ordinal variables
        you must ensure the lexigraphic ordering is the same as
        the natural ordering if you want groupby to respect it.
    *If you want the number of items in each group you must use
        apply instead of aggregate. aggregate will again return
        a DataFrame, whereas you just want a Series. This is true
        in general if your desired aggregation would yield the same
        value across all columns.

In the following I freely make use of awful, awful names simply
because in the interpreter they're easy to remember and type.
I do this just because that's a not unreasonable style for iteractive
use. b in front of a variable I create denotes binned, g denotes
a groupby.
"""

# Load and clean the data, removing a few people that don't seem
# to fit most reasonable models.

dat = np.genfromtxt('marchCPS_2010.txt', names=True, dtype=None,
                        missing_values='NA')
# Remove everyone without hourly wage data
indhr = ~np.isnan(dat['hrwage'])
hrdat = dat[indhr]
hrdat = np.delete(hrdat, [407, 515, 852, 1197])


###
# Create a dataframe and start the exploratory analysis
###

d = pa.DataFrame(hrdat)

# The names corresponding to the numerical codes
sex_names = {1:'Male', 2:'Female'}
race_names = {1:'White', 2:'Black', 3:'First Nation', 4:'Asian', 6:'Mixed'}
ptft_names = {1:'1-Part time', 2:'2-Full time'}
occ_names = {1:'Management', 2:'Professional', 3:'Service', 4:'Sales',
                5:'Office Support', 7:'Construction',
                8:'Maintenance', 9:'Production', 10:'Transportation'}
b_age_names = {0:'18-24', 1:'25-29', 2:'30-34', 3:'35-39', 4:'40-44',
                5:'45-49', 6:'50-54', 7:'55-59',8:'60+'}

# Function to create 5-ish year bins for ages
def time_bucket(age):
    return np.digitize(age, [25,30,35,40,45,50,55,60])

# Function to bucket similar education levels
# Leadning integers are adding to force groupby to use the correct
# ordering, as these variables have an order.
def educ_bucket(educ):
    if educ<39:
        return '1:<HS'
    elif educ==39:
        return '2:HS'
    elif educ==40:
        return '3:Some College'
    elif educ in [41,42]:
        return '4:Associate'
    elif educ == 43:
        return '5:Bachelor'
    elif educ > 43:
        return '6:Grad/Prof'

# Replace with numerical codes with the corresponding string names
d['sex'] = d['sex'].apply(sex_names.get)
d['race'] = d['race'].apply(race_names.get)
d['PTFT'] = d['PTFT'].apply(ptft_names.get)
d['occ'] = d['occ'].apply(occ_names.get)

# b for binned
d['bage'] = d['age'].apply(time_bucket)
d['bage'] = d['bage'].apply(b_age_names.get)
d['beduc'] = d['educ'].apply(educ_bucket)

# Get rid of the long, obscure name for the weights
d['wt'] = d['A_ERNLWT']
del d['A_ERNLWT']

# age, sex groupby
gas = d.groupby(['bage', 'sex'])
# age, ptft, sex groupby
gaps = d.groupby(['bage','PTFT', 'sex'])
# occ, sex groupby
gos = d.groupby(['occ','sex'])
# educ, sex groupby
ges = d.groupby(['beduc','sex'])

def mf_ratio(df):
    df['ratio'] = df['Female']/df['Male']
    return df

def wt_avg(group):
    return np.average(group['hrwage'], weights = group['wt'])

# I suspect the following scheme leaves something to be desired numerically.
# It was just translated verbatim from wikipedia's formula for the weighted
# sample variance
def wt_std(group):
    mean = np.average(group['hrwage'], weights = group['wt'])
    sos = np.average((group['hrwage'] - mean)**2, weights=group['wt'])
    wt_sum = group['wt'].sum()
    wt_sos = (group['wt']**2).sum()
    return np.sqrt((wt_sum)/(wt_sum**2 - wt_sos) * sos)

# Poke around each of the groupby's.

# How many observations, unweighted, for each combination
print gas.apply(len).unstack()
print gaps.apply(len).unstack(level=1).unstack()
print gos.apply(len).unstack()
print ges.apply(len).unstack()

# How many observations according to the weights
print gas['wt'].sum().unstack()
print gaps['wt'].sum().unstack(level=1).unstack()
print gos['wt'].sum().unstack()
print ges['wt'].sum().unstack()

# First look at the weighted mean for each
print gas.apply(wt_avg).unstack()
print gaps.apply(wt_avg).unstack(level=1).unstack()
print gos.apply(wt_avg).unstack()
print ges.apply(wt_avg).unstack()

# Then look at the weighted sample stardard errors
#   (Turns out that because of the weights they're all tiny,
#   so there's no need to think about the confidence intervals.
#   But it would be simple to create a function that returned confidence
#   intervals as a tuple instead of a scalar point estimate.)
print gas.apply(wt_std).unstack()
print gaps.apply(wt_std).unstack(level=1).unstack()
print gos.apply(wt_std).unstack()
print ges.apply(wt_std).unstack()

# Take a closer look at how the ratios change
# Dealing with the multi-index is slightly ungainly, and can probably
# be improved.
print mf_ratio(gas.apply(wt_avg).unstack())
print mf_ratio(gaps.apply(wt_avg).unstack()).stack().unstack(level=1).unstack()
print mf_ratio(gos.apply(wt_avg).unstack())
print mf_ratio(ges.apply(wt_avg).unstack())



