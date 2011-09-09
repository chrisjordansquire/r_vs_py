"""
This is an experiment to see how easily an analysis
done in R could be done in Python. The rcode is in a
companion file. The comments are cut and pasted from
the R code, and similar variable names are used as
much as possible to make comparisons easier.
"""

#An example analysis of a fairly simple dataset.
#The data is from the 2010 CPS March supplement.
#(Also called the Annual Social and Economic Supplement.)
#The goal is examining the difference in hourly pay
#between males and females. The column A_ERNLWT is
#survey weights, PTFT is part-time/full-time status,
#educ is education level, ind is what industry the
#person works in and occ is what occupation they work in.
#A full description of most of the variables sometimes with
#slightly different names, can be found at
#http://www.census.gov/apsd/techdoc/cps/cpsmar10.pdf



import numpy as np
import scikits.statsmodels.api as sm
import matplotlib.pyplot as plt
import scipy.stats as sps

dat = np.genfromtxt('marchCPS_2010.txt', names=True, dtype=None,
                        missing_values='NA')

print dat.shape
print dat.dtype
print len(dat.dtype)


####Actual analysis code

#Remove everyone that doesn't have an houlry wage data



indhr = ~np.isnan(dat['hrwage'])
hrdat = dat[indhr]


#Remove several people who just didn't fit the models (using the
#standard model checking techniques below)


hrdat = np.delete(hrdat, [407, 515, 852, 1197])


indf = np.flatnonzero(dat['sex'] == 2)
indm = np.flatnonzero(dat['sex'] == 1)

print len(indf)
print len(indm)

#With each of these models, typically do some
#commands to look more at the models, like summary(),
#, anova for the model on its own or betwen two models to see
#how much additional explantory power you get with the added
#variables, and plots to look at residuals, qqplot, and hist of residuals
#Currently can't do anova or lowess in python, and the qqplots are annoying
#to make.


#Initial model, only look at log(hrwage)~sex
X1 = hrdat['sex']==2
X1 = sm.add_constant(X1, prepend=True)
model1 = sm.WLS(np.log(hrdat['hrwage']), X1, weights = hrdat['A_ERNLWT'])
results1 = model1.fit()

print results1.summary()


#Pre-defining model matrix components for more complicated models
#dat_mat is DATa model MATtrices
n = len(hrdat)
dat_mat = {}
dat_names = {}
factor_vars = ['sex', 'educ', 'PTFT', 'ind', 'occ', 'marstat',
            'GEDIV', 'race', 'hispanic', 'disabled']
for name in factor_vars:
    dat_mat[name],dat_names[name] = sm.categorical(hrdat[name],
                                                   dictnames=True)
    dat_mat[name] = dat_mat[name][:,2:]
dat_mat['age'] = hrdat['age'].reshape(n,1)
dat_mat['age^2'] = (hrdat['age']**2).reshape(n,1)
dat_mat['const'] = np.ones((n,1))
dat_names['age'] = ['age']
dat_names['age^2'] = ['age^2']
dat_names['const'] = ['const']

for name in factor_vars:
    fact_names = sorted(dat_names[name].values())[1:]
    dat_names[name] = [''.join([name, str(val)]) for
                            val in fact_names]

#helper function to spit out design matrix and names

def get_mat(var_list):
    terms = map(dat_mat.get, var_list)
    mat = np.hstack(tuple(terms))

    return mat

def get_names(var_list):
    names = []
    map(names.extend, map(dat_names.get, var_list))

    return names

#More complicated model, log(hrwage)~sex+educ+age+PTFT
logwage = np.log(hrdat['hrwage'])
w = hrdat['A_ERNLWT']

m2_vars = ['const', 'sex', 'educ', 'age', 'PTFT']

X2 = get_mat(m2_vars)
m2_names = get_names(m2_vars)

model2 = sm.WLS(logwage, X2, weights = w)
results2 = model2.fit()

print results2.summary(xname=m2_names)



#Now include ind and occ (industry and occupation codings)

m2_5_vars = list(m2_vars)
m2_5_vars.extend(['ind','occ'])
m2_5_vars.insert(4, 'age^2')

X2_5 = get_mat(m2_5_vars)
m2_5_names = get_names(m2_5_vars)

model2_5 = sm.WLS(logwage, X2_5, weights=w)
results2_5 = model2_5.fit()

print results2_5.summary(xname=m2_5_names)


#Residual diagnostics for model2_5

plt.subplot(1,3,1)
plt.hist(results2_5.resid, 30, normed=1, facecolor='green', alpha=0.75)
plt.title('Histogram of Residuals')
plt.xlabel('Probability')
plt.grid(True)

plt.subplot(1,3,2)
plt.plot(results2_5.fittedvalues, results2_5.resid, 'bo')
plt.title('Residuals vs. Fitted Values')
plt.xlabel('Fitted Values')
plt.ylabel('Residuals')
plt.axhline(lw=3, color = 'r')
plt.grid(True)

plt.subplot(1,3,3)
normal_rv_q = sps.norm.ppf(np.linspace(0,1,n+1)[1:])
ordered_resid = np.copy(results2_5.resid)
ordered_resid.sort()
plt.plot(normal_rv_q, ordered_resid, 'ro')
plt.title('Normal qq Plot')
plt.xlabel('Theoretical Quantiles')
plt.ylabel('Sample Quantiles')
plt.grid(True)


#Used mean replacement for all the people whose hours varied,
#where the mean of 22 was the mean over all part time workers
#w/o missing data and using their weights
#Done just as a sanity check on the above models



#Throw in everything and the kitchen sink for reality check
indic = np.flatnonzero(hrdat['PMHRUSLT']<=0)
tmp = np.copy(hrdat['PMHRUSLT'])
tmp[indic] = 22
tmp = tmp.reshape(n,1)

m3_vars = ['const','sex', 'educ', 'PTFT', 'age', 'age^2',
            'marstat', 'GEDIV', 'race', 'hispanic',
            'tmp', 'disabled']
dat_mat['tmp'] = tmp
dat_names['tmp'] = ['Varying Hours']

X3 = get_mat(m3_vars)
m3_names = get_names(m3_vars)

model3 = sm.WLS(logwage, X3, weights=w)
results3 = model3.fit()

print results3.summary(xname=m3_names)


#These models bin the works by age group, <=30, 31-40, 41-50, >50.
#This was done to see what the difference betweens males and
#females was in each bin. This was done as a sanity check for the
#later exploratory analysis that fit lowess curves across age to
#males and females seperately. I wanted to make sure that the trends
#observed were real. (Where the gap was smaller for younger workers,
#expanded for middle age workers, and then contracted again.)


lt30 = (hrdat['age']<=30)
btw30_40 = (np.logical_and(30 <hrdat['age'], hrdat['age']<=40))
btw40_50 = (np.logical_and(40< hrdat['age'], hrdat['age']<=50))
gt50 = (50<hrdat['age'])

#Earlier design matrix helper function doesn't apply because it
#doesn't allow for taking indices
#Earlier name helper function also doesn't apply because some of the factors
#in the subset of data considered have fewer levels than in the full dataset
def age_design(indices):
  tmp = np.hstack((sm.categorical(hrdat['sex'][indices])[:,2:],
                  sm.categorical(hrdat['educ'][indices])[:,2:],
                  sm.categorical(hrdat['PTFT'][indices])[:,2:],
                  hrdat['age'].reshape(n,1)[indices,:],
                  (hrdat['age']**2).reshape(n,1)[indices,:]))
  return sm.add_constant(tmp, prepend = True)


def age_model(indices):
  return sm.WLS(logwage[indices], age_design(indices), weights = w[indices])

model7_1 = age_model(lt30)
model7_2 = age_model(btw30_40)
model7_3 = age_model(btw40_50)
model7_4 = age_model(gt50)

results7_1 = model7_1.fit()
results7_2 = model7_2.fit()
results7_3 = model7_3.fit()
results7_4 = model7_4.fit()

print results7_1.summary()
print results7_2.summary()
print results7_3.summary()
print results7_4.summary()

####


#This analysis switches gears and focuses on occupation.
#The males and females are broken down by occupation and the
#weighted mean of their hourly wage are compared.
#(The weights used are again the survey weights.)
#The matrix wocc stores all of that, and
#Use xtable to format tables in latex

occs = sorted(set(hrdat['occ']))
wocc = np.zeros( (len(occs), 4))
for i,occ in enumerate(occs):
  tmp_male = np.logical_and(hrdat['sex']==1, hrdat['occ'] == occ)
  tmp_female = np.logical_and(hrdat['sex']==2, hrdat['occ'] == occ)
  if np.any(tmp_male):
    wocc[i, 0] = sum(hrdat['A_ERNLWT'][tmp_male])
    wocc[i, 1] = np.average(hrdat['hrwage'][tmp_male],
        weights = hrdat['A_ERNLWT'][tmp_male])
  if np.any(tmp_female):
    wocc[i, 2] = np.sum(hrdat['A_ERNLWT'][tmp_female])
    wocc[i, 3] = np.average(hrdat['hrwage'][tmp_female],
                              weights = hrdat['A_ERNLWT'][tmp_female])

print wocc

# In the next iteration of pandas the above computations can be done more
# concisely and in a more readable manner.
# The following example code showing this is due to Wes McKinney
#
# hrdf = pa.DataFrame(hrdat)
# hrdf['sex'] = np.where(hrdf['sex'] == 1, 'male', 'female')
#
# def compute_stats(group):
#     sum_weight = group['A_ERNLWT'].sum()
#     wave_hrwage = (group['hrwage'] * group['A_ERNLWT']).sum()/sum_weight
#     return Series({'sum_weight':sum_weight, 'wave_hrwage':wave_hrwage})
#
# pawocc = hrdf.groupby(['sex','occ']).apply(compute_stats)



#These were just some quick computations to make sure wocc had
#the values I thought it had. Nothing more embarassing than
#bad statistics because you didn't double-check your output.

males = hrdat['sex']==1
females = hrdat['sex'] ==2

np.average(hrdat['hrwage'][males])
np.average(hrdat['hrwage'][males],
            weights = hrdat['A_ERNLWT'][males])
sum(wocc[:,0]*wocc[:,1])/sum(wocc[:,0])
np.average(hrdat['hrwage'][females],
            weights = hrdat['A_ERNLWT'][females])
tmp = np.delete(wocc, 5, axis=0)
sum(tmp[:,0] * tmp[:,3]) / sum(tmp[:,0])


"""
The final analysis using lowess can't be done in python (yet!)
since there's no analogous function in the numpy/scipy stack. Alas.
"""


