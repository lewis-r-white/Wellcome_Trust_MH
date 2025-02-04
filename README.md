# Wellcome_Trust_MH

## Preliminary Analysis of Heat Impact on Mental Health in Pregnancy Cohort

This analysis was incorporated into a grant proposal that focuses on analyzing the impact of temperature on mental health, identifying the mechanisms driving this impact, and test an intervention system where pregnant women receive additional screening at a routine pregnancy healthcare visit. 

### Background on the topic

In Ghana, estimates of maternal depression range from 18–50%——higher rates observed in rural areas where access to healthcare is more limited——with 13–17% reporting suicidal ideation. Anxiety disorders are also prevalent but often go undiagnosed due to stigma and insufficient screening methods. There is currently no routine perinatal screening for depression or anxiety in Ghana, as is typical for Sub-Saharan Africa. 

### Data used for analysis 

#### GRAPHS Pregnancy Cohort Data
The Ghana Randomized Air Pollution and Health Study (GRAPHS) focuses on a pregnancy cohort comprised of rural/agrarian communities in Ghana. For this analysis, there are a few measures related to mental health that we focus on.

1. **Perceived Stress Scale**: This survey asked respondents a series of four questions aimed to determine how in control of life respondents felt. A final PSS score was calculated from the responses.
2. **Crisis in Family Systems-Revised survey (CRYSIS-R)**: Respondents were asked if they had experienced an event in the previous 6 months and if yes, whether the event was a positive, negative, or neutral experience for them. Events were categorized into 12 domains, and an NDS score was calculated by adding together each domain where the respondent experienced a negative event. The score was then split into three groupings: low stress (NDS = 0-2), moderate stress (NDS = 3-5), and high stress (NDS > 5).
3. **Actigraphy data**: Participants wore actigraphy monitors to measure sleep quality and quantity. For this analysis, I focus on the impact of temperature on total sleep time. 

#### Temperature Data 
The proposed intervention in the grant proposal focused on high-temperature events, so daily maximum temperatures were used for this analysis. Hourly ERA-5 (~25 km) data was used to create a dataset of daily maximum Wet Bulb Globe Temperature (WBGT). WBGT is calculated using multiple atmospheric factors: temperature, humidity, wind speed, sun angle, and cloud cover. It is widely considered the gold standard when it comes to measuring heat stress during hot weather in direct sunlight. 

### Structure of Repository 

The repository contains R scripts and Markdown documents that clean stress/temperature data and perform a range of analyses: distributed lag models (DLMs), ordered logistic regression, and multiple regression. 

#### Key documents 

1. **clean_stress_data.R**: Loads and cleans the stress data for analysis 
2. **explore_weather_data.R**: Loads the weather data
3. **load_actigraphy_data.R**: Loads the sleep data
4. **analyses_for_proposal.Rmd**: Contains the final analyses used in the grand proposal 
5. **analyses_for_proposal.html**: Contains the output of the Rmd file

Other documents
1. **actigraphy_temp_analysis.Rmd**: contains additional analysis on the actigraphy data
2. **dlm_temp_stress.Rmd**: Contains more versions of the distributed lag model analysis
3. **dlmn_function.Rmd**: A notebook that uses a function to run a distributed lag model for a list of temperature metrics.
4. **unadjusted_regressions_function.Rmd**:  A notebook that uses a function to run regressions for a range of temperature metrics. 



