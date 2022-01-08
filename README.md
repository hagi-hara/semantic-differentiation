Semantic Differentiation of Early Object Words
====

## Overview
This repository includes the experimental stimuli, stan codes, graphing codes, and dataset for the following paper:

foo. (under review). When “shoe” becomes free from “putting on”: The link between early meanings of object words and object-specific actions

## Description
This repository consists of three folders, "**ExperimentalStimuli**," "**Study1**," and "**Study2**." The folders "Study1" and "Study2" correspond to codes for the cross-sectional study (Study 1), and codes longitudinal study (Study 2), respectively. Each of "Study1" or "study2" contains a csv file "data\_(study name).csv_", R codes, and a folder "Models", which contains Stan codes.

Here we show a simple folder structure.

- **ExperimentalStimuli**
  - WarmUpTrial
    - 3 video clips: 1_Dog_vs_Car.mp4, 2_Eating_vs_ChangingClothes.mp4, 3_Car_vs_Dog.mp4
  - TestTrial
    - 16 video clips: "(condition)\_(target object)\_(side of the correct answer).mp4" (e.g., Match_Shoe_Left.mp4)
- **Study1**
  - Models
  - Analysis1_Pointing_Binary_Age.R
  - Analysis1_Pointing_Binary_VocaularySize.R
  - Analysis1_PreferentialLooking_Binary_Age.R
  - Analysis1_PreferentialLooking_Binary_VocaularySize.R
  - Analysis1_PreferentialLooking_Continuous_Age.R
  - Analysis1_PreferentialLooking_Continuous_VocaularySize.R
  - Analysis2_Pointing_Binary_Age.R
  - Analysis2_Pointing_Binary_VocaularySize.R
  - Analysis2_PreferentialLooking_Binary_Age.R
  - Analysis2_PreferentialLooking_Binary_VocaularySize.R
  - Analysis2_PreferentialLooking_Continuous_Age.R
  - Analysis2_PreferentialLooking_Continuous_VocaularySize.R
  - Analysis3.R
  - data_study1.csv
- **Study2**
  - Models
  - Analysis1_PreferentialLooking_Binary.R
  - Analysis2_PreferentialLooking_Binary.R
  - Analysis3_Noun.R
  - Analysis3_Verb.R
  - data_study2.csv

In each study, we conducted mainly three kinds of analyses; "Analysis 1", "Analysis 2", and "Analysis 3" in the main text. Each analysis corresponds to R codes starting with the same name.

 In Study 1, we conducted a series of model selections while slightly changing each of the response variables or explanatory variables. In Analysis 1 and Analysis 2, the naming of the R code basically follows the rule "(analysis name)\_(response variable)\_(explanatory variable).R". When the response variable was preference looking, there were cases where the looking time proportion to the correct stimulus is treated as binary or continuous variables.

  In Study 2, the naming of the R code basically follows the rule "(analysis name)\_(response variable).R". As written in the paper, the binarized looking time proportion was treated as response variables.


## Data Structure
- data_study1.csv

| Column Name     | Variable                | Explanation                                                             |
| ----            | ----                    |   ----                                                                  |
| id              |qualitative              | Participant ID                                                          |
| TrialOrder      |quantitative (integer)   | Order of the trial (1 - 16)                                             |
| Condition       |qualitative              | Condition of the trial ("Match", "Mis", "ObjAbs", or "ObjDiff")         |
| Object          |qualitative              | Target object used in the trial ("cup" or "shoe")                       |
| CorrectSide     |qualitative              | Position of the target object ("left" or "right")                       |
| Pointing_Right  |quantitative (binary)    | Participants' pointing response (Leftt=0; Right=1)                      |
| Correct_Pointing|quantitative (binary)    | Whether or not the participant pointed to the correct stimulus          |
| PropLook_Correct|quantitative (continuous)| Proportion of time that the participant looked at the correct stimulus  |
| PropLook_Right  |quantitative (continuous)| Proportion of time that the participant looked at the right side        |
| Correct_Looking |quantitative (binary)    | Whether or not the participant preferred to look at the correct stimulus|
| sex             |qualitative              | Participants' gender ("f" = female; "m" = male)                         |
| ageM            |quantitative (integer)   | Participants' age in months                                             |
| vcbAll          |quantitative (integer)   | Participants' total vocabulary size                                     |
| noun            |quantitative (integer)   | Vocabulary size of common nouns                                         |
| verb            |quantitative (integer)   | Vocabulary size of verbs                                                |  

  
- data_study2.csv

| Column Name     | Variable                | Explanation                                                              |
| ----            | ----                    |   ----                                                                   |
| id              |qualitative              | Participant ID                                                           |
| TrialOrder      |quantitative (integer)   | Order of the trial (1 - 16)                                              |
| time            |quantitative (integer)   | Number of times of data collection (1 or 2)                              |
| Condition       |qualitative              | Condition of the trial ("Match", "Mis", "ObjAbs", or "ObjDiff")          |
| Object          |qualitative              | Target object used in the trial ("cup" or "shoe")                        |
| CorrectSide     |qualitative              | Position of the target object ("left" or "right")                        |
| Pointing_Right  |quantitative (binary)    | Participants' pointing response (Left=0; Right=1)                        |
| Correct_Pointing|quantitative (binary)    | Whether or not the participant pointed to the correct stimulus           |
| PropLook_Correct|quantitative (continuous)| Proportion of time that the participant looked at the correct stimulus   |
| PropLook_Right  |quantitative (continuous)| Proportion of time that the participant looked at the right side         |
| Correct_Looking |quantitative (binary)    | Whether or not the participant preferred to look at the correct stimulus |
| sex             |qualitative              | Participants' gender ("f" = female; "m" = male)                          |
| ageM            |quantitative (integer)   | Participants' age in months                                              |
| vcbAll          |quantitative (integer)   | Participants' total vocabulary size                                      |
| noun            |quantitative (integer)   | Vocabulary size of common nouns                                          |
| verb            |quantitative (integer)   | Vocabulary size of verbs                                                 |


## Software & Package Versions
- RStudio: 1.4.1106
- R: 4.04
- cmdstan: 2.27.0
- cmdstanr: 0.3.0
- ggmcmc: 1.5.1.1
- here: 1.0.1
- loo: 2.4.1
- rstan: 2.21.2
- tidyverse: 1.3.0

  
## Authors of This Repository
<!-- 
If you have any questions, please email at hiromichi.h gmail.com (please replace with @).
- [Hiromichi Hagihara](https://github.com/hagi-hara)
- [Hiroki Yamamoto](https://github.com/dororo1225)
-->
