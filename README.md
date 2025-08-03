# Investigating PPE efficacy in Preventing Cognitive Performance Decline in Construction Workers

**Goal**: Understanding the relationship between exposure to H2S and cognitive performance using two different mask types, as well as an interaction effect between exposure levels and mask type.

**Background**: The subset of data analyzed here includes 200 employees working in industrial environments with exposure to hydrogen sulfide, a toxic gas known to affect attention, reaction time, and working memory. Participants completed regular work tasks for 60 minutes wearing either a new respiratory mask or a standard model, while average airborne concentration of hydrogen sulfide was measured in parts per million. Afterwards, participants completed a cognitive performance test, scored on a 0-100 scale where higher scores reflect higher cognitive performance. Using Null Hypothesis Significance Testing (NHST) and Bayesian hierarchical linear regression, I estimate and compare the following two models: 

- Model 1 (M1): The first model includes the main effects of mask type and exposure level, assuming that the fixed effects are additive and linear. 

- Model 2 (M2): The second model expands upon M1 by including an interaction term between exposure level and mask type, allowing the effect of exposure on performance to vary by mask type. 

**Tools for Analysis**: Data was analyzed in R (v. 4.2.2) and the main script is in this repo as "Script.R"

Packages used include:

| Package | Description |
| --- | --- |
| tidyverse | Cleaning data |
| car | Making residual plots to check model assumptions |
| psych | Generating plots (histAll for correlations, describe/describeBy for descriptive stats), etc.|
| interactions, emmeans | Probing simple slopes, visualizing interaction effects |
| brms, bayestestR, BayesFactor | Bayesian model fitting |
| flextable, stargazer| Formatting tables |
| caret | k-fold cross validation (to compare models)|

**Results**: While the **interaction model is statistically favored over the main effect only model**, it seems to have a relatively small effect. Even without the moderating effect, **newer masks were associated with higher performance on the cognitive exam**, with an average cognitive improvement of 14.71 points (NHST) and 15.21 points (Bayesian), holding exposure levels constant. Considering that this improvement is around 15% of the total range of the outcome, its significance should be noted. In particular, mask type was the only predictor that was fully outside of the ROPE range for both the main effect only model and the interaction model, solidifying its practical and statistical significance. Hence **newer masks should be worn** in high-exposure work environments to minimize impacts on cognitive ability. 
