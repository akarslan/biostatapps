# 1. General Info
Associative classification (AC) is a supervised learning task that classifies based on association rules. The related AC model classifies the unlabeled dataset using the formed association rules. Some of AC models are CBA (Classification Based on Association Rules Algorithm), GARC (Gain based Association Rule Classification), RCAR (Regularized Class Association Rules), etc. There are many algorithms (Apriori, ECLAT, CARMA, etc.) that are used to obtain the association rules, the most common of which is the Apriori algorithm.

# 2. Developed Software
The developed software consists of two parts. In the first part, the data file is uploaded into the software. Then, the types of variables ('Continuous', 'Discrete', 'Nominal', 'Ordinal') and their roles ('Predictor', 'Response/Output') are determined.
In the second part, the researcher can optionally apply feature selection to the dataset. 'Boruta' can be used as the feature selection algorithm. In the next stage, support and trust values are determined. Related parameters are set to 0.2 for support and 0.8 for confidence by default. In the next stage, there is a drop-down list in which the discretization techniques can be selected depending on whether there is at least one numerical variable in the dataset. At the last stage, the AC model is selected. There are two defined AC models in the software. These are CBA and RCAR. Model results are given in the main panel. This panel includes the confusion matrix, metrics for the classification performance of the relevant AC model, the association rules formed by the Apriori algorithm and subsequently pruned, and network visualization of these association rules.

# 3. Packages and other sources

## 3.1 Graphical interface
shiny, shinythemes, shinyWidgets, shinyLP, shinyalert, shinyjs, shinysky, https://www.flaticon.com/

## 3.2 Data upload, Data tables
foreign, readxl, DT

## 3.3 Feature selection, association rules, discretization techniques, associative classification, network visualization
glmnet, Boruta, arules, arulesCBA, caret, arulesViz, visNetwork
