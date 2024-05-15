# ML_training
ML_training - Mainly Random Forest approach

Check Fuzzy Forest. 
https://www.rdocumentation.org/packages/fuzzyforest/versions/1.0.8/topics/fuzzy_forest

## Theory
Random forest takes random samples from the observations, random initial variables(columns) and tries to build a model. Random forest algorithm is as follows:  
- Draw a random bootstrap sample of size n (randomly choose n samples from training data).  
- Grow a decision tree from bootstrap sample. At each node of tree, randomly select d features.  
- Split the node using features(variables) that provide best split according to objective function. For instance, by maximizing the information gain.
- Repeat steps 1 to step 2, k times(k is the number of trees you want to create using subset of samples).  
- Aggregate the prediction by each tree for a new data point to assign the class label by majority vote i.e pick the group selected by most number of trees and assign new data point to that group.  
