# Data
The data to reproduce the analyses has to be obtained from https://www.ppmi-info.org/ and placed in the raw-data directory.

# Dependencies
The analysis code has the following dependencies:
```{r}
install.packages(c("tidyverse","broom","future","furrr","devtools","ltmle","SuperLearner","arm","randomForest"))
```
We used dplyr 0.8.5. Using version 1.0.0 is not currently compatible with some of the analysis code used here. 