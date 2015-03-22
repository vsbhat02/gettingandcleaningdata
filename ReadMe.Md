
## ReadMe (for Getting and Cleaning Data Final Project)

The raw data comes from the UCI ML Repository, located here: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The codebook for the new tidy data set can be found in Codebook.md with descriptions of the summarized data set.

The summarized data is created with the R script in this same repository. The R script is self-contained and does not link out to any other scripts (although it requires the dplyr library).

The basic components of the script are:
  0. Downloads and unzips the raw data files (this is commented out in the actual R script)
  1. Reads in the data files, merges the various training/test components, and then combines the training and test data
  2. Pulls out only those columns relating to mean and standard deviation of individual measurements (along with ID columns)
  3. Modifies the activity label IDs to be more informative
  4. Modifies the variable names to be more informative and consistently named
  5. Creates a summary of average measurements by subject and activity