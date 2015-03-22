
## Code Book (for Getting and Cleaning Data Final Project)

The raw data for this project comes from UCI ML Repository, which can be accessed at: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

### Basic Overview

The raw data consists of training and test portions, obtained from the embedded accelerometer and gyroscope Samsung Galaxy SII smartphones, recorded against a group of 30 subjects. The subjects were measured six basic activities (walking - on flat surfaces, up stairs, and down stairs - as well as while sitting, standing, and laying down).

The data presented here is grouped for the 30 subjects and 6 activities, and contains an average of all individual mean and standard deviation measurements taken during the experiment. Thus, the raw data set which is comprised of 10,299 observational moments and 563 measurements is reduced to 180 groups of observations and 68 average measurements across each group.

### Varibles in Summarized Data

The first two columns identify subjects and activities: 
  * subject_id:                                 Subject ID (one of 30 subjects)
  * activity_label:                             Activity Type (one of 6 types)

The remaining columns identify different kinds of measurements. The naming convention for each measurement variable in the summarized data here follows the convention below:
* 1st part of name is the measurement domain (time or frequency)
* 2nd part of name is the measurement device (accelerometer or gyroscope)
* 3rd part of name is the signal type (separated by underscores if multiple signal types; includes body, gravity, magnitude, and jerk signals)
* 4th part of name is the directional component in 3 dimensions (x, y, or z)
* 5th part of name is the statistical moment (mean or standard deviation)
 
The variables were already normalized to a [-1, 1] range in the raw data, so note that values can be negative for these observations.
