
## Code Book (for Getting and Cleaning Data Final Project)

The raw data for this project comes from UCI ML Repository, which can be accessed at: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

### Basic Overview

The raw data consists of training and test portions, obtained from the embedded accelerometer and gyroscope Samsung Galaxy SII smartphones, recorded against a group of 30 subjects. The subjects were measured six basic activities (walking - on flat surfaces, up stairs, and down stairs - as well as while sitting, standing, and laying down).

The data presented here is grouped for the 30 subjects and 6 activities, and contains an average of all individual mean and standard deviation measurements taken during the experiment. Thus, the raw data set which is comprised of 10,299 observational moments and 563 measurements is reduced to 180 groups of observations and 68 average measurements across each group.

### Varibles in Summarized Data

The naming convention for each measurement variable in the summarized data here follows the convention below:
  [domain]_[measure type]_[signal_types (separated by underscores if multiple)]_[3-dim directional component]_[statistical moment]
  
This allows for clear sorting amongst the variables, which are the following:
* subject_id:                                 Subject ID (one of 30 subjects)
* activity_label:                             Activity Type (one of 6 types)
* time_acc_body_[3 directions]_mean:          Mean Accelerometer values in Time Domain, Body Signal
* time_acc_body_[3 directions]_std:           Std Dev Accelerometer values in Time Domain, Body Signal
* time_acc_gravity_[3 directions]_mean:       Mean Accelerometer values in Time Domain, Gravity Signal
* time_acc_gravity_[3 directions]_std:        Std Dev Accelerometer values in Time Domain, Gravity Signal
* time_acc_body_jerk_[3 directions]_mean:     Mean Accelerometer values in Time Domain, Body Jerk Signal
* time_acc_body_jerk_[3 directions]_std:      Std Dev Accelerometer values in Time Domain, Body Jerk Signal
* time_gyro_body_[3 directions]_mean:         Mean Gyroscope values in Time Domain, Body Signal
* time_gyro_body_[3 directions]_std:          Std Dev Gyroscope values in Time Domain, Body Signal
* time_gyro_body_jerk_[3 directions]_mean:    Mean Gyroscope values in Time Domain, Body Jerk Signal
* time_gyro_body_jerk_[3 directions]_std:     Std Dev Gyroscope values in Time Domain, Body Jerk Signal
* time_acc_body_mag_mean:                     Mean Accelerometer Values in Time Domain, Body Magnitude Signal
* time_acc_body_mag_std:                      Std Dev Accelerometer Values in Time Domain, Body Magnitude Signal
* time_acc_gravity_mag_mean:                  Mean Accelerometer Values in Time Domain, Gravity Magnitude Signal
* time_acc_gravity_mag_std:                   Std Dev Accelerometer Values in Time Domain, Gravity Magnitude Signal
* time_acc_body_jerk_mag_mean:                Mean Accelerometer Values in Time Domain, Body Jerk Magnitude Signal
* time_acc_body_jerk_mag_std:                 Std Dev Accelerometer Values in Time Domain, Body Jerk Jerk Signal
* time_gyro_body_mag_mean:                    Mean Dev Gyroscope Values in Time Domain, Body Magnitude Signal
* time_gyro_body_mag_std:                     Std Dev Gyroscope Values in Time Domain, Body Magnitude Signal
* time_gyro_body_jerk_mag_mean:               Mean Dev Gyroscope Values in Time Domain, Body Jerk Magnitude Signal
* time_gyro_body_jerk_mag_std:                Std Dev Gyroscope Values in Time Domain, Body Jerk Magnitude Signal
* freq_acc_body_[3 directions]_mean:          Mean Accelerometer values in Frequency Domain, Body Signal
* freq_acc_body_[3 directions]_std:           Std Dev Accelerometer values in Frequency Domain, Body Signal
* freq_acc_body_jerk_[3 directions]_mean:     Mean Accelerometer values in Frequency Domain, Body Jerk Signal
* freq_acc_body_jerk_[3 directions]_std:      Std Dev Accelerometer values in Frequency Domain, Body Jerk Signal
* freq_gyro_body_[3 directions]_mean:         Mean Gyroscope values in Frequency Domain, Body Signal
* freq_gyro_body_[3 directions]_std:          Std Dev Accelerometer values in Frequency Domain, Body Signal
* freq_acc_body_mag_mean:                     Mean Accelerometer values in Frequency Domain, Body Magnitude Signal
* freq_acc_body_mag_std:                      Std Dev Accelerometer values in Frequency Domain, Body Magnitude Signal
* freq_acc_body_body_jerk_mag_mean:           Mean Accelerometer values in Frequency Domain, Body Jerk Magnitude Signal
* freq_acc_body_body_jerk_mag_std:            Std Dev Accelerometer values in Frequency Domain, Body Jerk Magnitude Signal
* freq_gyro_body_body_mag_mean:               Mean Gyroscope values in Frequency Domain, Body Magnitude Signal
* freq_gyro_body_body_mag_std:                Std Dev Gyroscope values in Frequency Domain, Body Magnitude Signal
* freq_gyro_body_body_jerk_mag_mean:          Mean Gyroscope values in Frequency Domain, Body Jerk Magnitude Signal
* freq_gyro_body_body_jerk_mag_std:           Std Dev Gyroscope values in Frequency Domain, Body Jerk Magnitude Signal
 
The variables were already normalized to a [-1, 1] range in the raw data, so note that values can be negative for these observations.
