# Code Book for Cleaning Data Assignment


## Data Transformation at different levels

Data transformation steps for this assignment is listed below:

1. Raw dataframes created from raw data.
   * **activity_labels**: Activity label data
   * **features**: All feature variables
   * **test**: Test data set
   * **test_labels**: Dataframe with labels for test data 
   * **test_subjects**: Dataframe with subject/volunteer identification for test data
   * **train**: Training data set
   * **train_labels**: Dataframe with labels for train data 
   * **train_subjects**: Dataframe with subject/volunteer identification for train data
   
2. Dataframes containing relevant test and train data together
   * **test_1**: test, test_labels and test_subjects dataframes together
   * **train_1**: train, train_labels and train_subjects dataframes together
   
3. **mergeData**: Combined test_1 and train_1 datasets

4.  **mean_std_Data**: Dataframe containing only measurements with mean (including mean frequency) and standard deviation

5. **Final_raw_data**: Data with descriptive activity names (Step-1 ;taken from activity_labels data frame) and descriptive variable names (Step-2)

6. **Final_tidy_data**:Tidy data with mean calculated for all variables by each activity and subject

## Assignment of Descriptive Variable Names

Descriptive variable names are assigned to raw feature names as below:

**Raw File Feature Name**|**Descriptive Name**
:-----|:-----
tBodyAcc-mean()-X|time_domain_body_acceleration_mean_X_Axis
tBodyAcc-mean()-Y|time_domain_body_acceleration_mean_Y_Axis
tBodyAcc-mean()-Z|time_domain_body_acceleration_mean_Z_Axis
tBodyAcc-std()-X|time_domain_body_acceleration_standard_deviation_X_Axis
tBodyAcc-std()-Y|time_domain_body_acceleration_standard_deviation_Y_Axis
tBodyAcc-std()-Z|time_domain_body_acceleration_standard_deviation_Z_Axis
tGravityAcc-mean()-X|time_domain_gravity_acceleration_mean_X_Axis
tGravityAcc-mean()-Y|time_domain_gravity_acceleration_mean_Y_Axis
tGravityAcc-mean()-Z|time_domain_gravity_acceleration_mean_Z_Axis
tGravityAcc-std()-X|time_domain_gravity_acceleration_standard_deviation_X_Axis
tGravityAcc-std()-Y|time_domain_gravity_acceleration_standard_deviation_Y_Axis
tGravityAcc-std()-Z|time_domain_gravity_acceleration_standard_deviation_Z_Axis
tBodyAccJerk-mean()-X|time_domain_body_acceleration_jerk_mean_X_Axis
tBodyAccJerk-mean()-Y|time_domain_body_acceleration_jerk_mean_Y_Axis
tBodyAccJerk-mean()-Z|time_domain_body_acceleration_jerk_mean_Z_Axis
tBodyAccJerk-std()-X|time_domain_body_acceleration_jerk_standard_deviation_X_Axis
tBodyAccJerk-std()-Y|time_domain_body_acceleration_jerk_standard_deviation_Y_Axis
tBodyAccJerk-std()-Z|time_domain_body_acceleration_jerk_standard_deviation_Z_Axis
tBodyGyro-mean()-X|time_domain_body_gyroscope_reading_mean_X_Axis
tBodyGyro-mean()-Y|time_domain_body_gyroscope_reading_mean_Y_Axis
tBodyGyro-mean()-Z|time_domain_body_gyroscope_reading_mean_Z_Axis
tBodyGyro-std()-X|time_domain_body_gyroscope_reading_standard_deviation_X_Axis
tBodyGyro-std()-Y|time_domain_body_gyroscope_reading_standard_deviation_Y_Axis
tBodyGyro-std()-Z|time_domain_body_gyroscope_reading_standard_deviation_Z_Axis
tBodyGyroJerk-mean()-X|time_domain_body_gyroscope_jerk_reading_mean_X_Axis
tBodyGyroJerk-mean()-Y|time_domain_body_gyroscope_jerk_reading_mean_Y_Axis
tBodyGyroJerk-mean()-Z|time_domain_body_gyroscope_jerk_reading_mean_Z_Axis
tBodyGyroJerk-std()-X|time_domain_body_gyroscope_jerk_reading_standard_deviation_X_Axis
tBodyGyroJerk-std()-Y|time_domain_body_gyroscope_jerk_reading_standard_deviation_Y_Axis
tBodyGyroJerk-std()-Z|time_domain_body_gyroscope_jerk_reading_standard_deviation_Z_Axis
tBodyAccMag-mean()|time_domain_body_acceleration_magnitude_mean
tBodyAccMag-std()|time_domain_body_acceleration_magnitude_standard_deviation
tGravityAccMag-mean()|time_domain_gravity_acceleration_magnitude_mean
tGravityAccMag-std()|time_domain_gravity_acceleration_magnitude_standard_deviation
tBodyAccJerkMag-mean()|time_domain_body_gyroscope_jerk_magnitudereading_mean
tBodyAccJerkMag-std()|time_domain_body_gyroscope_jerk_magnitudereading_standard_deviation
tBodyGyroMag-mean()|time_domain_body_gyroscope_magnitude_reading_mean
tBodyGyroMag-std()|time_domain_body_gyroscope_magnitude_reading_standard_deviation
tBodyGyroJerkMag-mean()|time_domain_body_gyroscope_jerk_magnitude_reading_mean
tBodyGyroJerkMag-std()|time_domain_body_gyroscope_jerk_magnitude_reading_standard_deviation
fBodyAcc-mean()-X|frequency_domain_body_acceleration_mean_X_Axis
fBodyAcc-mean()-Y|frequency_domain_body_acceleration_mean_Y_Axis
fBodyAcc-mean()-Z|frequency_domain_body_acceleration_mean_Z_Axis
fBodyAcc-std()-X|frequency_domain_body_acceleration_standard_deviation_X_Axis
fBodyAcc-std()-Y|frequency_domain_body_acceleration_standard_deviation_Y_Axis
fBodyAcc-std()-Z|frequency_domain_body_acceleration_standard_deviation_Z_Axis
fBodyAcc-meanFreq()-X|frequency_domain_body_acceleration_mean_frequency_X_Axis
fBodyAcc-meanFreq()-Y|frequency_domain_body_acceleration_mean_frequency_Y_Axis
fBodyAcc-meanFreq()-Z|frequency_domain_body_acceleration_mean_frequency_Z_Axis
fBodyAccJerk-mean()-X|frequency_domain_body_acceleration_jerk_mean_X_Axis
fBodyAccJerk-mean()-Y|frequency_domain_body_acceleration_jerk_mean_Y_Axis
fBodyAccJerk-mean()-Z|frequency_domain_body_acceleration_jerk_mean_Z_Axis
fBodyAccJerk-std()-X|frequency_domain_body_acceleration_jerk_standard_deviation_X_Axis
fBodyAccJerk-std()-Y|frequency_domain_body_acceleration_jerk_standard_deviation_Y_Axis
fBodyAccJerk-std()-Z|frequency_domain_body_acceleration_jerk_standard_deviation_Z_Axis
fBodyAccJerk-meanFreq()-X|frequency_domain_body_acceleration_jerk_mean_frequency_X_Axis
fBodyAccJerk-meanFreq()-Y|frequency_domain_body_acceleration_jerk_mean_frequency_Y_Axis
fBodyAccJerk-meanFreq()-Z|frequency_domain_body_acceleration_jerk_mean_frequency_Z_Axis
fBodyGyro-mean()-X|frequency_domain_body_gyroscope_reading_mean_X_Axis
fBodyGyro-mean()-Y|frequency_domain_body_gyroscope_reading_mean_Y_Axis
fBodyGyro-mean()-Z|frequency_domain_body_gyroscope_reading_mean_Z_Axis
fBodyGyro-std()-X|frequency_domain_body_gyroscope_reading_standard_deviation_X_Axis
fBodyGyro-std()-Y|frequency_domain_body_gyroscope_reading_standard_deviation_Y_Axis
fBodyGyro-std()-Z|frequency_domain_body_gyroscope_reading_standard_deviation_Z_Axis
fBodyGyro-meanFreq()-X|frequency_domain_body_gyroscope_reading_mean_frequency_X_Axis
fBodyGyro-meanFreq()-Y|frequency_domain_body_gyroscope_reading_mean_frequency_Y_Axis
fBodyGyro-meanFreq()-Z|frequency_domain_body_gyroscope_reading_mean_frequency_Z_Axis
fBodyAccMag-mean()|frequency_domain_body_acceleration_magnitude_mean
fBodyAccMag-std()|frequency_domain_body_acceleration_magnitude_standard_deviation
fBodyAccMag-meanFreq()|frequency_domain_body_acceleration_magnitude_mean_frequency
fBodyBodyAccJerkMag-mean()|frequency_domain_body_gyroscope_jerk_magnitudereading_mean
fBodyBodyAccJerkMag-std()|frequency_domain_body_gyroscope_jerk_magnitudereading_standard_deviation
fBodyBodyAccJerkMag-meanFreq()|frequency_domain_body_gyroscope_jerk_magnitudereading_mean_frequency
fBodyBodyGyroMag-mean()|frequency_domain_body_gyroscope_magnitude_reading_mean
fBodyBodyGyroMag-std()|frequency_domain_body_gyroscope_magnitude_reading_standard_deviation
fBodyBodyGyroMag-meanFreq()|frequency_domain_body_gyroscope_magnitude_reading_mean_frequency
fBodyBodyGyroJerkMag-mean()|frequency_domain_body_gyroscope_jerk_magnitude_reading_mean
fBodyBodyGyroJerkMag-std()|frequency_domain_body_gyroscope_jerk_magnitude_reading_standard_deviation
fBodyBodyGyroJerkMag-meanFreq()|frequency_domain_body_gyroscope_jerk_magnitude_reading_mean_frequency


## Variable Description

The following table desribes each variable. The table was generated with the excellent markdown table generator [here](http://jakebathman.github.io/Markdown-Table-Generator/) by Jake Bathman.

**Descriptive Name**|**Description**
:-----|:-----
activity|Activity identification among six different activities
subject|Volunteer identification among 30 different volunteers within age bracket of 19-48 years
time_domain_body_acceleration_mean_X_Axis|mean value of time domain signal captured from linear body acceleration in X-Axis
time_domain_body_acceleration_mean_Y_Axis|mean value of time domain signal captured from linear body acceleration in Y-Axis
time_domain_body_acceleration_mean_Z_Axis|mean value of time domain signal captured from linear body acceleration in Z-Axis
time_domain_body_acceleration_standard_deviation_X_Axis|standard deviation value of time domain signal captured from linear body acceleration in X-Axis
time_domain_body_acceleration_standard_deviation_Y_Axis|standard deviation value of time domain signal captured from linear body acceleration in Y-Axis
time_domain_body_acceleration_standard_deviation_Z_Axis|standard deviation value of time domain signal captured from linear body acceleration in Z-Axis
time_domain_gravity_acceleration_mean_X_Axis|mean value of time domain signal captured from gravity acceleration in X-Axis
time_domain_gravity_acceleration_mean_Y_Axis|mean value of time domain signal captured from gravity acceleration in Y-Axis
time_domain_gravity_acceleration_mean_Z_Axis|mean value of time domain signal captured from gravity acceleration in Z-Axis
time_domain_gravity_acceleration_standard_deviation_X_Axis|standard deviation value of time domain signal captured from gravity acceleration in X-Axis
time_domain_gravity_acceleration_standard_deviation_Y_Axis|standard deviation value of time domain signal captured from gravity acceleration in Y-Axis
time_domain_gravity_acceleration_standard_deviation_Z_Axis|standard deviation value of time domain signal captured from gravity acceleration in Z-Axis
time_domain_body_acceleration_jerk_mean_X_Axis|mean value of time domain jerk signal captured from  body acceleration in X-Axis
time_domain_body_acceleration_jerk_mean_Y_Axis|mean value of time domain jerk signal captured from  body acceleration in Y-Axis
time_domain_body_acceleration_jerk_mean_Z_Axis|mean value of time domain jerk signal captured from  body acceleration in Z-Axis
time_domain_body_acceleration_jerk_standard_deviation_X_Axis|standard deviation value of time domain jerk signal captured from  body acceleration in X-Axis
time_domain_body_acceleration_jerk_standard_deviation_Y_Axis|standard deviation value of time domain jerk signal captured from  body acceleration in Y-Axis
time_domain_body_acceleration_jerk_standard_deviation_Z_Axis|standard deviation value of time domain jerk signal captured from  body acceleration in Z-Axis
time_domain_body_gyroscope_reading_mean_X_Axis|mean value of time domain signal captured from angular body velocity in X-Axis
time_domain_body_gyroscope_reading_mean_Y_Axis|mean value of time domain signal captured from angular body velocity in Y-Axis
time_domain_body_gyroscope_reading_mean_Z_Axis|mean value of time domain signal captured from angular body velocity in Z-Axis
time_domain_body_gyroscope_reading_standard_deviation_X_Axis|standard deviation value of time domain signal captured from angular body velocity in X-Axis
time_domain_body_gyroscope_reading_standard_deviation_Y_Axis|standard deviation value of time domain signal captured from angular body velocity in Y-Axis
time_domain_body_gyroscope_reading_standard_deviation_Z_Axis|standard deviation value of time domain signal captured from angular body velocity in Z-Axis
time_domain_body_gyroscope_jerk_reading_mean_X_Axis|mean value of time domain jerk signal captured from  body velocity in X-Axis
time_domain_body_gyroscope_jerk_reading_mean_Y_Axis|mean value of time domain jerk signal captured from  body velocity in Y-Axis
time_domain_body_gyroscope_jerk_reading_mean_Z_Axis|mean value of time domain jerk signal captured from  body velocity in Z-Axis
time_domain_body_gyroscope_jerk_reading_standard_deviation_X_Axis|standard deviation value of time domain jerk signal captured from  body velocity in X-Axis
time_domain_body_gyroscope_jerk_reading_standard_deviation_Y_Axis|standard deviation value of time domain jerk signal captured from  body velocity in Y-Axis
time_domain_body_gyroscope_jerk_reading_standard_deviation_Z_Axis|standard deviation value of time domain jerk signal captured from  body velocity in Z-Axis
time_domain_body_acceleration_magnitude_mean|mean value of time domain magnitude of body acceleration signals
time_domain_body_acceleration_magnitude_standard_deviation|standard deviation value of time domain magnitude of body acceleration signals
time_domain_gravity_acceleration_magnitude_mean|mean value of time domain magnitude of gravity acceleration signals
time_domain_gravity_acceleration_magnitude_standard_deviation|standard deviation value of time domain magnitude of gravity acceleration signals
time_domain_body_gyroscope_jerk_magnitudereading_mean|mean value of time domain magnitude of body acceleration jerk signals
time_domain_body_gyroscope_jerk_magnitudereading_standard_deviation|standard deviation value of time domain magnitude of body acceleration jerk signals
time_domain_body_gyroscope_magnitude_reading_mean|mean value of time domain magnitude of body anglular velocity signals
time_domain_body_gyroscope_magnitude_reading_standard_deviation|standard deviation value of time domain magnitude of body angular velocity signals
time_domain_body_gyroscope_jerk_magnitude_reading_mean|mean value of time domain magnitude of body anglular velocity jerk signals
time_domain_body_gyroscope_jerk_magnitude_reading_standard_deviation|standard deviation value of time domain magnitude of body angular velocity jerk signals
frequency_domain_body_acceleration_mean_X_Axis|mean value of frequency domain signal captured from linear body acceleration in X-Axis
frequency_domain_body_acceleration_mean_Y_Axis|mean value of frequency domain signal captured from linear body acceleration in Y-Axis
frequency_domain_body_acceleration_mean_Z_Axis|mean value of frequency domain signal captured from linear body acceleration in Z-Axis
frequency_domain_body_acceleration_standard_deviation_X_Axis|standard deviation value of frequency domain signal captured from linear body acceleration in X-Axis
frequency_domain_body_acceleration_standard_deviation_Y_Axis|standard deviation value of frequency domain signal captured from linear body acceleration in Y-Axis
frequency_domain_body_acceleration_standard_deviation_Z_Axis|standard deviation value of frequency domain signal captured from linear body acceleration in Z-Axis
frequency_domain_body_acceleration_mean_frequency_X_Axis|mean frequency of frequency domain signal captured from linear body acceleration in X-Axis
frequency_domain_body_acceleration_mean_frequency_Y_Axis|mean frequency of frequency domain signal captured from linear body acceleration in Y-Axis
frequency_domain_body_acceleration_mean_frequency_Z_Axis|mean frequency of frequency domain signal captured from linear body acceleration in Z-Axis
frequency_domain_body_acceleration_jerk_mean_X_Axis|mean value of frequency domain jerk signal captured from  body acceleration in X-Axis
frequency_domain_body_acceleration_jerk_mean_Y_Axis|mean value of frequency domain jerk signal captured from  body acceleration in Y-Axis
frequency_domain_body_acceleration_jerk_mean_Z_Axis|mean value of frequency domain jerk signal captured from  body acceleration in Z-Axis
frequency_domain_body_acceleration_jerk_standard_deviation_X_Axis|standard deviation value of frequency domain jerk signal captured from  body acceleration in X-Axis
frequency_domain_body_acceleration_jerk_standard_deviation_Y_Axis|standard deviation value of frequency domain jerk signal captured from  body acceleration in Y-Axis
frequency_domain_body_acceleration_jerk_standard_deviation_Z_Axis|standard deviation value of frequency domain jerk signal captured from  body acceleration in Z-Axis
frequency_domain_body_acceleration_jerk_mean_frequency_X_Axis|mean frequency of frequency domain jerk signal captured from  body acceleration in X-Axis
frequency_domain_body_acceleration_jerk_mean_frequency_Y_Axis|mean frequency of frequency domain jerk signal captured from  body acceleration in Y-Axis
frequency_domain_body_acceleration_jerk_mean_frequency_Z_Axis|mean frequency of frequency domain jerk signal captured from  body acceleration in Z-Axis
frequency_domain_body_gyroscope_reading_mean_X_Axis|mean value of frequency domain signal captured from angular body velocity in X-Axis
frequency_domain_body_gyroscope_reading_mean_Y_Axis|mean value of frequency domain signal captured from angular body velocity in Y-Axis
frequency_domain_body_gyroscope_reading_mean_Z_Axis|mean value of frequency domain signal captured from angular body velocity in Z-Axis
frequency_domain_body_gyroscope_reading_standard_deviation_X_Axis|standard deviation value of frequency domain signal captured from angular body velocity in X-Axis
frequency_domain_body_gyroscope_reading_standard_deviation_Y_Axis|standard deviation value of frequency domain signal captured from angular body velocity in Y-Axis
frequency_domain_body_gyroscope_reading_standard_deviation_Z_Axis|standard deviation value of frequency domain signal captured from angular body velocity in Z-Axis
frequency_domain_body_gyroscope_reading_mean_frequency_X_Axis|mean frequency of frequency domain signal captured from angular body velocity in X-Axis
frequency_domain_body_gyroscope_reading_mean_frequency_Y_Axis|mean frequency of frequency domain signal captured from angular body velocity in Y-Axis
frequency_domain_body_gyroscope_reading_mean_frequency_Z_Axis|mean frequency of frequency domain signal captured from angular body velocity in Z-Axis
frequency_domain_body_acceleration_magnitude_mean|mean value of frequency domain magnitude of body acceleration signals
frequency_domain_body_acceleration_magnitude_standard_deviation|standard deviation value of frequency domain magnitude of body acceleration signals
frequency_domain_body_acceleration_magnitude_mean_frequency|mean frequency of frequency domain magnitude of body acceleration signals
frequency_domain_body_gyroscope_jerk_magnitudereading_mean|mean value of frequency domain magnitude of body acceleration jerk signals
frequency_domain_body_gyroscope_jerk_magnitudereading_standard_deviation|standard deviation value of frequency domain magnitude of body acceleration jerk signals
frequency_domain_body_gyroscope_jerk_magnitudereading_mean_frequency|mean frequency of frequency domain magnitude of body acceleration jerk signals
frequency_domain_body_gyroscope_magnitude_reading_mean|mean value of frequency domain magnitude of body anglular velocity signals
frequency_domain_body_gyroscope_magnitude_reading_standard_deviation|standard deviation value of frequency domain magnitude of body angular velocity signals
frequency_domain_body_gyroscope_magnitude_reading_mean_frequency|mean frequency of frequency domain magnitude of body anglular velocity signals
frequency_domain_body_gyroscope_jerk_magnitude_reading_mean|mean value of frequency domain magnitude of body anglular velocity jerk signals
frequency_domain_body_gyroscope_jerk_magnitude_reading_standard_deviation|standard deviation value of frequency domain magnitude of body angular velocity jerk signals
frequency_domain_body_gyroscope_jerk_magnitude_reading_mean_frequency|mean frequency of frequency domain magnitude of body anglular velocity jerk signals


## Variable Details

Variable details generated with the help of excellent [memisc package](https://cran.r-project.org/web/packages/memisc/index.html).

=================================================================================

   activity

---------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 6 levels

        Values and labels    N    Percent 
                                          
   1 'LAYING'               30   16.7     
   2 'SITTING'              30   16.7     
   3 'STANDING'             30   16.7     
   4 'WALKING'              30   16.7     
   5 'WALKING_DOWNSTAIRS'   30   16.7     
   6 'WALKING_UPSTAIRS'     30   16.7     

=================================================================================

   subject

---------------------------------------------------------------------------------

   Storage mode: integer
   Factor with 30 levels

   Values and labels   N   Percent
                                  
              1 '1'    6   3.3    
              2 '2'    6   3.3    
              3 '3'    6   3.3    
              4 '4'    6   3.3    
              5 '5'    6   3.3    
              6 '6'    6   3.3    
              7 '7'    6   3.3    
              8 '8'    6   3.3    
              9 '9'    6   3.3    
             10 '10'   6   3.3    
             11 '11'   6   3.3    
             12 '12'   6   3.3    
             13 '13'   6   3.3    
             14 '14'   6   3.3    
             15 '15'   6   3.3    
             16 '16'   6   3.3    
             17 '17'   6   3.3    
             18 '18'   6   3.3    
             19 '19'   6   3.3    
             20 '20'   6   3.3    
             21 '21'   6   3.3    
             22 '22'   6   3.3    
             23 '23'   6   3.3    
             24 '24'   6   3.3    
             25 '25'   6   3.3    
             26 '26'   6   3.3    
             27 '27'   6   3.3    
             28 '28'   6   3.3    
             29 '29'   6   3.3    
             30 '30'   6   3.3    

=================================================================================

   time_domain_body_acceleration_mean_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  0.222
       1st Qu.:  0.271
        Median:  0.277
          Mean:  0.274
       3rd Qu.:  0.280
          Max.:  0.301

=================================================================================

   time_domain_body_acceleration_mean_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.041
       1st Qu.:  -0.020
        Median:  -0.017
          Mean:  -0.018
       3rd Qu.:  -0.015
          Max.:  -0.001

=================================================================================

   time_domain_body_acceleration_mean_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.153
       1st Qu.:  -0.112
        Median:  -0.108
          Mean:  -0.109
       3rd Qu.:  -0.104
          Max.:  -0.075

=================================================================================

   time_domain_body_acceleration_standard_deviation_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.996
       1st Qu.:  -0.980
        Median:  -0.753
          Mean:  -0.558
       3rd Qu.:  -0.198
          Max.:   0.627

=================================================================================

   time_domain_body_acceleration_standard_deviation_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.990
       1st Qu.:  -0.942
        Median:  -0.509
          Mean:  -0.460
       3rd Qu.:  -0.031
          Max.:   0.617

=================================================================================

   time_domain_body_acceleration_standard_deviation_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.988
       1st Qu.:  -0.950
        Median:  -0.652
          Mean:  -0.576
       3rd Qu.:  -0.231
          Max.:   0.609

=================================================================================

   time_domain_gravity_acceleration_mean_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.680
       1st Qu.:   0.838
        Median:   0.921
          Mean:   0.697
       3rd Qu.:   0.943
          Max.:   0.975

=================================================================================

   time_domain_gravity_acceleration_mean_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.480
       1st Qu.:  -0.233
        Median:  -0.128
          Mean:  -0.016
       3rd Qu.:   0.088
          Max.:   0.957

=================================================================================

   time_domain_gravity_acceleration_mean_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.495
       1st Qu.:  -0.117
        Median:   0.024
          Mean:   0.074
       3rd Qu.:   0.149
          Max.:   0.958

=================================================================================

   time_domain_gravity_acceleration_standard_deviation_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.997
       1st Qu.:  -0.982
        Median:  -0.969
          Mean:  -0.964
       3rd Qu.:  -0.951
          Max.:  -0.830

=================================================================================

   time_domain_gravity_acceleration_standard_deviation_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.994
       1st Qu.:  -0.971
        Median:  -0.959
          Mean:  -0.952
       3rd Qu.:  -0.937
          Max.:  -0.644

=================================================================================

   time_domain_gravity_acceleration_standard_deviation_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.991
       1st Qu.:  -0.961
        Median:  -0.945
          Mean:  -0.936
       3rd Qu.:  -0.918
          Max.:  -0.610

=================================================================================

   time_domain_body_acceleration_jerk_mean_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  0.043
       1st Qu.:  0.074
        Median:  0.076
          Mean:  0.079
       3rd Qu.:  0.083
          Max.:  0.130

=================================================================================

   time_domain_body_acceleration_jerk_mean_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.039
       1st Qu.:   0.000
        Median:   0.009
          Mean:   0.008
       3rd Qu.:   0.013
          Max.:   0.057

=================================================================================

   time_domain_body_acceleration_jerk_mean_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.067
       1st Qu.:  -0.011
        Median:  -0.004
          Mean:  -0.005
       3rd Qu.:   0.002
          Max.:   0.038

=================================================================================

   time_domain_body_acceleration_jerk_standard_deviation_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.995
       1st Qu.:  -0.983
        Median:  -0.810
          Mean:  -0.595
       3rd Qu.:  -0.223
          Max.:   0.544

=================================================================================

   time_domain_body_acceleration_jerk_standard_deviation_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.990
       1st Qu.:  -0.972
        Median:  -0.776
          Mean:  -0.565
       3rd Qu.:  -0.148
          Max.:   0.355

=================================================================================

   time_domain_body_acceleration_jerk_standard_deviation_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.993
       1st Qu.:  -0.983
        Median:  -0.884
          Mean:  -0.736
       3rd Qu.:  -0.512
          Max.:   0.031

=================================================================================

   time_domain_body_gyroscope_reading_mean_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.206
       1st Qu.:  -0.047
        Median:  -0.029
          Mean:  -0.032
       3rd Qu.:  -0.017
          Max.:   0.193

=================================================================================

   time_domain_body_gyroscope_reading_mean_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.204
       1st Qu.:  -0.090
        Median:  -0.073
          Mean:  -0.074
       3rd Qu.:  -0.061
          Max.:   0.027

=================================================================================

   time_domain_body_gyroscope_reading_mean_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.072
       1st Qu.:   0.075
        Median:   0.085
          Mean:   0.087
       3rd Qu.:   0.102
          Max.:   0.179

=================================================================================

   time_domain_body_gyroscope_reading_standard_deviation_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.994
       1st Qu.:  -0.973
        Median:  -0.789
          Mean:  -0.692
       3rd Qu.:  -0.441
          Max.:   0.268

=================================================================================

   time_domain_body_gyroscope_reading_standard_deviation_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.994
       1st Qu.:  -0.963
        Median:  -0.802
          Mean:  -0.653
       3rd Qu.:  -0.420
          Max.:   0.477

=================================================================================

   time_domain_body_gyroscope_reading_standard_deviation_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.986
       1st Qu.:  -0.961
        Median:  -0.801
          Mean:  -0.616
       3rd Qu.:  -0.311
          Max.:   0.565

=================================================================================

   time_domain_body_gyroscope_jerk_reading_mean_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.157
       1st Qu.:  -0.103
        Median:  -0.099
          Mean:  -0.096
       3rd Qu.:  -0.091
          Max.:  -0.022

=================================================================================

   time_domain_body_gyroscope_jerk_reading_mean_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.077
       1st Qu.:  -0.046
        Median:  -0.041
          Mean:  -0.043
       3rd Qu.:  -0.038
          Max.:  -0.013

=================================================================================

   time_domain_body_gyroscope_jerk_reading_mean_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.092
       1st Qu.:  -0.062
        Median:  -0.053
          Mean:  -0.055
       3rd Qu.:  -0.049
          Max.:  -0.007

=================================================================================

   time_domain_body_gyroscope_jerk_reading_standard_deviation_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.997
       1st Qu.:  -0.980
        Median:  -0.840
          Mean:  -0.704
       3rd Qu.:  -0.463
          Max.:   0.179

=================================================================================

   time_domain_body_gyroscope_jerk_reading_standard_deviation_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.997
       1st Qu.:  -0.983
        Median:  -0.894
          Mean:  -0.764
       3rd Qu.:  -0.586
          Max.:   0.296

=================================================================================

   time_domain_body_gyroscope_jerk_reading_standard_deviation_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.995
       1st Qu.:  -0.985
        Median:  -0.861
          Mean:  -0.710
       3rd Qu.:  -0.474
          Max.:   0.193

=================================================================================

   time_domain_body_acceleration_magnitude_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.986
       1st Qu.:  -0.957
        Median:  -0.483
          Mean:  -0.497
       3rd Qu.:  -0.092
          Max.:   0.645

=================================================================================

   time_domain_body_acceleration_magnitude_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.986
       1st Qu.:  -0.943
        Median:  -0.607
          Mean:  -0.544
       3rd Qu.:  -0.209
          Max.:   0.428

=================================================================================

   time_domain_gravity_acceleration_magnitude_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.986
       1st Qu.:  -0.957
        Median:  -0.483
          Mean:  -0.497
       3rd Qu.:  -0.092
          Max.:   0.645

=================================================================================

   time_domain_gravity_acceleration_magnitude_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.986
       1st Qu.:  -0.943
        Median:  -0.607
          Mean:  -0.544
       3rd Qu.:  -0.209
          Max.:   0.428

=================================================================================

   time_domain_body_gyroscope_jerk_magnitudereading_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.993
       1st Qu.:  -0.981
        Median:  -0.817
          Mean:  -0.608
       3rd Qu.:  -0.246
          Max.:   0.434

=================================================================================

   time_domain_body_gyroscope_jerk_magnitudereading_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.995
       1st Qu.:  -0.977
        Median:  -0.801
          Mean:  -0.584
       3rd Qu.:  -0.217
          Max.:   0.451

=================================================================================

   time_domain_body_gyroscope_magnitude_reading_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.981
       1st Qu.:  -0.946
        Median:  -0.655
          Mean:  -0.565
       3rd Qu.:  -0.216
          Max.:   0.418

=================================================================================

   time_domain_body_gyroscope_magnitude_reading_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.981
       1st Qu.:  -0.948
        Median:  -0.742
          Mean:  -0.630
       3rd Qu.:  -0.360
          Max.:   0.300

=================================================================================

   time_domain_body_gyroscope_jerk_magnitude_reading_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.997
       1st Qu.:  -0.985
        Median:  -0.865
          Mean:  -0.736
       3rd Qu.:  -0.512
          Max.:   0.088

=================================================================================

   time_domain_body_gyroscope_jerk_magnitude_reading_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.998
       1st Qu.:  -0.980
        Median:  -0.881
          Mean:  -0.755
       3rd Qu.:  -0.577
          Max.:   0.250

=================================================================================

   frequency_domain_body_acceleration_mean_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.995
       1st Qu.:  -0.979
        Median:  -0.769
          Mean:  -0.576
       3rd Qu.:  -0.217
          Max.:   0.537

=================================================================================

   frequency_domain_body_acceleration_mean_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.989
       1st Qu.:  -0.954
        Median:  -0.595
          Mean:  -0.489
       3rd Qu.:  -0.063
          Max.:   0.524

=================================================================================

   frequency_domain_body_acceleration_mean_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.989
       1st Qu.:  -0.962
        Median:  -0.724
          Mean:  -0.630
       3rd Qu.:  -0.318
          Max.:   0.281

=================================================================================

   frequency_domain_body_acceleration_standard_deviation_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.997
       1st Qu.:  -0.982
        Median:  -0.747
          Mean:  -0.552
       3rd Qu.:  -0.197
          Max.:   0.659

=================================================================================

   frequency_domain_body_acceleration_standard_deviation_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.991
       1st Qu.:  -0.940
        Median:  -0.513
          Mean:  -0.481
       3rd Qu.:  -0.079
          Max.:   0.560

=================================================================================

   frequency_domain_body_acceleration_standard_deviation_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.987
       1st Qu.:  -0.946
        Median:  -0.644
          Mean:  -0.582
       3rd Qu.:  -0.265
          Max.:   0.687

=================================================================================

   frequency_domain_body_acceleration_mean_frequency_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.636
       1st Qu.:  -0.392
        Median:  -0.257
          Mean:  -0.232
       3rd Qu.:  -0.061
          Max.:   0.159

=================================================================================

   frequency_domain_body_acceleration_mean_frequency_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.380
       1st Qu.:  -0.081
        Median:   0.008
          Mean:   0.012
       3rd Qu.:   0.086
          Max.:   0.467

=================================================================================

   frequency_domain_body_acceleration_mean_frequency_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.520
       1st Qu.:  -0.036
        Median:   0.066
          Mean:   0.044
       3rd Qu.:   0.175
          Max.:   0.403

=================================================================================

   frequency_domain_body_acceleration_jerk_mean_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.995
       1st Qu.:  -0.983
        Median:  -0.813
          Mean:  -0.614
       3rd Qu.:  -0.282
          Max.:   0.474

=================================================================================

   frequency_domain_body_acceleration_jerk_mean_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.989
       1st Qu.:  -0.973
        Median:  -0.782
          Mean:  -0.588
       3rd Qu.:  -0.196
          Max.:   0.277

=================================================================================

   frequency_domain_body_acceleration_jerk_mean_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.992
       1st Qu.:  -0.980
        Median:  -0.871
          Mean:  -0.714
       3rd Qu.:  -0.470
          Max.:   0.158

=================================================================================

   frequency_domain_body_acceleration_jerk_standard_deviation_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.995
       1st Qu.:  -0.985
        Median:  -0.825
          Mean:  -0.612
       3rd Qu.:  -0.248
          Max.:   0.477

=================================================================================

   frequency_domain_body_acceleration_jerk_standard_deviation_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.990
       1st Qu.:  -0.974
        Median:  -0.785
          Mean:  -0.571
       3rd Qu.:  -0.169
          Max.:   0.350

=================================================================================

   frequency_domain_body_acceleration_jerk_standard_deviation_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.993
       1st Qu.:  -0.984
        Median:  -0.895
          Mean:  -0.756
       3rd Qu.:  -0.544
          Max.:  -0.006

=================================================================================

   frequency_domain_body_acceleration_jerk_mean_frequency_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.576
       1st Qu.:  -0.290
        Median:  -0.061
          Mean:  -0.069
       3rd Qu.:   0.177
          Max.:   0.331

=================================================================================

   frequency_domain_body_acceleration_jerk_mean_frequency_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.602
       1st Qu.:  -0.398
        Median:  -0.232
          Mean:  -0.228
       3rd Qu.:  -0.047
          Max.:   0.196

=================================================================================

   frequency_domain_body_acceleration_jerk_mean_frequency_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.628
       1st Qu.:  -0.309
        Median:  -0.092
          Mean:  -0.138
       3rd Qu.:   0.039
          Max.:   0.230

=================================================================================

   frequency_domain_body_gyroscope_reading_mean_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.993
       1st Qu.:  -0.970
        Median:  -0.730
          Mean:  -0.637
       3rd Qu.:  -0.339
          Max.:   0.475

=================================================================================

   frequency_domain_body_gyroscope_reading_mean_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.994
       1st Qu.:  -0.970
        Median:  -0.814
          Mean:  -0.677
       3rd Qu.:  -0.446
          Max.:   0.329

=================================================================================

   frequency_domain_body_gyroscope_reading_mean_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.986
       1st Qu.:  -0.962
        Median:  -0.791
          Mean:  -0.604
       3rd Qu.:  -0.263
          Max.:   0.492

=================================================================================

   frequency_domain_body_gyroscope_reading_standard_deviation_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.995
       1st Qu.:  -0.975
        Median:  -0.809
          Mean:  -0.711
       3rd Qu.:  -0.481
          Max.:   0.197

=================================================================================

   frequency_domain_body_gyroscope_reading_standard_deviation_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.994
       1st Qu.:  -0.960
        Median:  -0.796
          Mean:  -0.645
       3rd Qu.:  -0.415
          Max.:   0.646

=================================================================================

   frequency_domain_body_gyroscope_reading_standard_deviation_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.987
       1st Qu.:  -0.964
        Median:  -0.822
          Mean:  -0.658
       3rd Qu.:  -0.392
          Max.:   0.522

=================================================================================

   frequency_domain_body_gyroscope_reading_mean_frequency_X_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.396
       1st Qu.:  -0.213
        Median:  -0.116
          Mean:  -0.105
       3rd Qu.:   0.003
          Max.:   0.249

=================================================================================

   frequency_domain_body_gyroscope_reading_mean_frequency_Y_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.667
       1st Qu.:  -0.294
        Median:  -0.158
          Mean:  -0.167
       3rd Qu.:  -0.043
          Max.:   0.273

=================================================================================

   frequency_domain_body_gyroscope_reading_mean_frequency_Z_Axis

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.507
       1st Qu.:  -0.155
        Median:  -0.051
          Mean:  -0.057
       3rd Qu.:   0.042
          Max.:   0.377

=================================================================================

   frequency_domain_body_acceleration_magnitude_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.987
       1st Qu.:  -0.956
        Median:  -0.670
          Mean:  -0.537
       3rd Qu.:  -0.162
          Max.:   0.587

=================================================================================

   frequency_domain_body_acceleration_magnitude_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.988
       1st Qu.:  -0.945
        Median:  -0.651
          Mean:  -0.621
       3rd Qu.:  -0.365
          Max.:   0.179

=================================================================================

   frequency_domain_body_acceleration_magnitude_mean_frequency

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.312
       1st Qu.:  -0.015
        Median:   0.081
          Mean:   0.076
       3rd Qu.:   0.174
          Max.:   0.436

=================================================================================

   frequency_domain_body_gyroscope_jerk_magnitudereading_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.994
       1st Qu.:  -0.977
        Median:  -0.794
          Mean:  -0.576
       3rd Qu.:  -0.187
          Max.:   0.538

=================================================================================

   frequency_domain_body_gyroscope_jerk_magnitudereading_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.994
       1st Qu.:  -0.975
        Median:  -0.813
          Mean:  -0.599
       3rd Qu.:  -0.267
          Max.:   0.316

=================================================================================

   frequency_domain_body_gyroscope_jerk_magnitudereading_mean_frequency

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.125
       1st Qu.:   0.045
        Median:   0.172
          Mean:   0.163
       3rd Qu.:   0.276
          Max.:   0.488

=================================================================================

   frequency_domain_body_gyroscope_magnitude_reading_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.987
       1st Qu.:  -0.962
        Median:  -0.766
          Mean:  -0.667
       3rd Qu.:  -0.409
          Max.:   0.204

=================================================================================

   frequency_domain_body_gyroscope_magnitude_reading_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.981
       1st Qu.:  -0.949
        Median:  -0.773
          Mean:  -0.672
       3rd Qu.:  -0.428
          Max.:   0.237

=================================================================================

   frequency_domain_body_gyroscope_magnitude_reading_mean_frequency

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.457
       1st Qu.:  -0.170
        Median:  -0.054
          Mean:  -0.036
       3rd Qu.:   0.082
          Max.:   0.410

=================================================================================

   frequency_domain_body_gyroscope_jerk_magnitude_reading_mean

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.998
       1st Qu.:  -0.981
        Median:  -0.878
          Mean:  -0.756
       3rd Qu.:  -0.583
          Max.:   0.147

=================================================================================

   frequency_domain_body_gyroscope_jerk_magnitude_reading_standard_deviation

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.998
       1st Qu.:  -0.980
        Median:  -0.894
          Mean:  -0.772
       3rd Qu.:  -0.608
          Max.:   0.288

=================================================================================

   frequency_domain_body_gyroscope_jerk_magnitude_reading_mean_frequency

---------------------------------------------------------------------------------

   Storage mode: double

          Min.:  -0.183
       1st Qu.:   0.054
        Median:   0.112
          Mean:   0.126
       3rd Qu.:   0.208
          Max.:   0.426

