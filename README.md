# CourseProject
Course Project Getting and Clenaing the Data

The script works as follows:

The files of UCI were imported to the environment using `read.table` 

Put the column names from the features dataset 

Added activity and subject columns

Merged test and train datasets

Extracted mean and std columns

Change to more descriptive names for variables

Labeled data

Created new tidy dataset with mean of all variables.

A more detailed view in the following CODE BOOK:

# Data report overview
The dataset examined has the following dimensions:


---------------------------------
Feature                    Result
------------------------ --------
Number of observations        180

Number of variables            82
---------------------------------


1:30


# Codebook summary table

-------------------------------------------------------------------------------------------------
Label   Variable                                   Class         # unique  Missing  Description  
                                                                   values                        
------- ------------------------------------------ ----------- ---------- --------- -------------
        **[subject]**                              integer             30  0.00 %                

        **[activity]**                             integer              6  0.00 %                

        **[actdescr]**                             character            6  0.00 %                

        **[acc\_raw-mean()-X]**                    numeric            180  0.00 %                

        **[acc\_raw-mean()-Y]**                    numeric            180  0.00 %                

        **[acc\_raw-mean()-Z]**                    numeric            180  0.00 %                

        **[acc\_grav\_raw-mean()-X]**              numeric            180  0.00 %                

        **[acc\_grav\_raw-mean()-Y]**              numeric            180  0.00 %                

        **[acc\_grav\_raw-mean()-Z]**              numeric            180  0.00 %                

        **[acc\_jerk-mean()-X]**                   numeric            180  0.00 %                

        **[acc\_jerk-mean()-Y]**                   numeric            180  0.00 %                

        **[acc\_jerk-mean()-Z]**                   numeric            180  0.00 %                

        **[tBodyGyro-mean()-X]**                   numeric            180  0.00 %                

        **[tBodyGyro-mean()-Y]**                   numeric            180  0.00 %                

        **[tBodyGyro-mean()-Z]**                   numeric            180  0.00 %                

        **[gyro\_jerk-mean()-X]**                  numeric            180  0.00 %                

        **[gyro\_jerk-mean()-Y]**                  numeric            180  0.00 %                

        **[gyro\_jerk-mean()-Z]**                  numeric            180  0.00 %                

        **[acc\_magnitude-mean()]**                numeric            180  0.00 %                

        **[acc\_gravity\_magnitude-mean()]**       numeric            180  0.00 %                

        **[acc\_jerk\_magnitude-mean()]**          numeric            180  0.00 %                

        **[gyro\_magnitude-mean()]**               numeric            180  0.00 %                

        **[gyro\_jerk\_magnitude-mean()]**         numeric            180  0.00 %                

        **[fourier\_acc\_raw-mean()-X]**           numeric            180  0.00 %                

        **[fourier\_acc\_raw-mean()-Y]**           numeric            180  0.00 %                

        **[fourier\_acc\_raw-mean()-Z]**           numeric            180  0.00 %                

        **[fourier\_acc\_raw-meanFreq()-X]**       numeric            180  0.00 %                

        **[fourier\_acc\_raw-meanFreq()-Y]**       numeric            180  0.00 %                

        **[fourier\_acc\_raw-meanFreq()-Z]**       numeric            180  0.00 %                

        **[fourier\_acc\_jerk-mean()-X]**          numeric            180  0.00 %                

        **[fourier\_acc\_jerk-mean()-Y]**          numeric            180  0.00 %                

        **[fourier\_acc\_jerk-mean()-Z]**          numeric            180  0.00 %                

        **[fourier\_acc\_jerk-meanFreq()-X]**      numeric            180  0.00 %                

        **[fourier\_acc\_jerk-meanFreq()-Y]**      numeric            180  0.00 %                

        **[fourier\_acc\_jerk-meanFreq()-Z]**      numeric            180  0.00 %                

        **[fourier\_gyro\_raw-mean()-X]**          numeric            180  0.00 %                

        **[fourier\_gyro\_raw-mean()-Y]**          numeric            180  0.00 %                

        **[fourier\_gyro\_raw-mean()-Z]**          numeric            180  0.00 %                

        **[fourier\_gyro\_raw-meanFreq()-X]**      numeric            180  0.00 %                

        **[fourier\_gyro\_raw-meanFreq()-Y]**      numeric            180  0.00 %                

        **[fourier\_gyro\_raw-meanFreq()-Z]**      numeric            180  0.00 %                

        **[fourier\_acc\_magnitude-mean()]**       numeric            180  0.00 %                

        **[fourier\_acc\_magnitude-meanFreq()]**   numeric            180  0.00 %                

        **[fBodyBodyAccJerkMag-mean()]**           numeric            180  0.00 %                

        **[fBodyBodyAccJerkMag-meanFreq()]**       numeric            180  0.00 %                

        **[fBodyBodyGyroMag-mean()]**              numeric            180  0.00 %                

        **[fBodyBodyGyroMag-meanFreq()]**          numeric            180  0.00 %                

        **[fBodyBodyGyroJerkMag-mean()]**          numeric            180  0.00 %                

        **[fBodyBodyGyroJerkMag-meanFreq()]**      numeric            180  0.00 %                

        **[acc\_raw-std()-X]**                     numeric            180  0.00 %                

        **[acc\_raw-std()-Y]**                     numeric            180  0.00 %                

        **[acc\_raw-std()-Z]**                     numeric            180  0.00 %                

        **[acc\_grav\_raw-std()-X]**               numeric            180  0.00 %                

        **[acc\_grav\_raw-std()-Y]**               numeric            180  0.00 %                

        **[acc\_grav\_raw-std()-Z]**               numeric            180  0.00 %                

        **[acc\_jerk-std()-X]**                    numeric            180  0.00 %                

        **[acc\_jerk-std()-Y]**                    numeric            180  0.00 %                

        **[acc\_jerk-std()-Z]**                    numeric            180  0.00 %                

        **[tBodyGyro-std()-X]**                    numeric            180  0.00 %                

        **[tBodyGyro-std()-Y]**                    numeric            180  0.00 %                

        **[tBodyGyro-std()-Z]**                    numeric            180  0.00 %                

        **[gyro\_jerk-std()-X]**                   numeric            180  0.00 %                

        **[gyro\_jerk-std()-Y]**                   numeric            180  0.00 %                

        **[gyro\_jerk-std()-Z]**                   numeric            180  0.00 %                

        **[acc\_magnitude-std()]**                 numeric            180  0.00 %                

        **[acc\_gravity\_magnitude-std()]**        numeric            180  0.00 %                

        **[acc\_jerk\_magnitude-std()]**           numeric            180  0.00 %                

        **[gyro\_magnitude-std()]**                numeric            180  0.00 %                

        **[gyro\_jerk\_magnitude-std()]**          numeric            180  0.00 %                

        **[fourier\_acc\_raw-std()-X]**            numeric            180  0.00 %                

        **[fourier\_acc\_raw-std()-Y]**            numeric            180  0.00 %                

        **[fourier\_acc\_raw-std()-Z]**            numeric            180  0.00 %                

        **[fourier\_acc\_jerk-std()-X]**           numeric            180  0.00 %                

        **[fourier\_acc\_jerk-std()-Y]**           numeric            180  0.00 %                

        **[fourier\_acc\_jerk-std()-Z]**           numeric            180  0.00 %                

        **[fourier\_gyro\_raw-std()-X]**           numeric            180  0.00 %                

        **[fourier\_gyro\_raw-std()-Y]**           numeric            180  0.00 %                

        **[fourier\_gyro\_raw-std()-Z]**           numeric            180  0.00 %                

        **[fourier\_acc\_magnitude-std()]**        numeric            180  0.00 %                

        **[fBodyBodyAccJerkMag-std()]**            numeric            180  0.00 %                

        **[fBodyBodyGyroMag-std()]**               numeric            180  0.00 %                

        **[fBodyBodyGyroJerkMag-std()]**           numeric            180  0.00 %                
-------------------------------------------------------------------------------------------------




# Variable list
## subject

<div class = "row">
<div class = "col-lg-8">

-----------------------------------
Feature                      Result
------------------------- ---------
Variable type               integer

Number of missing obs.      0 (0 %)

Number of unique values          30

Median                         15.5

1st and 3rd quartiles         8; 23

Min. and max.                 1; 30
-----------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-1-subject', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(0, 5, 10, 15, 20, 25
), xmax = c(5, 10, 15, 20, 25, 30), ymin = c(0, 0, 0, 0, 0, 0
), ymax = c(30L, 30L, 30L, 30L, 30L, 30L)), class = "data.frame", row.names = c(NA, 
-6L)), vnam = "subject")
```
</div>
</div>




---

## activity

<div class = "row">
<div class = "col-lg-8">

-----------------------------------
Feature                      Result
------------------------- ---------
Variable type               integer

Number of missing obs.      0 (0 %)

Number of unique values           6

Median                          3.5

1st and 3rd quartiles          2; 5

Min. and max.                  1; 6
-----------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-2-activity', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(1, 1.5, 2, 2.5, 3, 3.5, 
4, 4.5, 5, 5.5), xmax = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 
6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(30L, 30L, 
0L, 30L, 0L, 30L, 0L, 30L, 0L, 30L)), class = "data.frame", row.names = c(NA, 
-10L)), vnam = "activity")
```
</div>
</div>




---

## actdescr

<div class = "row">
<div class = "col-lg-8">

-------------------------------------
Feature                        Result
------------------------- -----------
Variable type               character

Number of missing obs.        0 (0 %)

Number of unique values             6

Mode                         "LAYING"
-------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-3-actdescr', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggBarplot(data = structure(list(x = structure(1:6, .Label = c("LAYING", 
"SITTING", "STANDING", "WALKING", "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS"
), class = "factor"), y = c(30L, 30L, 30L, 30L, 30L, 30L)), class = "data.frame", row.names = c(NA, 
-6L)), vnam = "actdescr")
```
</div>
</div>


- Observed factor levels: \"LAYING\", \"SITTING\", \"STANDING\", \"WALKING\", \"WALKING_DOWNSTAIRS\", \"WALKING_UPSTAIRS\". 



---

## acc\_raw-mean()-X

<div class = "row">
<div class = "col-lg-8">

--------------------------------------
Feature                         Result
------------------------- ------------
Variable type                  numeric

Number of missing obs.         0 (0 %)

Number of unique values            180

Median                            0.28

1st and 3rd quartiles       0.27; 0.28

Min. and max.                0.22; 0.3
--------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-4-acc-raw-mean()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(0.22, 0.23, 0.24, 0.25, 
0.26, 0.27, 0.28, 0.29, 0.3), xmax = c(0.23, 0.24, 0.25, 0.26, 
0.27, 0.28, 0.29, 0.3, 0.31), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 
0), ymax = c(1L, 2L, 7L, 11L, 21L, 93L, 30L, 14L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "acc_raw-mean()-X")
```
</div>
</div>




---

## acc\_raw-mean()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.02

1st and 3rd quartiles       -0.02; -0.01

Min. and max.                   -0.04; 0
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-5-acc-raw-mean()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.045, -0.04, -0.035, 
-0.03, -0.025, -0.02, -0.015, -0.01, -0.005), xmax = c(-0.04, 
-0.035, -0.03, -0.025, -0.02, -0.015, -0.01, -0.005, 0), ymin = c(0, 
0, 0, 0, 0, 0, 0, 0, 0), ymax = c(1L, 0L, 6L, 15L, 25L, 87L, 
30L, 14L, 2L)), class = "data.frame", row.names = c(NA, -9L)), 
    vnam = "acc_raw-mean()-Y")
```
</div>
</div>




---

## acc\_raw-mean()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.11

1st and 3rd quartiles        -0.11; -0.1

Min. and max.               -0.15; -0.08
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-6-acc-raw-mean()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.16, -0.15, -0.14, 
-0.13, -0.12, -0.11, -0.1, -0.09, -0.08), xmax = c(-0.15, -0.14, 
-0.13, -0.12, -0.11, -0.1, -0.09, -0.08, -0.07), ymin = c(0, 
0, 0, 0, 0, 0, 0, 0, 0), ymax = c(2L, 1L, 2L, 15L, 48L, 92L, 
16L, 3L, 1L)), class = "data.frame", row.names = c(NA, -9L)), 
    vnam = "acc_raw-mean()-Z")
```
</div>
</div>




---

## acc\_grav\_raw-mean()-X

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.92

1st and 3rd quartiles        0.84; 0.94

Min. and max.               -0.68; 0.97
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-7-acc-grav-raw-mean()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.8, -0.6, -0.4, -0.2, 
0, 0.2, 0.4, 0.6, 0.8), xmax = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 
0.6, 0.8, 1), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(2L, 
13L, 9L, 6L, 0L, 0L, 1L, 6L, 143L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "acc_grav_raw-mean()-X")
```
</div>
</div>




---

## acc\_grav\_raw-mean()-Y

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.13

1st and 3rd quartiles       -0.23; 0.09

Min. and max.               -0.48; 0.96
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-8-acc-grav-raw-mean()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), xmax = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 
1), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(3L, 66L, 61L, 
14L, 11L, 5L, 10L, 10L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_grav_raw-mean()-Y")
```
</div>
</div>




---

## acc\_grav\_raw-mean()-Z

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.02

1st and 3rd quartiles       -0.12; 0.15

Min. and max.                -0.5; 0.96
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-9-acc-grav-raw-mean()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), xmax = c(-0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 
1), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(3L, 18L, 60L, 
63L, 13L, 7L, 10L, 6L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_grav_raw-mean()-Z")
```
</div>
</div>




---

## acc\_jerk-mean()-X

<div class = "row">
<div class = "col-lg-8">

--------------------------------------
Feature                         Result
------------------------- ------------
Variable type                  numeric

Number of missing obs.         0 (0 %)

Number of unique values            180

Median                            0.08

1st and 3rd quartiles       0.07; 0.08

Min. and max.               0.04; 0.13
--------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-10-acc-jerk-mean()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(0.04, 0.05, 0.06, 0.07, 
0.08, 0.09, 0.1, 0.11, 0.12, 0.13), xmax = c(0.05, 0.06, 0.07, 
0.08, 0.09, 0.1, 0.11, 0.12, 0.13, 0.14), ymin = c(0, 0, 0, 0, 
0, 0, 0, 0, 0, 0), ymax = c(3L, 4L, 13L, 98L, 34L, 14L, 8L, 5L, 
0L, 1L)), class = "data.frame", row.names = c(NA, -10L)), vnam = "acc_jerk-mean()-X")
```
</div>
</div>




---

## acc\_jerk-mean()-Y

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.01

1st and 3rd quartiles           0; 0.01

Min. and max.               -0.04; 0.06
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-11-acc-jerk-mean()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.04, -0.03, -0.02, 
-0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05), xmax = c(-0.03, -0.02, 
-0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06), ymin = c(0, 0, 
0, 0, 0, 0, 0, 0, 0, 0), ymax = c(2L, 3L, 13L, 26L, 52L, 59L, 
18L, 6L, 0L, 1L)), class = "data.frame", row.names = c(NA, -10L
)), vnam = "acc_jerk-mean()-Y")
```
</div>
</div>




---

## acc\_jerk-mean()-Z

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                                0

1st and 3rd quartiles          -0.01; 0

Min. and max.               -0.07; 0.04
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-12-acc-jerk-mean()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.07, -0.06, -0.05, 
-0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03), xmax = c(-0.06, 
-0.05, -0.04, -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03, 0.04), 
    ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(1L, 1L, 
    1L, 6L, 12L, 27L, 77L, 41L, 9L, 3L, 2L)), class = "data.frame", row.names = c(NA, 
-11L)), vnam = "acc_jerk-mean()-Z")
```
</div>
</div>




---

## tBodyGyro-mean()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.03

1st and 3rd quartiles       -0.05; -0.02

Min. and max.                -0.21; 0.19
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-13-tBodyGyro-mean()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.25, -0.2, -0.15, 
-0.1, -0.05, 0, 0.05, 0.1, 0.15), xmax = c(-0.2, -0.15, -0.1, 
-0.05, 0, 0.05, 0.1, 0.15, 0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 
0, 0), ymax = c(1L, 2L, 18L, 20L, 113L, 11L, 13L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "tBodyGyro-mean()-X")
```
</div>
</div>




---

## tBodyGyro-mean()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.07

1st and 3rd quartiles       -0.09; -0.06

Min. and max.                 -0.2; 0.03
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-14-tBodyGyro-mean()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.22, -0.2, -0.18, 
-0.16, -0.14, -0.12, -0.1, -0.08, -0.06, -0.04, -0.02, 0, 0.02
), xmax = c(-0.2, -0.18, -0.16, -0.14, -0.12, -0.1, -0.08, -0.06, 
-0.04, -0.02, 0, 0.02, 0.04), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0), ymax = c(1L, 1L, 2L, 5L, 5L, 12L, 38L, 72L, 23L, 
9L, 5L, 5L, 2L)), class = "data.frame", row.names = c(NA, -13L
)), vnam = "tBodyGyro-mean()-Y")
```
</div>
</div>




---

## tBodyGyro-mean()-Z

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.09

1st and 3rd quartiles         0.07; 0.1

Min. and max.               -0.07; 0.18
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-15-tBodyGyro-mean()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.08, -0.06, -0.04, 
-0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16), xmax = c(-0.06, 
-0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16, 
0.18), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(1L, 
1L, 1L, 3L, 2L, 4L, 11L, 36L, 74L, 23L, 8L, 13L, 3L)), class = "data.frame", row.names = c(NA, 
-13L)), vnam = "tBodyGyro-mean()-Z")
```
</div>
</div>




---

## gyro\_jerk-mean()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                              -0.1

1st and 3rd quartiles        -0.1; -0.09

Min. and max.               -0.16; -0.02
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-16-gyro-jerk-mean()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.16, -0.14, -0.12, 
-0.1, -0.08, -0.06, -0.04), xmax = c(-0.14, -0.12, -0.1, -0.08, 
-0.06, -0.04, -0.02), ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(10L, 
10L, 55L, 76L, 16L, 7L, 6L)), class = "data.frame", row.names = c(NA, 
-7L)), vnam = "gyro_jerk-mean()-X")
```
</div>
</div>




---

## gyro\_jerk-mean()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.04

1st and 3rd quartiles       -0.05; -0.04

Min. and max.               -0.08; -0.01
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-17-gyro-jerk-mean()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.08, -0.07, -0.06, 
-0.05, -0.04, -0.03, -0.02), xmax = c(-0.07, -0.06, -0.05, -0.04, 
-0.03, -0.02, -0.01), ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(5L, 
5L, 21L, 78L, 62L, 6L, 3L)), class = "data.frame", row.names = c(NA, 
-7L)), vnam = "gyro_jerk-mean()-Y")
```
</div>
</div>




---

## gyro\_jerk-mean()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.05

1st and 3rd quartiles       -0.06; -0.05

Min. and max.               -0.09; -0.01
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-18-gyro-jerk-mean()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.1, -0.09, -0.08, 
-0.07, -0.06, -0.05, -0.04, -0.03, -0.02, -0.01), xmax = c(-0.09, 
-0.08, -0.07, -0.06, -0.05, -0.04, -0.03, -0.02, -0.01, 0), ymin = c(0, 
0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(2L, 1L, 17L, 33L, 74L, 36L, 
11L, 4L, 1L, 1L)), class = "data.frame", row.names = c(NA, -10L
)), vnam = "gyro_jerk-mean()-Z")
```
</div>
</div>




---

## acc\_magnitude-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.48

1st and 3rd quartiles       -0.96; -0.09

Min. and max.                -0.99; 0.64
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-19-acc-magnitude-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4, 0.6), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(89L, 
0L, 2L, 17L, 37L, 26L, 8L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "acc_magnitude-mean()")
```
</div>
</div>




---

## acc\_gravity\_magnitude-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.48

1st and 3rd quartiles       -0.96; -0.09

Min. and max.                -0.99; 0.64
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-20-acc-gravity-magnitude-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4, 0.6), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(89L, 
0L, 2L, 17L, 37L, 26L, 8L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "acc_gravity_magnitude-mean()")
```
</div>
</div>




---

## acc\_jerk\_magnitude-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.82

1st and 3rd quartiles       -0.98; -0.25

Min. and max.                -0.99; 0.43
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-21-acc-jerk-magnitude-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 
17L, 33L, 29L, 7L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_jerk_magnitude-mean()")
```
</div>
</div>




---

## gyro\_magnitude-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.66

1st and 3rd quartiles       -0.95; -0.22

Min. and max.                -0.98; 0.42
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-22-gyro-magnitude-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 0L, 
7L, 42L, 28L, 11L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "gyro_magnitude-mean()")
```
</div>
</div>




---

## gyro\_jerk\_magnitude-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.86

1st and 3rd quartiles       -0.99; -0.51

Min. and max.                   -1; 0.09
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-23-gyro-jerk-magnitude-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0), xmax = c(-0.9, -0.8, 
-0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1), ymin = c(0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 0L, 6L, 19L, 26L, 
16L, 12L, 4L, 3L, 2L, 2L)), class = "data.frame", row.names = c(NA, 
-11L)), vnam = "gyro_jerk_magnitude-mean()")
```
</div>
</div>




---

## fourier\_acc\_raw-mean()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.77

1st and 3rd quartiles       -0.98; -0.22

Min. and max.                   -1; 0.54
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-24-fourier-acc-raw-mean()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 1L, 
9L, 36L, 23L, 16L, 4L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_acc_raw-mean()-X")
```
</div>
</div>




---

## fourier\_acc\_raw-mean()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.59

1st and 3rd quartiles       -0.95; -0.06

Min. and max.                -0.99; 0.52
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-25-fourier-acc-raw-mean()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(89L, 1L, 
1L, 14L, 40L, 25L, 7L, 3L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_acc_raw-mean()-Y")
```
</div>
</div>




---

## fourier\_acc\_raw-mean()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.72

1st and 3rd quartiles       -0.96; -0.32

Min. and max.                -0.99; 0.28
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-26-fourier-acc-raw-mean()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 28L, 40L, 
    13L, 5L, 2L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "fourier_acc_raw-mean()-Z")
```
</div>
</div>




---

## fourier\_acc\_raw-meanFreq()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.26

1st and 3rd quartiles       -0.39; -0.06

Min. and max.                -0.64; 0.16
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-27-fourier-acc-raw-meanFreq()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.7, -0.6, -0.5, -0.4, 
-0.3, -0.2, -0.1, 0, 0.1), xmax = c(-0.6, -0.5, -0.4, -0.3, -0.2, 
-0.1, 0, 0.1, 0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(1L, 
12L, 31L, 37L, 25L, 19L, 27L, 23L, 5L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "fourier_acc_raw-meanFreq()-X")
```
</div>
</div>




---

## fourier\_acc\_raw-meanFreq()-Y

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.01

1st and 3rd quartiles       -0.08; 0.09

Min. and max.               -0.38; 0.47
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-28-fourier-acc-raw-meanFreq()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.4, -0.3, -0.2, -0.1, 
0, 0.1, 0.2, 0.3, 0.4), xmax = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 
0.3, 0.4, 0.5), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(3L, 
8L, 28L, 41L, 59L, 22L, 14L, 4L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "fourier_acc_raw-meanFreq()-Y")
```
</div>
</div>




---

## fourier\_acc\_raw-meanFreq()-Z

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.07

1st and 3rd quartiles       -0.04; 0.18

Min. and max.                -0.52; 0.4
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-29-fourier-acc-raw-meanFreq()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.6, -0.5, -0.4, -0.3, 
-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4), xmax = c(-0.5, -0.4, -0.3, 
-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5), ymin = c(0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0), ymax = c(3L, 3L, 2L, 12L, 13L, 26L, 50L, 
34L, 28L, 8L, 1L)), class = "data.frame", row.names = c(NA, -11L
)), vnam = "fourier_acc_raw-meanFreq()-Z")
```
</div>
</div>




---

## fourier\_acc\_jerk-mean()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.81

1st and 3rd quartiles       -0.98; -0.28

Min. and max.                -0.99; 0.47
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-30-fourier-acc-jerk-mean()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 
21L, 35L, 23L, 6L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_acc_jerk-mean()-X")
```
</div>
</div>




---

## fourier\_acc\_jerk-mean()-Y

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.78

1st and 3rd quartiles       -0.97; -0.2

Min. and max.               -0.99; 0.28
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-31-fourier-acc-jerk-mean()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 14L, 28L, 
    34L, 9L, 3L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "fourier_acc_jerk-mean()-Y")
```
</div>
</div>




---

## fourier\_acc\_jerk-mean()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.87

1st and 3rd quartiles       -0.98; -0.47

Min. and max.                -0.99; 0.16
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-32-fourier-acc-jerk-mean()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999, 
0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 
0L, 6L, 17L, 15L, 17L, 16L, 11L, 3L, 2L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-12L)), vnam = "fourier_acc_jerk-mean()-Z")
```
</div>
</div>




---

## fourier\_acc\_jerk-meanFreq()-X

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.06

1st and 3rd quartiles       -0.29; 0.18

Min. and max.               -0.58; 0.33
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-33-fourier-acc-jerk-meanFreq()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.6, -0.5, -0.4, -0.3, 
-0.2, -0.1, 0, 0.1, 0.2, 0.3), xmax = c(-0.5, -0.4, -0.3, -0.2, 
-0.1, 0, 0.1, 0.2, 0.3, 0.4), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 
0, 0), ymax = c(3L, 12L, 29L, 34L, 10L, 5L, 20L, 33L, 29L, 5L
)), class = "data.frame", row.names = c(NA, -10L)), vnam = "fourier_acc_jerk-meanFreq()-X")
```
</div>
</div>




---

## fourier\_acc\_jerk-meanFreq()-Y

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.23

1st and 3rd quartiles       -0.4; -0.05

Min. and max.                 -0.6; 0.2
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-34-fourier-acc-jerk-meanFreq()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.7, -0.6, -0.5, -0.4, 
-0.3, -0.2, -0.1, 0, 0.1), xmax = c(-0.6, -0.5, -0.4, -0.3, -0.2, 
-0.1, 0, 0.1, 0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(1L, 
13L, 31L, 28L, 25L, 20L, 34L, 24L, 4L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "fourier_acc_jerk-meanFreq()-Y")
```
</div>
</div>




---

## fourier\_acc\_jerk-meanFreq()-Z

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.09

1st and 3rd quartiles       -0.31; 0.04

Min. and max.               -0.63; 0.23
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-35-fourier-acc-jerk-meanFreq()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.7, -0.6, -0.5, -0.4, 
-0.3, -0.2, -0.1, 0, 0.1, 0.2), xmax = c(-0.6, -0.5, -0.4, -0.3, 
-0.2, -0.1, 0, 0.1, 0.2, 0.3), ymin = c(0, 0, 0, 0, 0, 0, 0, 
0, 0, 0), ymax = c(3L, 8L, 14L, 25L, 15L, 21L, 28L, 51L, 14L, 
1L)), class = "data.frame", row.names = c(NA, -10L)), vnam = "fourier_acc_jerk-meanFreq()-Z")
```
</div>
</div>




---

## fourier\_gyro\_raw-mean()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.73

1st and 3rd quartiles       -0.97; -0.34

Min. and max.                -0.99; 0.47
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-36-fourier-gyro-raw-mean()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 1L, 
29L, 41L, 16L, 1L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_gyro_raw-mean()-X")
```
</div>
</div>




---

## fourier\_gyro\_raw-mean()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.81

1st and 3rd quartiles       -0.97; -0.45

Min. and max.                -0.99; 0.33
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-37-fourier-gyro-raw-mean()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 13L, 38L, 22L, 
    10L, 6L, 1L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "fourier_gyro_raw-mean()-Y")
```
</div>
</div>




---

## fourier\_gyro\_raw-mean()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.79

1st and 3rd quartiles       -0.96; -0.26

Min. and max.                -0.99; 0.49
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-38-fourier-gyro-raw-mean()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 
17L, 41L, 21L, 6L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_gyro_raw-mean()-Z")
```
</div>
</div>




---

## fourier\_gyro\_raw-meanFreq()-X

<div class = "row">
<div class = "col-lg-8">

--------------------------------------
Feature                         Result
------------------------- ------------
Variable type                  numeric

Number of missing obs.         0 (0 %)

Number of unique values            180

Median                           -0.12

1st and 3rd quartiles         -0.21; 0

Min. and max.               -0.4; 0.25
--------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-39-fourier-gyro-raw-meanFreq()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.4, -0.3, -0.2, -0.1, 
0, 0.1, 0.2), xmax = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3), ymin = c(0, 
0, 0, 0, 0, 0, 0), ymax = c(14L, 37L, 50L, 33L, 22L, 20L, 4L)), class = "data.frame", row.names = c(NA, 
-7L)), vnam = "fourier_gyro_raw-meanFreq()-X")
```
</div>
</div>




---

## fourier\_gyro\_raw-meanFreq()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.16

1st and 3rd quartiles       -0.29; -0.04

Min. and max.                -0.67; 0.27
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-40-fourier-gyro-raw-meanFreq()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.7, -0.6, -0.5, -0.4, 
-0.3, -0.2, -0.1, 0, 0.1, 0.2), xmax = c(-0.6, -0.5, -0.4, -0.3, 
-0.2, -0.1, 0, 0.1, 0.2, 0.3), ymin = c(0, 0, 0, 0, 0, 0, 0, 
0, 0, 0), ymax = c(3L, 4L, 9L, 27L, 24L, 51L, 29L, 23L, 7L, 3L
)), class = "data.frame", row.names = c(NA, -10L)), vnam = "fourier_gyro_raw-meanFreq()-Y")
```
</div>
</div>




---

## fourier\_gyro\_raw-meanFreq()-Z

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.05

1st and 3rd quartiles       -0.15; 0.04

Min. and max.               -0.51; 0.38
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-41-fourier-gyro-raw-meanFreq()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.6, -0.5, -0.4, -0.3, 
-0.2, -0.1, 0, 0.1, 0.2, 0.3), xmax = c(-0.5, -0.4, -0.3, -0.2, 
-0.1, 0, 0.1, 0.2, 0.3, 0.4), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 
0, 0), ymax = c(2L, 4L, 7L, 19L, 38L, 42L, 38L, 18L, 9L, 3L)), class = "data.frame", row.names = c(NA, 
-10L)), vnam = "fourier_gyro_raw-meanFreq()-Z")
```
</div>
</div>




---

## fourier\_acc\_magnitude-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.67

1st and 3rd quartiles       -0.96; -0.16

Min. and max.                -0.99; 0.59
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-42-fourier-acc-magnitude-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(89L, 1L, 
10L, 31L, 22L, 16L, 9L, 2L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_acc_magnitude-mean()")
```
</div>
</div>




---

## fourier\_acc\_magnitude-meanFreq()

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.08

1st and 3rd quartiles       -0.01; 0.17

Min. and max.               -0.31; 0.44
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-43-fourier-acc-magnitude-meanFreq()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.4, -0.3, -0.2, -0.1, 
0, 0.1, 0.2, 0.3, 0.4), xmax = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 
0.3, 0.4, 0.5), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(1L, 
3L, 16L, 33L, 52L, 40L, 26L, 8L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "fourier_acc_magnitude-meanFreq()")
```
</div>
</div>




---

## fBodyBodyAccJerkMag-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.79

1st and 3rd quartiles       -0.98; -0.19

Min. and max.                -0.99; 0.54
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-44-fBodyBodyAccJerkMag-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 1L, 
16L, 25L, 26L, 17L, 4L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fBodyBodyAccJerkMag-mean()")
```
</div>
</div>




---

## fBodyBodyAccJerkMag-meanFreq()

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.17

1st and 3rd quartiles        0.05; 0.28

Min. and max.               -0.13; 0.49
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-45-fBodyBodyAccJerkMag-meanFreq()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.15, -0.1, -0.05, 
0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45), xmax = c(-0.1, 
-0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5
), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(3L, 
6L, 18L, 21L, 16L, 19L, 24L, 18L, 20L, 18L, 12L, 3L, 2L)), class = "data.frame", row.names = c(NA, 
-13L)), vnam = "fBodyBodyAccJerkMag-meanFreq()")
```
</div>
</div>




---

## fBodyBodyGyroMag-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.77

1st and 3rd quartiles       -0.96; -0.41

Min. and max.                 -0.99; 0.2
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-46-fBodyBodyGyroMag-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 
0.3), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(88L, 
2L, 0L, 8L, 20L, 18L, 22L, 9L, 4L, 5L, 1L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-13L)), vnam = "fBodyBodyGyroMag-mean()")
```
</div>
</div>




---

## fBodyBodyGyroMag-meanFreq()

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.05

1st and 3rd quartiles       -0.17; 0.08

Min. and max.               -0.46; 0.41
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-47-fBodyBodyGyroMag-meanFreq()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.5, -0.4, -0.3, -0.2, 
-0.1, 0, 0.1, 0.2, 0.3, 0.4), xmax = c(-0.4, -0.3, -0.2, -0.1, 
0, 0.1, 0.2, 0.3, 0.4, 0.5), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 
0, 0), ymax = c(2L, 9L, 17L, 48L, 34L, 27L, 21L, 13L, 7L, 2L)), class = "data.frame", row.names = c(NA, 
-10L)), vnam = "fBodyBodyGyroMag-meanFreq()")
```
</div>
</div>




---

## fBodyBodyGyroJerkMag-mean()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.88

1st and 3rd quartiles       -0.98; -0.58

Min. and max.                   -1; 0.15
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-48-fBodyBodyGyroJerkMag-mean()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999, 
0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 
2L, 15L, 22L, 18L, 15L, 6L, 5L, 4L, 1L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-12L)), vnam = "fBodyBodyGyroJerkMag-mean()")
```
</div>
</div>




---

## fBodyBodyGyroJerkMag-meanFreq()

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                             0.11

1st and 3rd quartiles        0.05; 0.21

Min. and max.               -0.18; 0.43
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-49-fBodyBodyGyroJerkMag-meanFreq()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-0.2, -0.15, -0.1, -0.05, 
0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4), xmax = c(-0.15, 
-0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45
), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(1L, 
4L, 5L, 8L, 23L, 40L, 25L, 23L, 28L, 12L, 9L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-13L)), vnam = "fBodyBodyGyroJerkMag-meanFreq()")
```
</div>
</div>




---

## acc\_raw-std()-X

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.75

1st and 3rd quartiles       -0.98; -0.2

Min. and max.                  -1; 0.63
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-50-acc-raw-std()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4, 0.6), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 
0L, 8L, 37L, 20L, 16L, 7L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "acc_raw-std()-X")
```
</div>
</div>




---

## acc\_raw-std()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.51

1st and 3rd quartiles       -0.94; -0.03

Min. and max.                -0.99; 0.62
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-51-acc-raw-std()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4, 0.6), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(89L, 
1L, 0L, 10L, 43L, 24L, 10L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "acc_raw-std()-Y")
```
</div>
</div>




---

## acc\_raw-std()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.65

1st and 3rd quartiles       -0.95; -0.23

Min. and max.                -0.99; 0.61
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-52-acc-raw-std()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4, 0.6), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(89L, 
1L, 15L, 42L, 20L, 10L, 1L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "acc_raw-std()-Z")
```
</div>
</div>




---

## acc\_grav\_raw-std()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.97

1st and 3rd quartiles       -0.98; -0.95

Min. and max.                  -1; -0.83
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-53-acc-grav-raw-std()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.98, -0.96, -0.94, 
-0.92, -0.9, -0.88, -0.86, -0.84), xmax = c(-0.98, -0.96, -0.94, 
-0.92, -0.9, -0.88, -0.86, -0.84, -0.82), ymin = c(0, 0, 0, 0, 
0, 0, 0, 0, 0), ymax = c(55L, 52L, 52L, 12L, 4L, 3L, 1L, 0L, 
1L)), class = "data.frame", row.names = c(NA, -9L)), vnam = "acc_grav_raw-std()-X")
```
</div>
</div>




---

## acc\_grav\_raw-std()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.96

1st and 3rd quartiles       -0.97; -0.94

Min. and max.               -0.99; -0.64
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-54-acc-grav-raw-std()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.95, -0.9, -0.85, 
-0.8, -0.75, -0.7, -0.65), xmax = c(-0.95, -0.9, -0.85, -0.8, 
-0.75, -0.7, -0.65, -0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), 
    ymax = c(107L, 72L, 0L, 0L, 0L, 0L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_grav_raw-std()-Y")
```
</div>
</div>




---

## acc\_grav\_raw-std()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.95

1st and 3rd quartiles       -0.96; -0.92

Min. and max.               -0.99; -0.61
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-55-acc-grav-raw-std()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.95, -0.9, -0.85, 
-0.8, -0.75, -0.7, -0.65), xmax = c(-0.95, -0.9, -0.85, -0.8, 
-0.75, -0.7, -0.65, -0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), 
    ymax = c(79L, 76L, 23L, 1L, 0L, 0L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_grav_raw-std()-Z")
```
</div>
</div>




---

## acc\_jerk-std()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.81

1st and 3rd quartiles       -0.98; -0.22

Min. and max.                -0.99; 0.54
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-56-acc-jerk-std()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 
15L, 31L, 26L, 13L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_jerk-std()-X")
```
</div>
</div>




---

## acc\_jerk-std()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.78

1st and 3rd quartiles       -0.97; -0.15

Min. and max.                -0.99; 0.36
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-57-acc-jerk-std()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 1L, 10L, 28L, 
    32L, 12L, 7L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "acc_jerk-std()-Y")
```
</div>
</div>




---

## acc\_jerk-std()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.88

1st and 3rd quartiles       -0.98; -0.51

Min. and max.                -0.99; 0.03
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-58-acc-jerk-std()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0), xmax = c(-0.9, -0.8, 
-0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1), ymin = c(0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 1L, 11L, 16L, 19L, 
18L, 16L, 2L, 3L, 3L, 1L)), class = "data.frame", row.names = c(NA, 
-11L)), vnam = "acc_jerk-std()-Z")
```
</div>
</div>




---

## tBodyGyro-std()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.79

1st and 3rd quartiles       -0.97; -0.44

Min. and max.                -0.99; 0.27
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-59-tBodyGyro-std()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 4L, 51L, 31L, 
    3L, 0L, 1L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "tBodyGyro-std()-X")
```
</div>
</div>




---

## tBodyGyro-std()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                              -0.8

1st and 3rd quartiles       -0.96; -0.42

Min. and max.                -0.99; 0.48
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-60-tBodyGyro-std()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 6L, 
43L, 18L, 12L, 8L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "tBodyGyro-std()-Y")
```
</div>
</div>




---

## tBodyGyro-std()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                              -0.8

1st and 3rd quartiles       -0.96; -0.31

Min. and max.                -0.99; 0.56
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-61-tBodyGyro-std()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 
20L, 42L, 20L, 3L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "tBodyGyro-std()-Z")
```
</div>
</div>




---

## gyro\_jerk-std()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.84

1st and 3rd quartiles       -0.98; -0.46

Min. and max.                   -1; 0.18
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-62-gyro-jerk-std()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999, 
0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 
0L, 3L, 13L, 20L, 18L, 14L, 15L, 4L, 0L, 2L, 1L)), class = "data.frame", row.names = c(NA, 
-12L)), vnam = "gyro_jerk-std()-X")
```
</div>
</div>




---

## gyro\_jerk-std()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.89

1st and 3rd quartiles       -0.98; -0.59

Min. and max.                    -1; 0.3
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-63-gyro-jerk-std()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(97L, 36L, 30L, 10L, 
    5L, 1L, 1L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "gyro_jerk-std()-Y")
```
</div>
</div>




---

## gyro\_jerk-std()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.86

1st and 3rd quartiles       -0.98; -0.47

Min. and max.                   -1; 0.19
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-64-gyro-jerk-std()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999, 
0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 
0L, 4L, 14L, 18L, 20L, 17L, 8L, 4L, 1L, 2L, 2L)), class = "data.frame", row.names = c(NA, 
-12L)), vnam = "gyro_jerk-std()-Z")
```
</div>
</div>




---

## acc\_magnitude-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.61

1st and 3rd quartiles       -0.94; -0.21

Min. and max.                -0.99; 0.43
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-65-acc-magnitude-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(88L, 2L, 
12L, 34L, 21L, 13L, 8L, 2L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_magnitude-std()")
```
</div>
</div>




---

## acc\_gravity\_magnitude-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.61

1st and 3rd quartiles       -0.94; -0.21

Min. and max.                -0.99; 0.43
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-66-acc-gravity-magnitude-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(88L, 2L, 
12L, 34L, 21L, 13L, 8L, 2L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_gravity_magnitude-std()")
```
</div>
</div>




---

## acc\_jerk\_magnitude-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                              -0.8

1st and 3rd quartiles       -0.98; -0.22

Min. and max.                -0.99; 0.45
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-67-acc-jerk-magnitude-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 1L, 
20L, 26L, 26L, 12L, 4L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "acc_jerk_magnitude-std()")
```
</div>
</div>




---

## gyro\_magnitude-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.74

1st and 3rd quartiles       -0.95; -0.36

Min. and max.                 -0.98; 0.3
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-68-gyro-magnitude-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 32L, 38L, 
    14L, 2L, 2L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "gyro_magnitude-std()")
```
</div>
</div>




---

## gyro\_jerk\_magnitude-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.88

1st and 3rd quartiles       -0.98; -0.58

Min. and max.                   -1; 0.25
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-69-gyro-jerk-magnitude-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 
0.3), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 
3L, 15L, 20L, 21L, 14L, 5L, 5L, 3L, 2L, 1L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-13L)), vnam = "gyro_jerk_magnitude-std()")
```
</div>
</div>




---

## fourier\_acc\_raw-std()-X

<div class = "row">
<div class = "col-lg-8">

---------------------------------------
Feature                          Result
------------------------- -------------
Variable type                   numeric

Number of missing obs.          0 (0 %)

Number of unique values             180

Median                            -0.75

1st and 3rd quartiles       -0.98; -0.2

Min. and max.                  -1; 0.66
---------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-70-fourier-acc-raw-std()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4, 0.6), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 
0L, 7L, 37L, 22L, 13L, 7L, 3L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "fourier_acc_raw-std()-X")
```
</div>
</div>




---

## fourier\_acc\_raw-std()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.51

1st and 3rd quartiles       -0.94; -0.08

Min. and max.                -0.99; 0.56
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-71-fourier-acc-raw-std()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(89L, 1L, 
0L, 14L, 41L, 24L, 8L, 3L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_acc_raw-std()-Y")
```
</div>
</div>




---

## fourier\_acc\_raw-std()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.64

1st and 3rd quartiles       -0.95; -0.27

Min. and max.                -0.99; 0.69
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-72-fourier-acc-raw-std()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4, 0.6), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(89L, 
1L, 19L, 40L, 19L, 9L, 1L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "fourier_acc_raw-std()-Z")
```
</div>
</div>




---

## fourier\_acc\_jerk-std()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.83

1st and 3rd quartiles       -0.98; -0.25

Min. and max.                   -1; 0.48
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-73-fourier-acc-jerk-std()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 
21L, 30L, 25L, 11L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_acc_jerk-std()-X")
```
</div>
</div>




---

## fourier\_acc\_jerk-std()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.79

1st and 3rd quartiles       -0.97; -0.17

Min. and max.                -0.99; 0.35
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-74-fourier-acc-jerk-std()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 11L, 28L, 
    33L, 9L, 7L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "fourier_acc_jerk-std()-Y")
```
</div>
</div>




---

## fourier\_acc\_jerk-std()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                              -0.9

1st and 3rd quartiles       -0.98; -0.54

Min. and max.               -0.99; -0.01
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-75-fourier-acc-jerk-std()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1), xmax = c(-0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0), ymin = c(0, 0, 0, 0, 
0, 0, 0, 0, 0, 0), ymax = c(90L, 2L, 13L, 20L, 22L, 15L, 10L, 
3L, 1L, 4L)), class = "data.frame", row.names = c(NA, -10L)), 
    vnam = "fourier_acc_jerk-std()-Z")
```
</div>
</div>




---

## fourier\_gyro\_raw-std()-X

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.81

1st and 3rd quartiles       -0.98; -0.48

Min. and max.                 -0.99; 0.2
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-76-fourier-gyro-raw-std()-X', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999, 
0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(85L, 
5L, 1L, 10L, 28L, 21L, 22L, 6L, 1L, 0L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-12L)), vnam = "fourier_gyro_raw-std()-X")
```
</div>
</div>




---

## fourier\_gyro\_raw-std()-Y

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                              -0.8

1st and 3rd quartiles       -0.96; -0.42

Min. and max.                -0.99; 0.65
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-77-fourier-gyro-raw-std()-Y', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4, 0.6), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 
0.2, 0.4, 0.6, 0.8), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 
4L, 42L, 21L, 12L, 8L, 2L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-9L)), vnam = "fourier_gyro_raw-std()-Y")
```
</div>
</div>




---

## fourier\_gyro\_raw-std()-Z

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.82

1st and 3rd quartiles       -0.96; -0.39

Min. and max.                -0.99; 0.52
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-78-fourier-gyro-raw-std()-Z', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2, 0.4), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 
0.4, 0.6), ymin = c(0, 0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 5L, 
37L, 36L, 6L, 4L, 1L, 1L)), class = "data.frame", row.names = c(NA, 
-8L)), vnam = "fourier_gyro_raw-std()-Z")
```
</div>
</div>




---

## fourier\_acc\_magnitude-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.65

1st and 3rd quartiles       -0.95; -0.37

Min. and max.                -0.99; 0.18
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-79-fourier-acc-magnitude-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.0999999999999999, 
0.2), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(82L, 
6L, 1L, 3L, 13L, 20L, 20L, 8L, 8L, 9L, 7L, 3L)), class = "data.frame", row.names = c(NA, 
-12L)), vnam = "fourier_acc_magnitude-std()")
```
</div>
</div>




---

## fBodyBodyAccJerkMag-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.81

1st and 3rd quartiles       -0.98; -0.27

Min. and max.                -0.99; 0.32
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-80-fBodyBodyAccJerkMag-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(90L, 1L, 25L, 22L, 
    27L, 12L, 3L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "fBodyBodyAccJerkMag-std()")
```
</div>
</div>




---

## fBodyBodyGyroMag-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.77

1st and 3rd quartiles       -0.95; -0.43

Min. and max.                -0.98; 0.24
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-81-fBodyBodyGyroMag-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.9, -0.8, -0.7, 
-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2), xmax = c(-0.9, 
-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 
0.3), ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ymax = c(80L, 
10L, 1L, 6L, 18L, 24L, 23L, 7L, 7L, 2L, 1L, 0L, 1L)), class = "data.frame", row.names = c(NA, 
-13L)), vnam = "fBodyBodyGyroMag-std()")
```
</div>
</div>




---

## fBodyBodyGyroJerkMag-std()

<div class = "row">
<div class = "col-lg-8">

----------------------------------------
Feature                           Result
------------------------- --------------
Variable type                    numeric

Number of missing obs.           0 (0 %)

Number of unique values              180

Median                             -0.89

1st and 3rd quartiles       -0.98; -0.61

Min. and max.                   -1; 0.29
----------------------------------------


</div>
<div class = "col-lg-4">
```{r 'Var-82-fBodyBodyGyroJerkMag-std()', echo=FALSE, fig.width=4, fig.height=3, message=FALSE, warning=FALSE}
ggAggHist(data = structure(list(xmin = c(-1, -0.8, -0.6, -0.4, 
-0.2, 0, 0.2), xmax = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4), 
    ymin = c(0, 0, 0, 0, 0, 0, 0), ymax = c(94L, 41L, 29L, 10L, 
    5L, 0L, 1L)), class = "data.frame", row.names = c(NA, -7L
)), vnam = "fBodyBodyGyroJerkMag-std()")
```
</div>
</div>




---





