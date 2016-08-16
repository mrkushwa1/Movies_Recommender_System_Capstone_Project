# Movies_Recommender_System: Capstone Project
Manu Kushwaha  
August 11, 2016  
***
### OBJECTIVE/SYNOPSIS

The objective of this project is create a movies recommender system which utilizes the collaborative filtering methodology to recommend atlest top 3-5 movies to a target user. The data was downloaded from the MoviesLens site: <http://grouplens.org/datasets/movielens>. The datasets are provided by the GroupLens Research Group. (Please refer to the "READ.ME" file included for endorsements)

The dataset that was chosen is from the "MovieLens 1M Dataset" section and the compressed file name was: "ml-1m.zip".

The structure of the Movie data is such that it does not provide any Movie content information, i.e. there is no information about the directors, actors, producers, etc. Hence, content based collaborative filtering methodology cannot be used here. Ideally, to make a sensible collaborative filtering model, both user and content based filtering methods need to be employed in tandem.

Based on this limitation, the approach is then to utlize the user based collaborative filtering method, which finds similarity between ratings of multiple users who are the closest to the target user, and then provide movies' recommendations to this user which they have not yet rated or seen.

***
### ABOUT THE FILES IN THE "MOVIELENS 1M DATASET""

There are 3 data files included in the compressed zip file and these files contain 1,000,209 anonymous ratings of approximately 3,900 movies 
made by 6,040 MovieLens users who joined MovieLens in 2000.

The description and content of these 3 files is given below:

***
#### 1. RATINGS FILE DESCRIPTION

All ratings are contained in the file "ratings.dat" and are in the
following format:

UserID::MovieID::Rating::Timestamp

- UserIDs range between 1 and 6040 
- MovieIDs range between 1 and 3952
- Ratings are made on a 5-star scale (whole-star ratings only)
- Timestamp is represented in seconds since the epoch as returned by time(2)
- Each user has at least 20 ratings

***
#### 2. USERS FILE DESCRIPTION

User information is in the file "users.dat" and is in the following
format:

UserID::Gender::Age::Occupation::Zip-code

All demographic information is provided voluntarily by the users and is
not checked for accuracy.  Only users who have provided some demographic
information are included in this data set.

- Gender is denoted by a "M" for male and "F" for female
- Age is chosen from the following ranges:

	*  1:  "Under 18"
	* 18:  "18-24"
	* 25:  "25-34"
	* 35:  "35-44"
	* 45:  "45-49"
	* 50:  "50-55"
	* 56:  "56+"

- Occupation is chosen from the following choices:

	*  0:  "other" or not specified
	*  1:  "academic/educator"
	*  2:  "artist"
	*  3:  "clerical/admin"
	*  4:  "college/grad student"
	*  5:  "customer service"
	*  6:  "doctor/health care"
	*  7:  "executive/managerial"
	*  8:  "farmer"
	*  9:  "homemaker"
	* 10:  "K-12 student"
	* 11:  "lawyer"
	* 12:  "programmer"
	* 13:  "retired"
	* 14:  "sales/marketing"
	* 15:  "scientist"
	* 16:  "self-employed"
	* 17:  "technician/engineer"
	* 18:  "tradesman/craftsman"
	* 19:  "unemployed"
	* 20:  "writer"

***
#### 3. MOVIES FILE DESCRIPTION

Movie information is in the file "movies.dat" and is in the following
format:

MovieID::Title::Genres

- Titles are identical to titles provided by the IMDB (including
year of release)
- Genres are pipe-separated and are selected from the following genres:

	* Action
	* Adventure
	* Animation
	* Children's
	* Comedy
	* Crime
	* Documentary
	* Drama
	* Fantasy
	* Film-Noir
	* Horror
	* Musical
	* Mystery
	* Romance
	* Sci-Fi
	* Thriller
	* War
	* Western

***
### INITAL OBSERVATIONS ABOUT THE DATA

The format of the data is already provided above and there is, clearly, a need to wrangle it to bring it into a workable form. To confirm whether R sees the same format as above, all the 3 files are read into their respective dataframes. 


```r
# Loading the required libraries
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(reshape2))
suppressMessages(library(splitstackshape))
suppressMessages(library(scales))
```


```r
## Creating a utility function to perform basic checks on the dataframes
check_df <- function(y, ...){
  
  if(... == 0){
    df_records_count <- sapply(y, function(x) sum(length(x)))
    print("Checking for the total number of records in the dataframe to ensure completeness:")
    cat("\n")
    print(df_records_count)
    cat("\n\n")
    
    df_missing_data <- sapply(y, function(x) sum(length(which(is.na(x)))))
    print("Checking for the total number of missing values (NA) in the dataframe if any:")
    cat("\n")
    print(df_missing_data)
  }
  
  if(... == 1){
    df_records_count <- sapply(y, function(x) sum(length(x)))
    print("Checking for the total number of records in the dataframe to ensure completeness:")
    cat("\n")
    print(df_records_count)
    cat("\n\n")
  }
  
  if(... == 2){
    df_missing_data <- sapply(y, function(x) sum(length(which(is.na(x)))))
    print("Checking for the total number of missing values (NA) in the dataframe if any:")
    cat("\n")
    print(df_missing_data)
  }

}
```

***
Reading in the data from the 3 files.


```r
# reading the movies.dat file
mov_lines <- readLines("movies.dat")
head(mov_lines)
```

```
## [1] "1::Toy Story (1995)::Animation|Children's|Comedy"
## [2] "2::Jumanji (1995)::Adventure|Children's|Fantasy" 
## [3] "3::Grumpier Old Men (1995)::Comedy|Romance"      
## [4] "4::Waiting to Exhale (1995)::Comedy|Drama"       
## [5] "5::Father of the Bride Part II (1995)::Comedy"   
## [6] "6::Heat (1995)::Action|Crime|Thriller"
```

##### Observations:

  * There is no header information to explain the varibales in the dataset
  * The variables here are seperated by "::"
  * Movie release year is a part of the movie title
  * Generes are seperated using a pipe operator ("|")

***

```r
# reading the users.dat file
usrs_lines <- readLines("users.dat")
head(usrs_lines)
```

```
## [1] "1::F::1::10::48067"  "2::M::56::16::70072" "3::M::25::15::55117"
## [4] "4::M::45::7::02460"  "5::M::25::20::55455" "6::F::50::9::55117"
```

##### Observations:

  * There is again no header information to explain the varibales in the dataset
  * The variables here are also seperated by "::"
  * Age and Occupation appear as IDs rather than in their meaningful form 

***

```r
# reading the ratings.dat file
ratings_lines <- readLines("ratings.dat")
head(ratings_lines)
```

```
## [1] "1::1193::5::978300760" "1::661::3::978302109"  "1::914::3::978301968" 
## [4] "1::3408::4::978300275" "1::2355::5::978824291" "1::1197::3::978302268"
```

##### Observations:

  * There is once again no header information to explain the varibales in the dataset
  * The variables here are again seperated by "::"
  * The timestamp needs to be converted into a meaningful date format

***
### DATA WRANGLING APPROACH

Clearly, the data needs to be formatted properly to bring it into a workable form. The idea here is to combine the 3 datasets to create one complete movies dataframe which contains all the information provided in these seperate files.

In order to do so, the following approach is undertaken:

For the movies (mov_lines) dataset:

  1. The "::" seperator needs to be removed
  2. Header information needs to be input to give meaningful names to the variables (data columns)
  3. The movie release year needs to be seperated from the movie title variable and a new ReleaseYear variable needs to be created
  4. Similarly, the Generes need to be seperated and a new Genere variable needs to be created

Hence, implementing the above 4 points...

Addressing point 1 above:

```r
# Transforming the mov_lines into a Matrix and then into a mov_df dataframe
Matrix <- do.call(rbind, strsplit(mov_lines,"::", fixed=TRUE))
mov_df <- as.data.frame(Matrix)
```

Addressing point 2 above:

```r
# From the readme file included, assigning the relevant column/variable names
names(mov_df) <- c("MovieID", "MovieTitle","Genere")

# Checking the first few records of the mov_df dataframe
head(mov_df)
```

```
##   MovieID                         MovieTitle                       Genere
## 1       1                   Toy Story (1995)  Animation|Children's|Comedy
## 2       2                     Jumanji (1995) Adventure|Children's|Fantasy
## 3       3            Grumpier Old Men (1995)               Comedy|Romance
## 4       4           Waiting to Exhale (1995)                 Comedy|Drama
## 5       5 Father of the Bride Part II (1995)                       Comedy
## 6       6                        Heat (1995)        Action|Crime|Thriller
```

```r
# Running the utility function to check the mov_df dataframe
check_df(mov_df,0)
```

```
## [1] "Checking for the total number of records in the dataframe to ensure completeness:"
## 
##    MovieID MovieTitle     Genere 
##       3883       3883       3883 
## 
## 
## [1] "Checking for the total number of missing values (NA) in the dataframe if any:"
## 
##    MovieID MovieTitle     Genere 
##          0          0          0
```

```r
# checking the structure of the mov_df
str(mov_df)
```

```
## 'data.frame':	3883 obs. of  3 variables:
##  $ MovieID   : Factor w/ 3883 levels "1","10","100",..: 1 1056 2166 3228 3339 3450 3556 3666 3775 2 ...
##  $ MovieTitle: Factor w/ 3883 levels "'burbs, The (1989)",..: 3576 1860 1485 3718 1179 1564 3012 3551 3364 1426 ...
##  $ Genere    : Factor w/ 301 levels "Action","Action|Adventure",..: 146 116 208 186 177 64 208 107 1 38 ...
```

Since all variables are factors, the MovieID will beed to be turned into numeric type and MovieTitle into Character such that string manipulations can be done.


```r
# converting MovieTitle to character vector
mov_df$MovieTitle <- as.character(mov_df$MovieTitle)

# converting MovieID to numeric vector
mov_df$MovieID <- as.numeric(mov_df$MovieID)
```

Addressing point 3 above:

```r
# Extracting the release year portion from the MovieTitle variable 
mov_year <- extract_numeric(substr(mov_df$MovieTitle, nchar(mov_df$MovieTitle)-5, nchar(mov_df$MovieTitle)))
mov_title <- substr(mov_df$MovieTitle, 1, nchar(mov_df$MovieTitle)-7)

# Reassigning the data back to the MovieTitle variable without the year
mov_df$MovieTitle <- mov_title

# Creating a Release year column in the mov_df
mov_df$ReleaseYear <- mov_year

# Checking the first few records of the new movive dataframe
head(mov_df)
```

```
##   MovieID                  MovieTitle                       Genere
## 1       1                   Toy Story  Animation|Children's|Comedy
## 2    1056                     Jumanji Adventure|Children's|Fantasy
## 3    2166            Grumpier Old Men               Comedy|Romance
## 4    3228           Waiting to Exhale                 Comedy|Drama
## 5    3339 Father of the Bride Part II                       Comedy
## 6    3450                        Heat        Action|Crime|Thriller
##   ReleaseYear
## 1        1995
## 2        1995
## 3        1995
## 4        1995
## 5        1995
## 6        1995
```

Addressing Point 4 above:

```r
# split the Genere column to corresponding Generes which are separated by "|"
mov_df <- cSplit(mov_df, "Genere", sep="|")

# checking the first few observatins
head(mov_df)
```

```
##    MovieID                  MovieTitle ReleaseYear  Genere_1   Genere_2
## 1:       1                   Toy Story        1995 Animation Children's
## 2:    1056                     Jumanji        1995 Adventure Children's
## 3:    2166            Grumpier Old Men        1995    Comedy    Romance
## 4:    3228           Waiting to Exhale        1995    Comedy      Drama
## 5:    3339 Father of the Bride Part II        1995    Comedy         NA
## 6:    3450                        Heat        1995    Action      Crime
##    Genere_3 Genere_4 Genere_5 Genere_6
## 1:   Comedy       NA       NA       NA
## 2:  Fantasy       NA       NA       NA
## 3:       NA       NA       NA       NA
## 4:       NA       NA       NA       NA
## 5:       NA       NA       NA       NA
## 6: Thriller       NA       NA       NA
```

```r
#sapply(mov_df, class)
#sapply(mov_df, function(y) sum(length(which(is.na(y)))))
check_df(mov_df,0)
```

```
## [1] "Checking for the total number of records in the dataframe to ensure completeness:"
## 
##     MovieID  MovieTitle ReleaseYear    Genere_1    Genere_2    Genere_3 
##        3883        3883        3883        3883        3883        3883 
##    Genere_4    Genere_5    Genere_6 
##        3883        3883        3883 
## 
## 
## [1] "Checking for the total number of missing values (NA) in the dataframe if any:"
## 
##     MovieID  MovieTitle ReleaseYear    Genere_1    Genere_2    Genere_3 
##           0           0           0           0        2025        3347 
##    Genere_4    Genere_5    Genere_6 
##        3768        3868        3882
```

From the above 2 outputs, it can be seen that although Generes have been split into 6 different categories, they are not unique. Hence, to fix this:

```r
mdata <- melt(mov_df, id=c("MovieID","MovieTitle","ReleaseYear"))

# Using the utility function to check the mdata dataframe
check_df(mdata,0)
```

```
## [1] "Checking for the total number of records in the dataframe to ensure completeness:"
## 
##     MovieID  MovieTitle ReleaseYear    variable       value 
##       23298       23298       23298       23298       23298 
## 
## 
## [1] "Checking for the total number of missing values (NA) in the dataframe if any:"
## 
##     MovieID  MovieTitle ReleaseYear    variable       value 
##           0           0           0           0       16890
```

The total number of observations is now 23298 which is: 3883 movies * 6 Generes. This means that there are movies in the dataframe that are repeated and which do not fit into a particular Genere type. Hence, it is safe to remove these records.


```r
# removing records where value is NA which indicates that a movie does not have a genere specified
mdata <- mdata[!is.na(mdata$value),]
#sapply(mdata, function(y) sum(length(which(is.na(y)))))
check_df(mdata, 0)
```

```
## [1] "Checking for the total number of records in the dataframe to ensure completeness:"
## 
##     MovieID  MovieTitle ReleaseYear    variable       value 
##        6408        6408        6408        6408        6408 
## 
## 
## [1] "Checking for the total number of missing values (NA) in the dataframe if any:"
## 
##     MovieID  MovieTitle ReleaseYear    variable       value 
##           0           0           0           0           0
```

Another observation based on the above output indicates that there are now 6408 records where a movie may be repeated across other Generes. Now creating a binary classification for the Generes:

```r
# Createing a 1's column in the mov_df dataframe where 1 indicates that the movie has been classified into certain Genere
mdata$Type <- 1

# Widening the mov_df dataframe to have Generes as variables
mov_df <- dcast(mdata, MovieID + MovieTitle + ReleaseYear ~ value, value.var="Type")

check_df(mov_df,0)
```

```
## [1] "Checking for the total number of records in the dataframe to ensure completeness:"
## 
##     MovieID  MovieTitle ReleaseYear      Action   Adventure   Animation 
##        3883        3883        3883        3883        3883        3883 
##  Children's      Comedy       Crime Documentary       Drama     Fantasy 
##        3883        3883        3883        3883        3883        3883 
##   Film-Noir      Horror     Musical     Mystery     Romance      Sci-Fi 
##        3883        3883        3883        3883        3883        3883 
##    Thriller         War     Western 
##        3883        3883        3883 
## 
## 
## [1] "Checking for the total number of missing values (NA) in the dataframe if any:"
## 
##     MovieID  MovieTitle ReleaseYear      Action   Adventure   Animation 
##           0           0           0        3380        3600        3778 
##  Children's      Comedy       Crime Documentary       Drama     Fantasy 
##        3632        2683        3672        3756        2280        3815 
##   Film-Noir      Horror     Musical     Mystery     Romance      Sci-Fi 
##        3839        3540        3769        3777        3412        3607 
##    Thriller         War     Western 
##        3391        3740        3815
```

```r
# replacing all NA values with 0
mov_df[is.na(mov_df)] <- 0

# Again checking the first few records of the mov_df
head(mov_df)
```

```
##    MovieID                MovieTitle ReleaseYear Action Adventure
## 1:       1                 Toy Story        1995      0         0
## 2:       2                 GoldenEye        1995      1         1
## 3:       3                 City Hall        1996      0         0
## 4:       4                   Curdled        1996      0         0
## 5:       5 Associate, The (L'Associe        1982      0         0
## 6:       6            Ed's Next Move        1996      0         0
##    Animation Children's Comedy Crime Documentary Drama Fantasy Film-Noir
## 1:         1          1      1     0           0     0       0         0
## 2:         0          0      0     0           0     0       0         0
## 3:         0          0      0     0           0     1       0         0
## 4:         0          0      0     1           0     0       0         0
## 5:         0          0      1     0           0     0       0         0
## 6:         0          0      1     0           0     0       0         0
##    Horror Musical Mystery Romance Sci-Fi Thriller War Western
## 1:      0       0       0       0      0        0   0       0
## 2:      0       0       0       0      0        1   0       0
## 3:      0       0       0       0      0        1   0       0
## 4:      0       0       0       0      0        0   0       0
## 5:      0       0       0       0      0        0   0       0
## 6:      0       0       0       0      0        0   0       0
```

```r
# Checking the structure of the mov_df dataframe
str(mov_df)
```

```
## Classes 'data.table' and 'data.frame':	3883 obs. of  21 variables:
##  $ MovieID    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ MovieTitle : chr  "Toy Story" "GoldenEye" "City Hall" "Curdled" ...
##  $ ReleaseYear: num  1995 1995 1996 1996 1982 ...
##  $ Action     : num  0 1 0 0 0 0 0 1 0 0 ...
##  $ Adventure  : num  0 1 0 0 0 0 0 0 0 0 ...
##  $ Animation  : num  1 0 0 0 0 0 0 0 0 0 ...
##  $ Children's : num  1 0 0 0 0 0 0 0 1 0 ...
##  $ Comedy     : num  1 0 0 0 1 1 0 0 1 0 ...
##  $ Crime      : num  0 0 0 1 0 0 0 0 0 0 ...
##  $ Documentary: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Drama      : num  0 0 1 0 0 0 1 0 0 1 ...
##  $ Fantasy    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Film-Noir  : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Horror     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Musical    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Mystery    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Romance    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Sci-Fi     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Thriller   : num  0 1 1 0 0 0 1 1 0 0 ...
##  $ War        : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Western    : num  0 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, ".internal.selfref")=<externalptr> 
##  - attr(*, "sorted")= chr  "MovieID" "MovieTitle" "ReleaseYear"
```

```r
#sapply(mov_df, class)

# check to see if any punctuation characters are present in the dataframe
sapply(mov_df, function(y) sum(length(grep("[^[:alnum:]]", y))))
```

```
##     MovieID  MovieTitle ReleaseYear      Action   Adventure   Animation 
##           0        3228           0           0           0           0 
##  Children's      Comedy       Crime Documentary       Drama     Fantasy 
##           0           0           0           0           0           0 
##   Film-Noir      Horror     Musical     Mystery     Romance      Sci-Fi 
##           0           0           0           0           0           0 
##    Thriller         War     Western 
##           0           0           0
```

As seen here, the Generes are now all numeric in nature rather than factors as originally obtained. They will need to be converted into factors once the full dataframe is created. Also, as expected, the movie title variable contains punctuation characters which is fine.

***

For the users (usrs_lines) dataset:

  1. The "::" seperator needs to be removed
  2. Header information needs to be input to give meaningful names to the variables (data columns)
  3. Age and Occupation appear as IDs rather than in their meaningful form. Therefore, seperate Age and Occupation dataframes need to be created, containing both AgeID and AgeRange, and, OccupationID and Occupation, and then joined with the users dataframe

Hence, implementing the above 3 points...

Addressing point 1 above:

```r
# Transforming the usrs_lines into a Matrix and then into a usrs_df dataframe
Matrix <- do.call(rbind, strsplit(usrs_lines,"::", fixed=TRUE))
usrs_df <- data.frame(Matrix, stringsAsFactors = FALSE)
```

Addressing point 2 above:

```r
# From the readme file included, assigning the relevant column/variable names
names(usrs_df) <- c("UserID", "Gender", "Age", "OccupationID", "ZipCode")

# checking the first few records of the usrs_df
head(usrs_df)
```

```
##   UserID Gender Age OccupationID ZipCode
## 1      1      F   1           10   48067
## 2      2      M  56           16   70072
## 3      3      M  25           15   55117
## 4      4      M  45            7   02460
## 5      5      M  25           20   55455
## 6      6      F  50            9   55117
```

```r
# checking the structure of the usrs_df
str(usrs_df)
```

```
## 'data.frame':	6040 obs. of  5 variables:
##  $ UserID      : chr  "1" "2" "3" "4" ...
##  $ Gender      : chr  "F" "M" "M" "M" ...
##  $ Age         : chr  "1" "56" "25" "45" ...
##  $ OccupationID: chr  "10" "16" "15" "7" ...
##  $ ZipCode     : chr  "48067" "70072" "55117" "02460" ...
```


```r
# check for special chars in the entire usr_df dataframe, the result will indicate which columns have issues
sapply(usrs_df, function(y) sum(length(grep("[^[:alnum:]]", y))))
```

```
##       UserID       Gender          Age OccupationID      ZipCode 
##            0            0            0            0           66
```

```r
# since Zipcode column is showing 66 non-alphanumeric values, we need to see what these are
head(grep("[^[:alnum:]]", usrs_df$ZipCode, value = TRUE))
```

```
## [1] "98107-2117" "37919-4204" "55337-4056" "55405-2546" "55103-1006"
## [6] "52570-9634"
```

```r
# since all US Zipcodes are 5 digits, selecting only the LHS values before the "-" (which are 5 digits)
# and replacing them back in the same place
usrs_df$ZipCode <- sub("(\\d{5}).*", "\\1", usrs_df$ZipCode)
```


```r
# changing all columns but Gender to numeric
usrs_df[, colnames(usrs_df) != "Gender"] <- lapply(usrs_df[, colnames(usrs_df) != "Gender"], as.numeric)

# checking the structure of the usrs_df
str(usrs_df)
```

```
## 'data.frame':	6040 obs. of  5 variables:
##  $ UserID      : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Gender      : chr  "F" "M" "M" "M" ...
##  $ Age         : num  1 56 25 45 25 50 35 25 25 35 ...
##  $ OccupationID: num  10 16 15 7 20 9 1 12 17 1 ...
##  $ ZipCode     : num  48067 70072 55117 2460 55455 ...
```

```r
# checking to see if there is any variation in how the Gender is entered
unique(usrs_df$Gender)
```

```
## [1] "F" "M"
```

Addressing point 3 above:

  * For this one, 2 new dat files (one containing the age information and the other with occupation data) were manually created. Their format is given below:
  

```r
# creating the ages ref dataframe
age_df <- read.csv("ages.dat", sep = ":", header = FALSE, stringsAsFactors = FALSE, colClasses = c("numeric","character"))

# assigning correct header names from the read_me file
names(age_df) <- c("Age", "AgeRange")

# checking the first few records of the age_df
head(age_df)
```

```
##   Age   AgeRange
## 1   1   Under 18
## 2  18      18-24
## 3  25      25-34
## 4  35      35-44
## 5  45      45-49
## 6  50      50-55
```

```r
# checking the structure of the age_df
str(age_df)
```

```
## 'data.frame':	7 obs. of  2 variables:
##  $ Age     : num  1 18 25 35 45 50 56
##  $ AgeRange: chr  "  Under 18" "  18-24" "  25-34" "  35-44" ...
```


```r
# creating the occupation ref dataframe
occup_df <- read.csv("occupation.dat", sep = ":", header = FALSE, stringsAsFactors = FALSE, colClasses = c("numeric","character"))

# assigning correct header names from the read_me file
names(occup_df) <- c("OccupationID", "Occupation")

# checking the first few records of the occup_df
head(occup_df)
```

```
##   OccupationID               Occupation
## 1            0   other or not specified
## 2            1        academic/educator
## 3            2                   artist
## 4            3           clerical/admin
## 5            4     college/grad student
## 6            5         customer service
```

```r
# checking the structure of the occup_df
str(occup_df)
```

```
## 'data.frame':	21 obs. of  2 variables:
##  $ OccupationID: num  0 1 2 3 4 5 6 7 8 9 ...
##  $ Occupation  : chr  "  other or not specified" "  academic/educator" "  artist" "  clerical/admin" ...
```

Now joining the age_df and the occup_df dataframes with the usrs_df dataframe:


```r
# joining the age_df to the usrs_df by Age
usrs_df <- left_join(usrs_df, age_df, by = "Age")

# joining the occup_df to the usrs_df by OccupationID
usrs_df <- left_join(usrs_df, occup_df, by = "OccupationID")

# checking the structure of the usrs_df
str(usrs_df)
```

```
## 'data.frame':	6040 obs. of  7 variables:
##  $ UserID      : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ Gender      : chr  "F" "M" "M" "M" ...
##  $ Age         : num  1 56 25 45 25 50 35 25 25 35 ...
##  $ OccupationID: num  10 16 15 7 20 9 1 12 17 1 ...
##  $ ZipCode     : num  48067 70072 55117 2460 55455 ...
##  $ AgeRange    : chr  "  Under 18" "  56+" "  25-34" "  45-49" ...
##  $ Occupation  : chr  "  K-12 student" "  self-employed" "  scientist" "  executive/managerial" ...
```

```r
# reordering columns
usrs_df <- usrs_df[, c("UserID", "Age", "AgeRange", "Gender", "OccupationID", "Occupation", "ZipCode")]

# checking the first few records of the usrs_df
head(usrs_df)
```

```
##   UserID Age   AgeRange Gender OccupationID             Occupation ZipCode
## 1      1   1   Under 18      F           10           K-12 student   48067
## 2      2  56        56+      M           16          self-employed   70072
## 3      3  25      25-34      M           15              scientist   55117
## 4      4  45      45-49      M            7   executive/managerial    2460
## 5      5  25      25-34      M           20                 writer   55455
## 6      6  50      50-55      F            9              homemaker   55117
```

The usrs_df is now complete with all the correct variables in place.

***

For the ratings (ratings_lines) dataset:

  1. The "::" seperator needs to be removed
  2. Header information needs to be input to give meaningful names to the variables (data columns)
  3. The timestamp needs to be converted into a meaningful date and time format

Hence, implementing the above 3 points...

Addressing point 1 above:

```r
# reading the ratings.dat file
Matrix <- do.call(rbind, strsplit(ratings_lines,"::", fixed=TRUE))
ratings_df <- data.frame(Matrix, stringsAsFactors = FALSE)
```

Addressing point 2 above:

```r
# assigning correct header names from the read_me file
names(ratings_df) <- c("UserID", "MovieID", "Rating", "TimeStamp")

# checking the structure of the ratings_df dataframe
str(ratings_df)
```

```
## 'data.frame':	1000209 obs. of  4 variables:
##  $ UserID   : chr  "1" "1" "1" "1" ...
##  $ MovieID  : chr  "1193" "661" "914" "3408" ...
##  $ Rating   : chr  "5" "3" "3" "4" ...
##  $ TimeStamp: chr  "978300760" "978302109" "978301968" "978300275" ...
```

```r
check_df(ratings_df, 0)
```

```
## [1] "Checking for the total number of records in the dataframe to ensure completeness:"
## 
##    UserID   MovieID    Rating TimeStamp 
##   1000209   1000209   1000209   1000209 
## 
## 
## [1] "Checking for the total number of missing values (NA) in the dataframe if any:"
## 
##    UserID   MovieID    Rating TimeStamp 
##         0         0         0         0
```

Addressing point 3 above:

```r
# Changing all columns to numeric data type so that date and time operations can be performed on the timestamp
ratings_df[, colnames(ratings_df)] <- lapply(ratings_df[, colnames(ratings_df)], as.numeric)

# Now epoch or UNIX time is given in the TimeStamp - so extracting the date and time from it
ratings_df$Date <- strftime(as.POSIXct(ratings_df$TimeStamp, origin = "1970-01-01", tz =""),"%Y-%m-%d")
ratings_df$Time <- strftime(as.POSIXct(ratings_df$TimeStamp, origin = "1970-01-01", tz =""),"%H:%M:%S")

# checking the first few records of the ratings_df
head(ratings_df)
```

```
##   UserID MovieID Rating TimeStamp       Date     Time
## 1      1    1193      5 978300760 2000-12-31 17:12:40
## 2      1     661      3 978302109 2000-12-31 17:35:09
## 3      1     914      3 978301968 2000-12-31 17:32:48
## 4      1    3408      4 978300275 2000-12-31 17:04:35
## 5      1    2355      5 978824291 2001-01-06 18:38:11
## 6      1    1197      3 978302268 2000-12-31 17:37:48
```

```r
# checking the structure of the ratings_ds
str(ratings_df)
```

```
## 'data.frame':	1000209 obs. of  6 variables:
##  $ UserID   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ MovieID  : num  1193 661 914 3408 2355 ...
##  $ Rating   : num  5 3 3 4 5 3 5 5 4 4 ...
##  $ TimeStamp: num  9.78e+08 9.78e+08 9.78e+08 9.78e+08 9.79e+08 ...
##  $ Date     : chr  "2000-12-31" "2000-12-31" "2000-12-31" "2000-12-31" ...
##  $ Time     : chr  "17:12:40" "17:35:09" "17:32:48" "17:04:35" ...
```

***

To create the full movies dataframe that contains all the users, their ratings and movies information:

  1. The usrs_df and mov_df dataframes will be joined with the ratings_df



```r
# joining the usrs_df with the ratings_df by UserID column
full_mov_df <- left_join(ratings_df, usrs_df, by = "UserID")

# joining the mov_df with the full_mov_df created in the above step by MovieID column
full_mov_df <- left_join(full_mov_df, mov_df, by = "MovieID")

# checking the first few records of the full_mov_df
head(full_mov_df)
```

```
##   UserID MovieID Rating TimeStamp       Date     Time Age   AgeRange
## 1      1    1193      5 978300760 2000-12-31 17:12:40   1   Under 18
## 2      1     661      3 978302109 2000-12-31 17:35:09   1   Under 18
## 3      1     914      3 978301968 2000-12-31 17:32:48   1   Under 18
## 4      1    3408      4 978300275 2000-12-31 17:04:35   1   Under 18
## 5      1    2355      5 978824291 2001-01-06 18:38:11   1   Under 18
## 6      1    1197      3 978302268 2000-12-31 17:37:48   1   Under 18
##   Gender OccupationID     Occupation ZipCode
## 1      F           10   K-12 student   48067
## 2      F           10   K-12 student   48067
## 3      F           10   K-12 student   48067
## 4      F           10   K-12 student   48067
## 5      F           10   K-12 student   48067
## 6      F           10   K-12 student   48067
##                       MovieTitle ReleaseYear Action Adventure Animation
## 1                           Cujo        1983      0         0         0
## 2                          Crumb        1994      0         0         0
## 3      Friend of the Deceased, A        1997      0         0         0
## 4     Killer (Bulletproof Heart)        1994      0         0         0
## 5    Falcon and the Snowman, The        1984      0         0         0
## 6 Ever After: A Cinderella Story        1998      0         0         0
##   Children's Comedy Crime Documentary Drama Fantasy Film-Noir Horror
## 1          0      0     0           0     0       0         0      1
## 2          0      0     0           1     0       0         0      0
## 3          0      1     0           0     1       0         0      0
## 4          0      0     0           0     0       0         0      0
## 5          0      0     0           0     1       0         0      0
## 6          0      0     0           0     1       0         0      0
##   Musical Mystery Romance Sci-Fi Thriller War Western
## 1       0       0       0      0        1   0       0
## 2       0       0       0      0        0   0       0
## 3       0       0       0      0        0   0       0
## 4       0       0       0      0        1   0       0
## 5       0       0       0      0        0   0       0
## 6       0       0       1      0        0   0       0
```

```r
# using the utility function to check for missing values and total data
check_df(full_mov_df, 0)
```

```
## [1] "Checking for the total number of records in the dataframe to ensure completeness:"
## 
##       UserID      MovieID       Rating    TimeStamp         Date 
##      1000209      1000209      1000209      1000209      1000209 
##         Time          Age     AgeRange       Gender OccupationID 
##      1000209      1000209      1000209      1000209      1000209 
##   Occupation      ZipCode   MovieTitle  ReleaseYear       Action 
##      1000209      1000209      1000209      1000209      1000209 
##    Adventure    Animation   Children's       Comedy        Crime 
##      1000209      1000209      1000209      1000209      1000209 
##  Documentary        Drama      Fantasy    Film-Noir       Horror 
##      1000209      1000209      1000209      1000209      1000209 
##      Musical      Mystery      Romance       Sci-Fi     Thriller 
##      1000209      1000209      1000209      1000209      1000209 
##          War      Western 
##      1000209      1000209 
## 
## 
## [1] "Checking for the total number of missing values (NA) in the dataframe if any:"
## 
##       UserID      MovieID       Rating    TimeStamp         Date 
##            0            0            0            0            0 
##         Time          Age     AgeRange       Gender OccupationID 
##            0            0            0            0            0 
##   Occupation      ZipCode   MovieTitle  ReleaseYear       Action 
##            0            0         8825         8825         8825 
##    Adventure    Animation   Children's       Comedy        Crime 
##         8825         8825         8825         8825         8825 
##  Documentary        Drama      Fantasy    Film-Noir       Horror 
##         8825         8825         8825         8825         8825 
##      Musical      Mystery      Romance       Sci-Fi     Thriller 
##         8825         8825         8825         8825         8825 
##          War      Western 
##         8825         8825
```

```r
# finally, checking the structure of the full_mov_df
str(full_mov_df)
```

```
## 'data.frame':	1000209 obs. of  32 variables:
##  $ UserID      : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ MovieID     : num  1193 661 914 3408 2355 ...
##  $ Rating      : num  5 3 3 4 5 3 5 5 4 4 ...
##  $ TimeStamp   : num  9.78e+08 9.78e+08 9.78e+08 9.78e+08 9.79e+08 ...
##  $ Date        : chr  "2000-12-31" "2000-12-31" "2000-12-31" "2000-12-31" ...
##  $ Time        : chr  "17:12:40" "17:35:09" "17:32:48" "17:04:35" ...
##  $ Age         : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ AgeRange    : chr  "  Under 18" "  Under 18" "  Under 18" "  Under 18" ...
##  $ Gender      : chr  "F" "F" "F" "F" ...
##  $ OccupationID: num  10 10 10 10 10 10 10 10 10 10 ...
##  $ Occupation  : chr  "  K-12 student" "  K-12 student" "  K-12 student" "  K-12 student" ...
##  $ ZipCode     : num  48067 48067 48067 48067 48067 ...
##  $ MovieTitle  : chr  "Cujo" "Crumb" "Friend of the Deceased, A" "Killer (Bulletproof Heart)" ...
##  $ ReleaseYear : num  1983 1994 1997 1994 1984 ...
##  $ Action      : num  0 0 0 0 0 0 0 0 1 1 ...
##  $ Adventure   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Animation   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Children's  : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Comedy      : num  0 0 1 0 0 0 0 0 0 0 ...
##  $ Crime       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Documentary : num  0 1 0 0 0 0 0 0 0 0 ...
##  $ Drama       : num  0 0 1 0 1 1 0 0 0 1 ...
##  $ Fantasy     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Film-Noir   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Horror      : num  1 0 0 0 0 0 0 1 0 0 ...
##  $ Musical     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Mystery     : num  0 0 0 0 0 0 1 0 0 0 ...
##  $ Romance     : num  0 0 0 0 0 1 0 0 1 0 ...
##  $ Sci-Fi      : num  0 0 0 0 0 0 0 1 0 1 ...
##  $ Thriller    : num  1 0 0 1 0 0 1 0 1 1 ...
##  $ War         : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ Western     : num  0 0 0 0 0 0 0 0 0 0 ...
```

The utility function shows that there are 8825 records in the full_mov_df dataframe that contain NAs, starting from MovieTitle variable onwards. 

### Here is where I don't understand why? Need Help!

Question is that if there are movie titles with NAs for that user, then how can there be a rating value? Have I made a mistake in joining?


```r
# Checking for the number of unique movie titles
#unique(full_mov_df$MovieTitle)

# changing all NAs to -1
#full_mov_df[is.na(full_mov_df)] <- -1

# running the utility function again to do the check for NAs
#check_df(full_mov_df, 2)
```

Finally, from the sturcture output of the complete full_mov_df dataframe, it is apparent that many of the applicable variables will need to be converted to factors. Hence, this is taken care of below:


```r
full_mov_df$Rating <- as.factor(full_mov_df$Rating)
full_mov_df$AgeRange <- as.factor(full_mov_df$AgeRange)
full_mov_df$Gender <- as.factor(full_mov_df$Gender)
full_mov_df$Occupation <- as.factor(full_mov_df$Occupation)
full_mov_df$ZipCode <- as.factor(full_mov_df$ZipCode)
full_mov_df$MovieTitle <- as.factor(full_mov_df$MovieTitle)
full_mov_df$ReleaseYear <- as.factor(full_mov_df$ReleaseYear)
# converting generes to factors which are from col 15 to 32 in the full_mov_df
full_mov_df[15:32] <- lapply(full_mov_df[15:32], as.factor)

# Lastly, checking the structure of the final full_mov_df dataframe
str(full_mov_df)
```

```
## 'data.frame':	1000209 obs. of  32 variables:
##  $ UserID      : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ MovieID     : num  1193 661 914 3408 2355 ...
##  $ Rating      : Factor w/ 5 levels "1","2","3","4",..: 5 3 3 4 5 3 5 5 4 4 ...
##  $ TimeStamp   : num  9.78e+08 9.78e+08 9.78e+08 9.78e+08 9.79e+08 ...
##  $ Date        : chr  "2000-12-31" "2000-12-31" "2000-12-31" "2000-12-31" ...
##  $ Time        : chr  "17:12:40" "17:35:09" "17:32:48" "17:04:35" ...
##  $ Age         : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ AgeRange    : Factor w/ 7 levels "  18-24","  25-34",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ Gender      : Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ OccupationID: num  10 10 10 10 10 10 10 10 10 10 ...
##  $ Occupation  : Factor w/ 21 levels "  academic/educator",..: 10 10 10 10 10 10 10 10 10 10 ...
##  $ ZipCode     : Factor w/ 3402 levels "231","606","681",..: 1575 1575 1575 1575 1575 1575 1575 1575 1575 1575 ...
##  $ MovieTitle  : Factor w/ 3601 levels "'burbs, The",..: 804 798 1232 1771 1084 1049 3141 589 3022 878 ...
##  $ ReleaseYear : Factor w/ 81 levels "1919","1920",..: 64 75 78 75 65 79 22 77 78 79 ...
##  $ Action      : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 2 2 ...
##  $ Adventure   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Animation   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Children's  : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Comedy      : Factor w/ 2 levels "0","1": 1 1 2 1 1 1 1 1 1 1 ...
##  $ Crime       : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Documentary : Factor w/ 2 levels "0","1": 1 2 1 1 1 1 1 1 1 1 ...
##  $ Drama       : Factor w/ 2 levels "0","1": 1 1 2 1 2 2 1 1 1 2 ...
##  $ Fantasy     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Film-Noir   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Horror      : Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 2 1 1 ...
##  $ Musical     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Mystery     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 2 1 1 1 ...
##  $ Romance     : Factor w/ 2 levels "0","1": 1 1 1 1 1 2 1 1 2 1 ...
##  $ Sci-Fi      : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 2 1 2 ...
##  $ Thriller    : Factor w/ 2 levels "0","1": 2 1 1 2 1 1 2 1 2 2 ...
##  $ War         : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Western     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
```

***

The next step would be to do some EDA to find out about:

  1. How the data is distributed out in the dataframe (a plot of count vs generes)
  2. Gender vs Age distributions
  3. Occupation distributions
  4. Trends:
  
    * Generes over Release years - to find out if there is there a change/popularity of Generes over time
    * Age, Gender, Occupation vs Generes - to find out a little bit about users' and if there is a relationship with Generes
    * Similar to above but against Release years - to find out whether users' preferences on Generes have changed over time 
    * Similar to above but against Ranking - to find out the ranking behaviour of users

There might be some more questions as I go through the above.

***

### Collaborative Filtering model methodology

  1. Extract: Users vs MovieTitles vs Ratings dataframe
  2. Split the dataframe into Training, CV and Test datasets
    * Ensure that the data is randomly selected and is normally distributed in each of the datasets
  3. Run a cosine similarity algorithm to find similar users and recommend movies that they have not yet seen
  
