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

```r
mdata <- melt(mov_df, id=c("MovieID","MovieTitle","ReleaseYear"))
#check to see if there are any NA observations

sapply(mdata, function(y) sum(length(which(is.na(y)))))
```

```
##     MovieID  MovieTitle ReleaseYear    variable       value 
##           0           0           0           0       16890
```

```r
# removing records where value is NA
mdata <- mdata[!is.na(mdata$value),]
#sapply(mdata, function(y) sum(length(which(is.na(y)))))

# Createing a 1's column in the mov_df dataframe where 1 indicates that the movie has been classified into certain Genere
mdata$Type <- 1

# Widening the mov_df dataframe to have Generes as variables
mov_df <- dcast(mdata, MovieID + MovieTitle + ReleaseYear ~ value, value.var="Type")

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

The utility function shows that there are quite a few NA values for most of the generes. The assumption was that when the mov_df dataframe was created and the NAs were replaced by 0, this should have remained consistent when the join happened. The possible explanation could be be due to the different datatypes during joining.

To fix this:

```r
# Checking for the number of unique movie titles
unique(full_mov_df$MovieTitle)
```

```
##    [1] "Cujo"                                                                       
##    [2] "Crumb"                                                                      
##    [3] "Friend of the Deceased, A"                                                  
##    [4] "Killer (Bulletproof Heart)"                                                 
##    [5] "Falcon and the Snowman, The"                                                
##    [6] "Ever After: A Cinderella Story"                                             
##    [7] "Suspicion"                                                                  
##    [8] "Carnosaur 3: Primal Species"                                                
##    [9] "Speed 2: Cruise Control"                                                    
##   [10] "Deep Impact"                                                                
##   [11] "Squeeze"                                                                    
##   [12] "Beyond Silence"                                                             
##   [13] "Snows of Kilimanjaro, The"                                                  
##   [14] "Baraka"                                                                     
##   [15] "Friday the 13th Part VII: The New Blood"                                    
##   [16] "Committed"                                                                  
##   [17] "Inherit the Wind"                                                           
##   [18] "Buddy Holly Story, The"                                                     
##   [19] "Easy Money"                                                                 
##   [20] "Smiling Fish and Goat on Fire"                                              
##   [21] "Stealing Home"                                                              
##   [22] "Starship Troopers"                                                          
##   [23] "Merry War, A"                                                               
##   [24] "Devil's Own, The"                                                           
##   [25] "Anna and the King"                                                          
##   [26] "Secret Agent, The"                                                          
##   [27] "Blackbeard's Ghost"                                                         
##   [28] "Election"                                                                   
##   [29] "Gumby: The Movie"                                                           
##   [30] "Sense and Sensibility"                                                      
##   [31] "End of Days"                                                                
##   [32] "Hellraiser III: Hell on Earth"                                              
##   [33] "Texas Chainsaw Massacre, The"                                               
##   [34] "Trial and Error"                                                            
##   [35] "Adventures of Sebastian Cole, The"                                          
##   [36] "Great Expectations"                                                         
##   [37] "Wild Wild West"                                                             
##   [38] "Nightmare on Elm Street Part 2: Freddy's Revenge, A"                        
##   [39] "Keeping the Faith"                                                          
##   [40] "Private Benjamin"                                                           
##   [41] "Toy Story"                                                                  
##   [42] "Bat, The"                                                                   
##   [43] "Iron Eagle"                                                                 
##   [44] "Jonah Who Will Be 25 in the Year 2000"                                      
##   [45] "Seventh Seal, The (Sjunde inseglet, Det)"                                   
##   [46] "Friday the 13th"                                                            
##   [47] "Friday the 13th Part 2"                                                     
##   [48] "Weird Science"                                                              
##   [49] "Sommersby"                                                                  
##   [50] "Love and Other Catastrophes"                                                
##   [51] "Aimée & Jaguar"                                                             
##   [52] "Canadian Bacon"                                                             
##   [53] "Babysitter, The"                                                            
##   [54] "Century of Cinema, A"                                                       
##   [55] "Breaker Morant"                                                             
##   [56] "Stepmom"                                                                    
##   [57] "Excess Baggage"                                                             
##   [58] "My Best Girl"                                                               
##   [59] "Kull the Conqueror"                                                         
##   [60] "Matewan"                                                                    
##   [61] "Mirror, The (Zerkalo)"                                                      
##   [62] "Computer Wore Tennis Shoes, The"                                            
##   [63] "White Christmas"                                                            
##   [64] "Bread and Chocolate (Pane e cioccolata)"                                    
##   [65] "Charlotte's Web"                                                            
##   [66] "War of the Worlds, The"                                                     
##   [67] "Mod Squad, The"                                                             
##   [68] "Before the Rain (Pred dozhdot)"                                             
##   [69] "Multiplicity"                                                               
##   [70] "Fighting Seabees, The"                                                      
##   [71] "On Our Merry Way"                                                           
##   [72] "Legend"                                                                     
##   [73] "Sneakers"                                                                   
##   [74] "Joe the King"                                                               
##   [75] "Silent Fall"                                                                
##   [76] "Spiral Staircase, The"                                                      
##   [77] "Shaft's Big Score!"                                                         
##   [78] "Rope"                                                                       
##   [79] "God Said 'Ha!'"                                                             
##   [80] "Manchurian Candidate, The"                                                  
##   [81] "Holiday Inn"                                                                
##   [82] "Love and Death"                                                             
##   [83] "Character (Karakter)"                                                       
##   [84] "Blood Beach"                                                                
##   [85] "Marnie"                                                                     
##   [86] "Crew, The"                                                                  
##   [87] "Affliction"                                                                 
##   [88] "Man Who Knew Too Much, The"                                                 
##   [89] "Nineteen Eighty-Four"                                                       
##   [90] "Blown Away"                                                                 
##   [91] "Blue Chips"                                                                 
##   [92] "Blue Sky"                                                                   
##   [93] "American President, The"                                                    
##   [94] "Dangerous Minds"                                                            
##   [95] "Trois"                                                                      
##   [96] "Love Affair"                                                                
##   [97] "Two Much"                                                                   
##   [98] "Buddy"                                                                      
##   [99] "Walk on the Moon, A"                                                        
##  [100] "I, Worst of All (Yo, la peor de todas)"                                     
##  [101] "Road Trip"                                                                  
##  [102] "Murder in the First"                                                        
##  [103] "Great Ziegfeld, The"                                                        
##  [104] "Golden Child, The"                                                          
##  [105] "Twelve Chairs, The"                                                         
##  [106] "Gods Must Be Crazy, The"                                                    
##  [107] "Kama Sutra: A Tale of Love"                                                 
##  [108] "Night Falls on Manhattan"                                                   
##  [109] "Swan Princess, The"                                                         
##  [110] "Glory"                                                                      
##  [111] "Practical Magic"                                                            
##  [112] "Godfather: Part III, The"                                                   
##  [113] "Once Upon a Time... When We Were Colored"                                   
##  [114] "Amos & Andrew"                                                              
##  [115] "Pope of Greenwich Village, The"                                             
##  [116] "Poison Ivy II"                                                              
##  [117] "Stage Fright"                                                               
##  [118] "Pecker"                                                                     
##  [119] "Run Silent, Run Deep"                                                       
##  [120] "Bandit Queen"                                                               
##  [121] "Carnal Knowledge"                                                           
##  [122] "In the Realm of the Senses (Ai no corrida)"                                 
##  [123] "Train Ride to Hollywood"                                                    
##  [124] "Con Air"                                                                    
##  [125] "Addams Family, The"                                                         
##  [126] "In Dreams"                                                                  
##  [127] "Alien: Resurrection"                                                        
##  [128] "Snake Eyes"                                                                 
##  [129] "To Have, or Not"                                                            
##  [130] "Ulysses (Ulisse)"                                                           
##  [131] "Dial M for Murder"                                                          
##  [132] "Madame Sousatzka"                                                           
##  [133] "Asphalt Jungle, The"                                                        
##  [134] "Head On"                                                                    
##  [135] "Black Sunday (La Maschera Del Demonio)"                                     
##  [136] "Manny & Lo"                                                                 
##  [137] "Universal Soldier: The Return"                                              
##  [138] "Blood, Guts, Bullets and Octane"                                            
##  [139] "Notorious"                                                                  
##  [140] "Universal Soldier"                                                          
##  [141] "Love Stinks"                                                                
##  [142] "GoodFellas"                                                                 
##  [143] "Unzipped"                                                                   
##  [144] "Perfect Blue"                                                               
##  [145] "When We Were Kings"                                                         
##  [146] "Shaggy Dog, The"                                                            
##  [147] "JLG/JLG - autoportrait de décembre"                                         
##  [148] "Billy's Hollywood Screen Kiss"                                              
##  [149] "Nightmare on Elm Street, A"                                                 
##  [150] "Pitch Black"                                                                
##  [151] "Blue Juice"                                                                 
##  [152] "Walkabout"                                                                  
##  [153] "Black Hole, The"                                                            
##  [154] "Little Men"                                                                 
##  [155] "Message to Love: The Isle of Wight Festival"                                
##  [156] "Christmas Vacation"                                                         
##  [157] "Slingshot, The (Kådisbellan )"                                              
##  [158] "Night at the Roxbury, A"                                                    
##  [159] "Indian in the Cupboard, The"                                                
##  [160] "Phantasm II"                                                                
##  [161] "Big Lebowski, The"                                                          
##  [162] "Absolute Power"                                                             
##  [163] "No Way Out"                                                                 
##  [164] "Saturn 3"                                                                   
##  [165] "Howling II: Your Sister Is a Werewolf"                                      
##  [166] "Across the Sea of Time"                                                     
##  [167] "Nightmares"                                                                 
##  [168] "Some Kind of Wonderful"                                                     
##  [169] "Hamlet"                                                                     
##  [170] "Things to Do in Denver when You're Dead"                                    
##  [171] "Dance with Me"                                                              
##  [172] "Amityville: Dollhouse"                                                      
##  [173] "Dead Man on Campus"                                                         
##  [174] NA                                                                           
##  [175] "Wrongfully Accused"                                                         
##  [176] "Ciao, Professore! (Io speriamo che me la cavo )"                            
##  [177] "Meteor"                                                                     
##  [178] "Nashville"                                                                  
##  [179] "Some Folks Call It a Sling Blade"                                           
##  [180] "Crying Game, The"                                                           
##  [181] "Ready to Rumble"                                                            
##  [182] "Velvet Goldmine"                                                            
##  [183] "Pompatus of Love, The"                                                      
##  [184] "Seven Samurai (The Magnificent Seven) (Shichinin no samurai)"               
##  [185] "Match, The"                                                                 
##  [186] "Office Space"                                                               
##  [187] "Sabotage"                                                                   
##  [188] "My Own Private Idaho"                                                       
##  [189] "Shanghai Triad (Yao a yao yao dao waipo qiao)"                              
##  [190] "Crocodile Dundee II"                                                        
##  [191] "Ashes of Time"                                                              
##  [192] "Child's Play 3"                                                             
##  [193] "Temptress Moon (Feng Yue)"                                                  
##  [194] "54"                                                                         
##  [195] "Anastasia"                                                                  
##  [196] "Death and the Maiden"                                                       
##  [197] "Stardust Memories"                                                          
##  [198] "Butterfly Kiss"                                                             
##  [199] "Champagne"                                                                  
##  [200] "Scream 3"                                                                   
##  [201] "Goya in Bordeaux (Goya en Bodeos)"                                          
##  [202] "Night to Remember, A"                                                       
##  [203] "Walking and Talking"                                                        
##  [204] "Seven Days in May"                                                          
##  [205] "Home Alone 2: Lost in New York"                                             
##  [206] "Tender Mercies"                                                             
##  [207] "Hang 'em High"                                                              
##  [208] "Angel Heart"                                                                
##  [209] "Dark Crystal, The"                                                          
##  [210] "Friday the 13th Part VIII: Jason Takes Manhattan"                           
##  [211] "Ransom"                                                                     
##  [212] "Angela's Ashes"                                                             
##  [213] "Strangeland"                                                                
##  [214] "World of Apu, The (Apur Sansar)"                                            
##  [215] "Saltmen of Tibet, The"                                                      
##  [216] "Mad Max"                                                                    
##  [217] "Surf Nazis Must Die"                                                        
##  [218] "Trouble in Paradise"                                                        
##  [219] "Diamonds"                                                                   
##  [220] "Tron"                                                                       
##  [221] "Alice in Wonderland"                                                        
##  [222] "Deer Hunter, The"                                                           
##  [223] "Last Picture Show, The"                                                     
##  [224] "Mighty Joe Young"                                                           
##  [225] "History of the World: Part I"                                               
##  [226] "Desperately Seeking Susan"                                                  
##  [227] "Moonlight and Valentino"                                                    
##  [228] "Empty Mirror, The"                                                          
##  [229] "Barney's Great Adventure"                                                   
##  [230] "King and I, The"                                                            
##  [231] "Diner"                                                                      
##  [232] "Star Wars: Episode V - The Empire Strikes Back"                             
##  [233] "Saragossa Manuscript, The (Rekopis znaleziony w Saragossie)"                
##  [234] "Vegas Vacation"                                                             
##  [235] "Donnie Brasco"                                                              
##  [236] "Navigator: A Mediaeval Odyssey, The"                                        
##  [237] "Roman Holiday"                                                              
##  [238] "Basketball Diaries, The"                                                    
##  [239] "Freeway"                                                                    
##  [240] "Phantasm"                                                                   
##  [241] "My Favorite Year"                                                           
##  [242] "Happy Gilmore"                                                              
##  [243] "Lucas"                                                                      
##  [244] "Back to the Future"                                                         
##  [245] "Lost World: Jurassic Park, The"                                             
##  [246] "200 Cigarettes"                                                             
##  [247] "Wings of the Dove, The"                                                     
##  [248] "Down by Law"                                                                
##  [249] "Girl on the Bridge, The (La Fille sur le Pont)"                             
##  [250] "Mission to Mars"                                                            
##  [251] "Stiff Upper Lips"                                                           
##  [252] "12 Angry Men"                                                               
##  [253] "Mille bolle blu"                                                            
##  [254] "Apocalypse Now"                                                             
##  [255] "Ed's Next Move"                                                             
##  [256] "Golden Bowl, The"                                                           
##  [257] "Prom Night IV: Deliver Us From Evil"                                        
##  [258] "Fallen"                                                                     
##  [259] "That Thing You Do!"                                                         
##  [260] "Surviving Picasso"                                                          
##  [261] "Bushwhacked"                                                                
##  [262] "Needful Things"                                                             
##  [263] "Autumn in New York"                                                         
##  [264] "Cape Fear"                                                                  
##  [265] "Cure, The"                                                                  
##  [266] "Hollow Man"                                                                 
##  [267] "Billy's Holiday"                                                            
##  [268] "Where the Buffalo Roam"                                                     
##  [269] "Jagged Edge"                                                                
##  [270] "Purple Noon"                                                                
##  [271] "Virus"                                                                      
##  [272] "Bowfinger"                                                                  
##  [273] "Roadside Prophets"                                                          
##  [274] "Jerry Springer: Ringmaster"                                                 
##  [275] "River, The"                                                                 
##  [276] "Curtis's Charm"                                                             
##  [277] "Governess, The"                                                             
##  [278] "Hot Spot, The"                                                              
##  [279] "Limey, The"                                                                 
##  [280] "Cookie's Fortune"                                                           
##  [281] "Circus"                                                                     
##  [282] "Encino Man"                                                                 
##  [283] "Country"                                                                    
##  [284] "Airport"                                                                    
##  [285] "Copycat"                                                                    
##  [286] "Carrington"                                                                 
##  [287] "Mr. Saturday Night"                                                         
##  [288] "Heaven Can Wait"                                                            
##  [289] "My Life"                                                                    
##  [290] "Raven, The"                                                                 
##  [291] "Bridge at Remagen, The"                                                     
##  [292] "Blade Runner"                                                               
##  [293] "Gone Fishin'"                                                               
##  [294] "Show, The"                                                                  
##  [295] "Krippendorf's Tribe"                                                        
##  [296] "Thunderbolt and Lightfoot"                                                  
##  [297] "It Came from Hollywood"                                                     
##  [298] "Love in Bloom"                                                              
##  [299] "Murder at 1600"                                                             
##  [300] "Waterboy, The"                                                              
##  [301] "Thunderball"                                                                
##  [302] "Monster, The (Il Mostro)"                                                   
##  [303] "Logan's Run"                                                                
##  [304] "Being Human"                                                                
##  [305] "Jason's Lyric"                                                              
##  [306] "Lost & Found"                                                               
##  [307] "Hidden, The"                                                                
##  [308] "Frequency"                                                                  
##  [309] "Escape from the Planet of the Apes"                                         
##  [310] "Gandhi"                                                                     
##  [311] "For Love of the Game"                                                       
##  [312] "Air America"                                                                
##  [313] "Nine 1/2 Weeks"                                                             
##  [314] "Invasion of the Body Snatchers"                                             
##  [315] "While You Were Sleeping"                                                    
##  [316] "Powder"                                                                     
##  [317] "Long Walk Home, The"                                                        
##  [318] "Cocoon"                                                                     
##  [319] "Othello"                                                                    
##  [320] "Avalanche"                                                                  
##  [321] "Big Hit, The"                                                               
##  [322] "Beyond the Poseidon Adventure"                                              
##  [323] "Tess of the Storm Country"                                                  
##  [324] "Entrapment"                                                                 
##  [325] "Modern Affair, A"                                                           
##  [326] "Blood Spattered Bride, The (La Novia Ensangrentada)"                        
##  [327] "Tic Code, The"                                                              
##  [328] "Herbie Rides Again"                                                         
##  [329] "Thumbelina"                                                                 
##  [330] "Delicatessen"                                                               
##  [331] "Romy and Michele's High School Reunion"                                     
##  [332] "Rocky III"                                                                  
##  [333] "Duel in the Sun"                                                            
##  [334] "Great Santini, The"                                                         
##  [335] "Young Sherlock Holmes"                                                      
##  [336] "Saving Private Ryan"                                                        
##  [337] "Bronx Tale, A"                                                              
##  [338] "Dancer, Texas Pop. 81"                                                      
##  [339] "Idle Hands"                                                                 
##  [340] "Welcome to the Dollhouse"                                                   
##  [341] "Great Muppet Caper, The"                                                    
##  [342] "Enemy of the State"                                                         
##  [343] "Heaven"                                                                     
##  [344] "King of Masks, The (Bian Lian)"                                             
##  [345] "Suture"                                                                     
##  [346] "Little Women"                                                               
##  [347] "Clean Slate"                                                                
##  [348] "Cliffhanger"                                                                
##  [349] "Phantom, The"                                                               
##  [350] "Boys from Brazil, The"                                                      
##  [351] "Tinseltown"                                                                 
##  [352] "That's Life!"                                                               
##  [353] "Star Trek IV: The Voyage Home"                                              
##  [354] "Actor's Revenge, An (Yukinojo Henge)"                                       
##  [355] "Greatest Show on Earth, The"                                                
##  [356] "Mission, The"                                                               
##  [357] "Little Shop of Horrors"                                                     
##  [358] "Nikita (La Femme Nikita)"                                                   
##  [359] "20,000 Leagues Under the Sea"                                               
##  [360] "In Crowd, The"                                                              
##  [361] "I'm Not Rappaport"                                                          
##  [362] "Secret of Roan Inish, The"                                                  
##  [363] "$1,000,000 Duck"                                                            
##  [364] "Winnie the Pooh and the Blustery Day"                                       
##  [365] "Turning, The"                                                               
##  [366] "Black Cauldron, The"                                                        
##  [367] "General, The"                                                               
##  [368] "Re-Animator"                                                                
##  [369] "Touch"                                                                      
##  [370] "English Patient, The"                                                       
##  [371] "Xiu Xiu: The Sent-Down Girl (Tian yu)"                                      
##  [372] "Romeo and Juliet"                                                           
##  [373] "Three Days of the Condor"                                                   
##  [374] "Happy Weekend"                                                              
##  [375] "American Werewolf in London, An"                                            
##  [376] "So Dear to My Heart"                                                        
##  [377] "Blade"                                                                      
##  [378] "Frances"                                                                    
##  [379] "Squanto: A Warrior's Tale"                                                  
##  [380] "Apartment, The"                                                             
##  [381] "Mary Poppins"                                                               
##  [382] "Amityville: A New Generation"                                               
##  [383] "Unforgettable"                                                              
##  [384] "Welcome To Sarajevo"                                                        
##  [385] "American Flyers"                                                            
##  [386] "Cheetah"                                                                    
##  [387] "Flirt"                                                                      
##  [388] "Friday the 13th Part 3: 3D"                                                 
##  [389] "Among Giants"                                                               
##  [390] "Audrey Rose"                                                                
##  [391] "Courage Under Fire"                                                         
##  [392] "Official Story, The (La Historia Oficial)"                                  
##  [393] "Pot O' Gold"                                                                
##  [394] "Dragonheart"                                                                
##  [395] "Hello Mary Lou: Prom Night II"                                              
##  [396] "Next Step, The"                                                             
##  [397] "Down Periscope"                                                             
##  [398] "Bluebeard"                                                                  
##  [399] "Pather Panchali"                                                            
##  [400] "Leatherface: Texas Chainsaw Massacre III"                                   
##  [401] "Citizen's Band (a.k.a. Handle with Care)"                                   
##  [402] "Honigmond"                                                                  
##  [403] "Spy Hard"                                                                   
##  [404] "Carnosaur"                                                                  
##  [405] "French Twist (Gazon maudit)"                                                
##  [406] "Relic, The"                                                                 
##  [407] "Larger Than Life"                                                           
##  [408] "Sugar Town"                                                                 
##  [409] "Two Deaths"                                                                 
##  [410] "Man from Down Under, The"                                                   
##  [411] "Rich and Strange"                                                           
##  [412] "Callejón de los milagros, El"                                               
##  [413] "Kramer Vs. Kramer"                                                          
##  [414] "Oscar and Lucinda (a.k.a. Oscar & Lucinda)"                                 
##  [415] "Reality Bites"                                                              
##  [416] "Rosencrantz and Guildenstern Are Dead"                                      
##  [417] "Old Yeller"                                                                 
##  [418] "Stepford Wives, The"                                                        
##  [419] "Mafia!"                                                                     
##  [420] "Party Girl"                                                                 
##  [421] "If Lucy Fell"                                                               
##  [422] "Twice Upon a Yesterday"                                                     
##  [423] "Bad Lieutenant"                                                             
##  [424] "Perez Family, The"                                                          
##  [425] "In Too Deep"                                                                
##  [426] "Edward Scissorhands"                                                        
##  [427] "Losing Chase"                                                               
##  [428] "Telling You"                                                                
##  [429] "Mulholland Falls"                                                           
##  [430] "Proposition, The"                                                           
##  [431] "Only Angels Have Wings"                                                     
##  [432] "Deadly Friend"                                                              
##  [433] "Stranger, The"                                                              
##  [434] "Clan of the Cave Bear, The"                                                 
##  [435] "Bat Whispers, The"                                                          
##  [436] "Andre"                                                                      
##  [437] "My Cousin Vinny"                                                            
##  [438] "Beauty"                                                                     
##  [439] "Booty Call"                                                                 
##  [440] "Bamboozled"                                                                 
##  [441] "Sound of Music, The"                                                        
##  [442] "187"                                                                        
##  [443] "Nelly & Monsieur Arnaud"                                                    
##  [444] "Love & Sex"                                                                 
##  [445] "Price of Glory"                                                             
##  [446] "Police Academy"                                                             
##  [447] "Tango"                                                                      
##  [448] "Monty Python and the Holy Grail"                                            
##  [449] "Rhyme & Reason"                                                             
##  [450] "Curdled"                                                                    
##  [451] "Breaks, The"                                                                
##  [452] "Turn It Up"                                                                 
##  [453] "Looking for Richard"                                                        
##  [454] "My Chauffeur"                                                               
##  [455] "Zachariah"                                                                  
##  [456] "Late August, Early September (Fin août, début septembre)"                   
##  [457] "Better Than Chocolate"                                                      
##  [458] "Psycho"                                                                     
##  [459] "Once Upon a Time in the West"                                               
##  [460] "In Search of the Castaways"                                                 
##  [461] "Snowriders"                                                                 
##  [462] "Reach the Rock"                                                             
##  [463] "Liar Liar"                                                                  
##  [464] "Lethal Weapon"                                                              
##  [465] "Thieves (Voleurs, Les)"                                                     
##  [466] "Go"                                                                         
##  [467] "Dirty Work"                                                                 
##  [468] "Bent"                                                                       
##  [469] "Modulations"                                                                
##  [470] "Towering Inferno, The"                                                      
##  [471] "Mystery Science Theater 3000: The Movie"                                    
##  [472] "Miss Julie"                                                                 
##  [473] "Zeus and Roxanne"                                                           
##  [474] "Kiss of Death"                                                              
##  [475] "Cutthroat Island"                                                           
##  [476] "Jingle All the Way"                                                         
##  [477] "Sister Act 2: Back in the Habit"                                            
##  [478] "Aladdin and the King of Thieves"                                            
##  [479] "Another Stakeout"                                                           
##  [480] "Prince of Central Park, The"                                                
##  [481] "Glengarry Glen Ross"                                                        
##  [482] "Annie Hall"                                                                 
##  [483] "Beverly Hillbillies, The"                                                   
##  [484] "Beverly Hills Cop III"                                                      
##  [485] "Cement Garden, The"                                                         
##  [486] "Braveheart"                                                                 
##  [487] "Boxing Helena"                                                              
##  [488] "Houseguest"                                                                 
##  [489] "Barefoot in the Park"                                                       
##  [490] "Bottle Rocket"                                                              
##  [491] "When Harry Met Sally..."                                                    
##  [492] "Broadway Damage"                                                            
##  [493] "Shanghai Surprise"                                                          
##  [494] "Dead Poets Society"                                                         
##  [495] "Austin Powers: International Man of Mystery"                                
##  [496] "Six Ways to Sunday"                                                         
##  [497] "City Slickers II: The Legend of Curly's Gold"                               
##  [498] "Three Seasons"                                                              
##  [499] "Primal Fear"                                                                
##  [500] "Hustler, The"                                                               
##  [501] "Bitter Moon"                                                                
##  [502] "Breathing Room"                                                             
##  [503] "Sea Wolves, The"                                                            
##  [504] "Mr. Wrong"                                                                  
##  [505] "Three Ages, The"                                                            
##  [506] "Cradle Will Rock, The"                                                      
##  [507] "Finding North"                                                              
##  [508] "I'll Never Forget What's 'is Name"                                          
##  [509] "Baby Geniuses"                                                              
##  [510] "Junior"                                                                     
##  [511] "Freddy's Dead: The Final Nightmare"                                         
##  [512] "Bullets Over Broadway"                                                      
##  [513] "Shining, The"                                                               
##  [514] "Blob, The"                                                                  
##  [515] "Withnail and I"                                                             
##  [516] "Good Man in Africa, A"                                                      
##  [517] "Four Days in September"                                                     
##  [518] "Mutters Courage"                                                            
##  [519] "Akira"                                                                      
##  [520] "Son of Frankenstein"                                                        
##  [521] "Heaven & Earth"                                                             
##  [522] "Master Ninja I"                                                             
##  [523] "Way of the Gun, The"                                                        
##  [524] "Amateur"                                                                    
##  [525] "Love and a .45"                                                             
##  [526] "Lethal Weapon 2"                                                            
##  [527] "Drop Zone"                                                                  
##  [528] "Permanent Midnight"                                                         
##  [529] "Best in Show"                                                               
##  [530] "Girls Town"                                                                 
##  [531] "To Be or Not to Be"                                                         
##  [532] "Island of Dr. Moreau, The"                                                  
##  [533] "Distinguished Gentleman, The"                                               
##  [534] "Little Mermaid, The"                                                        
##  [535] "Grumpier Old Men"                                                           
##  [536] "Black Beauty"                                                               
##  [537] "52 Pick-Up"                                                                 
##  [538] "House II: The Second Story"                                                 
##  [539] "Heart Condition"                                                            
##  [540] "Stalingrad"                                                                 
##  [541] "Kundun"                                                                     
##  [542] "Dead Ringers"                                                               
##  [543] "Carried Away"                                                               
##  [544] "Henry: Portrait of a Serial Killer, Part 2"                                 
##  [545] "Easy Virtue"                                                                
##  [546] "Cops and Robbersons"                                                        
##  [547] "Mountain Eagle, The"                                                        
##  [548] "My Fellow Americans"                                                        
##  [549] "Double Happiness"                                                           
##  [550] "Renaissance Man"                                                            
##  [551] "Rising Sun"                                                                 
##  [552] "Ladybird Ladybird"                                                          
##  [553] "Without Limits"                                                             
##  [554] "Story of Us, The"                                                           
##  [555] "Fever Pitch"                                                                
##  [556] "Gulliver's Travels"                                                         
##  [557] "Déjà Vu"                                                                    
##  [558] "Family Plot"                                                                
##  [559] "Small Wonders"                                                              
##  [560] "Topaz"                                                                      
##  [561] "Boys on the Side"                                                           
##  [562] "Zone 39"                                                                    
##  [563] "Robin Hood: Men in Tights"                                                  
##  [564] "Rough Magic"                                                                
##  [565] "Beyond Rangoon"                                                             
##  [566] "Terminator 2: Judgment Day"                                                 
##  [567] "Confessional, The (Le Confessionnal)"                                       
##  [568] "Getting Even with Dad"                                                      
##  [569] "With Byrd at the South Pole"                                                
##  [570] "Boiler Room"                                                                
##  [571] "GoldenEye"                                                                  
##  [572] "Hanging Up"                                                                 
##  [573] "Dadetown"                                                                   
##  [574] "Extreme Measures"                                                           
##  [575] "Panther"                                                                    
##  [576] "All That Jazz"                                                              
##  [577] "Red Sorghum (Hong Gao Liang)"                                               
##  [578] "Crimes and Misdemeanors"                                                    
##  [579] "Prophecy II, The"                                                           
##  [580] "Bats"                                                                       
##  [581] "Alarmist, The"                                                              
##  [582] "Choices"                                                                    
##  [583] "Why Do Fools Fall In Love?"                                                 
##  [584] "Jungle Fever"                                                               
##  [585] "Pillow Book, The"                                                           
##  [586] "Sudden Manhattan"                                                           
##  [587] "Pretty Woman"                                                               
##  [588] "Marcello Mastroianni: I Remember Yes, I Remember"                           
##  [589] "Bringing Out the Dead"                                                      
##  [590] "Molly"                                                                      
##  [591] "See the Sea (Regarde la mer)"                                               
##  [592] "Dream Man"                                                                  
##  [593] "Year of Living Dangerously"                                                 
##  [594] "Outbreak"                                                                   
##  [595] "Firelight"                                                                  
##  [596] "Boys, Les"                                                                  
##  [597] "MatchMaker, The"                                                            
##  [598] "Phoenix"                                                                    
##  [599] "Aparajito"                                                                  
##  [600] "Masque of the Red Death, The"                                               
##  [601] "Red Dawn"                                                                   
##  [602] "So I Married an Axe Murderer"                                               
##  [603] "Koyaanisqatsi"                                                              
##  [604] "Pie in the Sky"                                                             
##  [605] "Amadeus"                                                                    
##  [606] "High Plains Drifter"                                                        
##  [607] "Paradine Case, The"                                                         
##  [608] "Lifeboat"                                                                   
##  [609] "Good Will Hunting"                                                          
##  [610] "Razor's Edge, The"                                                          
##  [611] "Mr. & Mrs. Smith"                                                           
##  [612] "Stupids, The"                                                               
##  [613] "Funny Bones"                                                                
##  [614] "Jane Eyre"                                                                  
##  [615] "Bay of Blood (Reazione a catena)"                                           
##  [616] "Ruling Class, The"                                                          
##  [617] "Angela"                                                                     
##  [618] "Shaft in Africa"                                                            
##  [619] "Bad Moon"                                                                   
##  [620] "House on Haunted Hill, The"                                                 
##  [621] "Kid, The"                                                                   
##  [622] "Return with Honor"                                                          
##  [623] "But I'm a Cheerleader"                                                      
##  [624] "Time of the Gypsies (Dom za vesanje)"                                       
##  [625] "Paralyzing Fear: The Story of Polio in America, A"                          
##  [626] "Fire Within, The (Le Feu Follet)"                                           
##  [627] "Species II"                                                                 
##  [628] "Love Bewitched, A (El Amor Brujo)"                                          
##  [629] "Secret Agent"                                                               
##  [630] "Sullivan's Travels"                                                         
##  [631] "Waltzes from Vienna"                                                        
##  [632] "Sweet Hereafter, The"                                                       
##  [633] "Mystery Train"                                                              
##  [634] "Chariots of Fire"                                                           
##  [635] "Tequila Sunrise"                                                            
##  [636] "Man Who Would Be King, The"                                                 
##  [637] "Parallel Sons"                                                              
##  [638] "Go West"                                                                    
##  [639] "Sheltering Sky, The"                                                        
##  [640] "Bone Collector, The"                                                        
##  [641] "Fiendish Plot of Dr. Fu Manchu, The"                                        
##  [642] "Them!"                                                                      
##  [643] "Go Now"                                                                     
##  [644] "Thing, The"                                                                 
##  [645] "Player, The"                                                                
##  [646] "Misérables, Les"                                                            
##  [647] "Clockwatchers"                                                              
##  [648] "Species"                                                                    
##  [649] "Center Stage"                                                               
##  [650] "I Shot Andy Warhol"                                                         
##  [651] "Driving Miss Daisy"                                                         
##  [652] "Take the Money and Run"                                                     
##  [653] "Metropolitan"                                                               
##  [654] "Family Thing, A"                                                            
##  [655] "Mad Dog Time"                                                               
##  [656] "8 1/2"                                                                      
##  [657] "Irma la Douce"                                                              
##  [658] "Day the Earth Stood Still, The"                                             
##  [659] "Prophecy, The"                                                              
##  [660] "Lawn Dogs"                                                                  
##  [661] "Stonewall"                                                                  
##  [662] "High School High"                                                           
##  [663] "Godzilla"                                                                   
##  [664] "Phat Beach"                                                                 
##  [665] "Labyrinth"                                                                  
##  [666] "Vermin"                                                                     
##  [667] "No Looking Back"                                                            
##  [668] "Stars Fell on Henrietta, The"                                               
##  [669] "Nightmare on Elm Street 4: The Dream Master, A"                             
##  [670] "Retroactive"                                                                
##  [671] "Nell"                                                                       
##  [672] "Jade"                                                                       
##  [673] "Batman: Mask of the Phantasm"                                               
##  [674] "Voyage of the Damned"                                                       
##  [675] "When a Man Loves a Woman"                                                   
##  [676] "Hope Floats"                                                                
##  [677] "Friday the 13th: The Final Chapter"                                         
##  [678] "Condorman"                                                                  
##  [679] "Friday the 13th Part V: A New Beginning"                                    
##  [680] "Bastard Out of Carolina"                                                    
##  [681] "Land Girls, The"                                                            
##  [682] "Basquiat"                                                                   
##  [683] "Prom Night"                                                                 
##  [684] "Body Snatcher, The"                                                         
##  [685] "Odessa File, The"                                                           
##  [686] "Colonel Chabert, Le"                                                        
##  [687] "Saphead, The"                                                               
##  [688] "Mad About Mambo"                                                            
##  [689] "Wyatt Earp"                                                                 
##  [690] "Mr. Jealousy"                                                               
##  [691] "Mulan"                                                                      
##  [692] "Low Life, The"                                                              
##  [693] "Carnival of Souls"                                                          
##  [694] "Original Gangstas"                                                          
##  [695] "BASEketball"                                                                
##  [696] "Smilla's Sense of Snow"                                                     
##  [697] "Lone Star"                                                                  
##  [698] "Big Kahuna, The"                                                            
##  [699] "Roger & Me"                                                                 
##  [700] "Eye of the Beholder"                                                        
##  [701] "Get on the Bus"                                                             
##  [702] "I'll Do Anything"                                                           
##  [703] "House on Haunted Hill"                                                      
##  [704] "Santitos"                                                                   
##  [705] "Phantasm IV: Oblivion"                                                      
##  [706] "Pumpkinhead"                                                                
##  [707] "To Catch a Thief"                                                           
##  [708] "Beyond Bedlam"                                                              
##  [709] "Voyage to the Beginning of the World"                                       
##  [710] "Public Access"                                                              
##  [711] "Safe Men"                                                                   
##  [712] "Soldier's Story, A"                                                         
##  [713] "Atlantic City"                                                              
##  [714] "Don't Look in the Basement!"                                                
##  [715] "Autumn Sonata (Höstsonaten )"                                               
##  [716] "Small Soldiers"                                                             
##  [717] "Spy Who Loved Me, The"                                                      
##  [718] "Rendezvous in Paris (Rendez-vous de Paris, Les)"                            
##  [719] "Alien Nation"                                                               
##  [720] "Real Blonde, The"                                                           
##  [721] "American Beauty"                                                            
##  [722] "Vagabond (Sans toit ni loi)"                                                
##  [723] "Mr. Nice Guy"                                                               
##  [724] "Walk in the Clouds, A"                                                      
##  [725] "Mad Max 2 (a.k.a. The Road Warrior)"                                        
##  [726] "Apollo 13"                                                                  
##  [727] "Third Miracle, The"                                                         
##  [728] "Bad Girls"                                                                  
##  [729] "Urban Legends: Final Cut"                                                   
##  [730] "Woman on Top"                                                               
##  [731] "Trans"                                                                      
##  [732] "Shallow Grave"                                                              
##  [733] "Whatever"                                                                   
##  [734] "Nemesis 2: Nebula"                                                          
##  [735] "Blue Streak"                                                                
##  [736] "Hard Day's Night, A"                                                        
##  [737] "American Tail, An"                                                          
##  [738] "Poseidon Adventure, The"                                                    
##  [739] "Antonia's Line (Antonia)"                                                   
##  [740] "Keys to Tulsa"                                                              
##  [741] "Kissed"                                                                     
##  [742] "8 Heads in a Duffel Bag"                                                    
##  [743] "Blink"                                                                      
##  [744] "Death Becomes Her"                                                          
##  [745] "What Planet Are You From?"                                                  
##  [746] "Dancemaker"                                                                 
##  [747] "Color of Money, The"                                                        
##  [748] "Beach Party"                                                                
##  [749] "Streetcar Named Desire, A"                                                  
##  [750] "Firewalker"                                                                 
##  [751] "Separation, The (La Séparation)"                                            
##  [752] "St. Elmo's Fire"                                                            
##  [753] "Total Eclipse"                                                              
##  [754] "Sunshine"                                                                   
##  [755] "Trixie"                                                                     
##  [756] "Beyond the Mat"                                                             
##  [757] "Carlito's Way"                                                              
##  [758] "Voyage to the Bottom of the Sea"                                            
##  [759] "Even Dwarfs Started Small (Auch Zwerge haben klein angefangen)"             
##  [760] "Street Fighter"                                                             
##  [761] "Loaded Weapon 1"                                                            
##  [762] "Manxman, The"                                                               
##  [763] "Slums of Beverly Hills, The"                                                
##  [764] "Dirty Dozen, The"                                                           
##  [765] "Natural Born Killers"                                                       
##  [766] "Goldfinger"                                                                 
##  [767] "Henry: Portrait of a Serial Killer"                                         
##  [768] "From Russia with Love"                                                      
##  [769] "To Wong Foo, Thanks for Everything! Julie Newmar"                           
##  [770] "Orlando"                                                                    
##  [771] "Mediterraneo"                                                               
##  [772] "Forces of Nature"                                                           
##  [773] "Thin Blue Line, The"                                                        
##  [774] "Pyromaniac's Love Story, A"                                                 
##  [775] "Adventures of Elmo in Grouchland, The"                                      
##  [776] "Simon Sez"                                                                  
##  [777] "Mystery, Alaska"                                                            
##  [778] "Three Kings"                                                                
##  [779] "Happy, Texas"                                                               
##  [780] "New Rose Hotel"                                                             
##  [781] "Romance"                                                                    
##  [782] "Endurance"                                                                  
##  [783] "Princess Bride, The"                                                        
##  [784] "Devil Girl From Mars"                                                       
##  [785] "Mo' Better Blues"                                                           
##  [786] "Clear and Present Danger"                                                   
##  [787] "Soul Food"                                                                  
##  [788] "Window to Paris"                                                            
##  [789] "Boys of St. Vincent, The"                                                   
##  [790] "Ferris Bueller's Day Off"                                                   
##  [791] "Grand Day Out, A"                                                           
##  [792] "Quiet Man, The"                                                             
##  [793] "Jamaica Inn"                                                                
##  [794] "Threesome"                                                                  
##  [795] "Make Them Die Slowly (Cannibal Ferox)"                                      
##  [796] "Let's Get Harry"                                                            
##  [797] "Agnes Browne"                                                               
##  [798] "Stranger in the House"                                                      
##  [799] "Santa with Muscles"                                                         
##  [800] "Allan Quartermain and the Lost City of Gold"                                
##  [801] "Two Jakes, The"                                                             
##  [802] "Naked Gun: From the Files of Police Squad!, The"                            
##  [803] "Rent-A-Cop"                                                                 
##  [804] "Midaq Alley (Callejón de los milagros, El)"                                 
##  [805] "Cobb"                                                                       
##  [806] "War Zone, The"                                                              
##  [807] "Godzilla (Gojira)"                                                          
##  [808] "Catfish in Black Bean Sauce"                                                
##  [809] "Another Day in Paradise"                                                    
##  [810] "Somebody is Waiting"                                                        
##  [811] "Ghosts of Mississippi"                                                      
##  [812] "Circus, The"                                                                
##  [813] "Fly II, The"                                                                
##  [814] "Miracle on 34th Street"                                                     
##  [815] "Cleo From 5 to 7 (Cléo de 5 à 7)"                                           
##  [816] "Girlfight"                                                                  
##  [817] "Sleeper"                                                                    
##  [818] "Catwalk"                                                                    
##  [819] "Mrs. Miniver"                                                               
##  [820] "Benny & Joon"                                                               
##  [821] "Jail Bait"                                                                  
##  [822] "Rage: Carrie 2, The"                                                        
##  [823] "Mike's Murder"                                                              
##  [824] "Bean"                                                                       
##  [825] "Mighty Aphrodite"                                                           
##  [826] "Replacement Killers, The"                                                   
##  [827] "Reckless"                                                                   
##  [828] "Hungarian Fairy Tale, A"                                                    
##  [829] "Blackmail"                                                                  
##  [830] "Tom Jones"                                                                  
##  [831] "Titanic"                                                                    
##  [832] "North by Northwest"                                                         
##  [833] "Married to the Mob"                                                         
##  [834] "Trouble with Harry, The"                                                    
##  [835] "Coogan's Bluff"                                                             
##  [836] "Croupier"                                                                   
##  [837] "Thin Line Between Love and Hate, A"                                         
##  [838] "Chamber, The"                                                               
##  [839] "Lords of Flatbush, The"                                                     
##  [840] "Two if by Sea"                                                              
##  [841] "Substance of Fire, The"                                                     
##  [842] "Interiors"                                                                  
##  [843] "Draughtsman's Contract, The"                                                
##  [844] "Police Academy 3: Back in Training"                                         
##  [845] "Love Jones"                                                                 
##  [846] "Marie Baie Des Anges"                                                       
##  [847] "Proprietor, The"                                                            
##  [848] "Lover's Knot"                                                               
##  [849] "Mr. Death: The Rise and Fall of Fred A. Leuchter Jr."                       
##  [850] "American Tail: Fievel Goes West, An"                                        
##  [851] "Next Best Thing, The"                                                       
##  [852] "Three Amigos!"                                                              
##  [853] "Color of Night"                                                             
##  [854] "Home for the Holidays"                                                      
##  [855] "Robert A. Heinlein's The Puppet Masters"                                    
##  [856] "I Confess"                                                                  
##  [857] "Searchers, The"                                                             
##  [858] "Outlaw Josey Wales, The"                                                    
##  [859] "Rocketship X-M"                                                             
##  [860] "Verdict, The"                                                               
##  [861] "Place in the Sun, A"                                                        
##  [862] "Decline of Western Civilization, The"                                       
##  [863] "Lucie Aubrac"                                                               
##  [864] "Shaft"                                                                      
##  [865] "Tigger Movie, The"                                                          
##  [866] "Abyss, The"                                                                 
##  [867] "Gone with the Wind"                                                         
##  [868] "Dorado, El"                                                                 
##  [869] "Hoosiers"                                                                   
##  [870] "Adventures of Rocky and Bullwinkle, The"                                    
##  [871] "Perfect Storm, The"                                                         
##  [872] "Midnight Express"                                                           
##  [873] "Line King: Al Hirschfeld, The"                                              
##  [874] "Flubber"                                                                    
##  [875] "Murphy's Romance"                                                           
##  [876] "Buck and the Preacher"                                                      
##  [877] "Wolf Man, The"                                                              
##  [878] "American History X"                                                         
##  [879] "Homeward Bound II: Lost in San Francisco"                                   
##  [880] "Run of the Country, The"                                                    
##  [881] "Sting, The"                                                                 
##  [882] "Sadness of Sex, The"                                                        
##  [883] "Other Voices, Other Rooms"                                                  
##  [884] "Gothic"                                                                     
##  [885] "Otello"                                                                     
##  [886] "Where the Money Is"                                                         
##  [887] "Truth About Cats & Dogs, The"                                               
##  [888] "East is East"                                                               
##  [889] "Outrageous Fortune"                                                         
##  [890] "Message in a Bottle"                                                        
##  [891] "Slam"                                                                       
##  [892] "On the Town"                                                                
##  [893] "One Little Indian"                                                          
##  [894] "Stealing Beauty"                                                            
##  [895] "That Darn Cat!"                                                             
##  [896] "Bring It On"                                                                
##  [897] "Maurice"                                                                    
##  [898] "Grapes of Wrath, The"                                                       
##  [899] "Other Sister, The"                                                          
##  [900] "Long Goodbye, The"                                                          
##  [901] "Steam: The Turkish Bath (Hamam)"                                            
##  [902] "Never Been Kissed"                                                          
##  [903] "Sum of Us, The"                                                             
##  [904] "Sleepers"                                                                   
##  [905] "Planet of the Apes"                                                         
##  [906] "Interview with the Vampire"                                                 
##  [907] "Dark Command"                                                               
##  [908] "Papillon"                                                                   
##  [909] "Twelve Monkeys"                                                             
##  [910] "Burglar"                                                                    
##  [911] "Volcano"                                                                    
##  [912] "Cop Land"                                                                   
##  [913] "Cabin Boy"                                                                  
##  [914] "Nadine"                                                                     
##  [915] "Eternity and a Day (Mia eoniotita ke mia mera )"                            
##  [916] "Loss of Sexual Innocence, The"                                              
##  [917] "Last Detail, The"                                                           
##  [918] "Dead Calm"                                                                  
##  [919] "Philadelphia Story, The"                                                    
##  [920] "East Palace West Palace (Dong gong xi gong)"                                
##  [921] "Children of the Damned"                                                     
##  [922] "Underneath, The"                                                            
##  [923] "Trial, The (Le Procès)"                                                     
##  [924] "High Fidelity"                                                              
##  [925] "Bull Durham"                                                                
##  [926] "Son of the Sheik, The"                                                      
##  [927] "August"                                                                     
##  [928] "Creature Comforts"                                                          
##  [929] "Misery"                                                                     
##  [930] "Client, The"                                                                
##  [931] "Defying Gravity"                                                            
##  [932] "Screwed"                                                                    
##  [933] "American Psycho"                                                            
##  [934] "Myth of Fingerprints, The"                                                  
##  [935] "Titus"                                                                      
##  [936] "Little Buddha"                                                              
##  [937] "Five Easy Pieces"                                                           
##  [938] "One False Move"                                                             
##  [939] "Jerk, The"                                                                  
##  [940] "Night Visitor, The"                                                         
##  [941] "Perils of Pauline, The"                                                     
##  [942] "Shopping"                                                                   
##  [943] "Outsiders, The"                                                             
##  [944] "Cold Fever (Á köldum klaka)"                                                
##  [945] "Big Night"                                                                  
##  [946] "Secret Garden, The"                                                         
##  [947] "Ghost of Frankenstein, The"                                                 
##  [948] "Wrong Trousers, The"                                                        
##  [949] "One Fine Day"                                                               
##  [950] "Indiana Jones and the Last Crusade"                                         
##  [951] "Henry V"                                                                    
##  [952] "Siege, The"                                                                 
##  [953] "Shadow of a Doubt"                                                          
##  [954] "Bloodsport"                                                                 
##  [955] "Eyes of Laura Mars"                                                         
##  [956] "Good Earth, The"                                                            
##  [957] "Good Morning, Vietnam"                                                      
##  [958] "Grumpy Old Men"                                                             
##  [959] "Guess Who's Coming to Dinner"                                               
##  [960] "Speechless"                                                                 
##  [961] "Sweet and Lowdown"                                                          
##  [962] "Bachelor, The"                                                              
##  [963] "Grifters, The"                                                              
##  [964] "Predator"                                                                   
##  [965] "Frankie Starlight"                                                          
##  [966] "First Wives Club, The"                                                      
##  [967] "Puppet Master II"                                                           
##  [968] "Wend Kuuni (God's Gift)"                                                    
##  [969] "Three Caballeros, The"                                                      
##  [970] "Payback"                                                                    
##  [971] "Tales from the Crypt Presents: Bordello of Blood"                           
##  [972] "Blazing Saddles"                                                            
##  [973] "Mouth to Mouth (Boca a boca)"                                               
##  [974] "That Old Feeling"                                                           
##  [975] "Pump Up the Volume"                                                         
##  [976] "October Sky"                                                                
##  [977] "Magnolia"                                                                   
##  [978] "High Art"                                                                   
##  [979] "Last of the Mohicans, The"                                                  
##  [980] "Just the Ticket"                                                            
##  [981] "Anatomy (Anatomie)"                                                         
##  [982] "Backdraft"                                                                  
##  [983] "Fisher King, The"                                                           
##  [984] "Very Natural Thing, A"                                                      
##  [985] "Rock, The"                                                                  
##  [986] "Hard Target"                                                                
##  [987] "Adventures in Babysitting"                                                  
##  [988] "Three Wishes"                                                               
##  [989] "Damsel in Distress, A"                                                      
##  [990] "My Dog Skip"                                                                
##  [991] "Stop Making Sense"                                                          
##  [992] "My Man Godfrey"                                                             
##  [993] "When the Cats Away (Chacun cherche son chat)"                               
##  [994] "Babes in Toyland"                                                           
##  [995] "Chain of Fools"                                                             
##  [996] "Drowning Mona"                                                              
##  [997] "Far and Away"                                                               
##  [998] "To Live (Huozhe)"                                                           
##  [999] "Convent, The (Convento, O)"                                                 
## [1000] "Tommy Boy"                                                                  
## [1001] "Circle of Friends"                                                          
## [1002] "Nénette et Boni"                                                            
## [1003] "Baby... Secret of the Lost Legend"                                          
## [1004] "For All Mankind"                                                            
## [1005] "Fools Rush In"                                                              
## [1006] "Blood Feast"                                                                
## [1007] "Midsummer Night's Dream, A"                                                 
## [1008] "Penitentiary"                                                               
## [1009] "Penitentiary II"                                                            
## [1010] "Lonely Are the Brave"                                                       
## [1011] "Big Trouble in Little China"                                                
## [1012] "Beefcake"                                                                   
## [1013] "And the Ship Sails On (E la nave va)"                                       
## [1014] "Indiana Jones and the Temple of Doom"                                       
## [1015] "Good, The Bad and The Ugly, The"                                            
## [1016] "Get Over It"                                                                
## [1017] "Rich Man's Wife, The"                                                       
## [1018] "Best Man, The"                                                              
## [1019] "Dances with Wolves"                                                         
## [1020] "Alien Escape"                                                               
## [1021] "Max Dugan Returns"                                                          
## [1022] "Where Eagles Dare"                                                          
## [1023] "Cool Hand Luke"                                                             
## [1024] "Star Wars: Episode VI - Return of the Jedi"                                 
## [1025] "Crazy in Alabama"                                                           
## [1026] "Three to Tango"                                                             
## [1027] "Faust"                                                                      
## [1028] "Body Shots"                                                                 
## [1029] "Men Cry Bullets"                                                            
## [1030] "Blame It on Rio"                                                            
## [1031] "Knock Off"                                                                  
## [1032] "Two Bits"                                                                   
## [1033] "Somewhere in Time"                                                          
## [1034] "Melvin and Howard"                                                          
## [1035] "Drunken Master (Zui quan)"                                                  
## [1036] "Hairspray"                                                                  
## [1037] "Professional, The (a.k.a. Leon: The Professional)"                          
## [1038] "Two Moon Juction"                                                           
## [1039] "Me Myself I"                                                                
## [1040] "Nightmare Before Christmas, The"                                            
## [1041] "Three Musketeers, The"                                                      
## [1042] "Tombstone"                                                                  
## [1043] "Killing Fields, The"                                                        
## [1044] "Destiny Turns on the Radio"                                                 
## [1045] "In God's Hands"                                                             
## [1046] "French Connection, The"                                                     
## [1047] "Man of the Year"                                                            
## [1048] "Crimes of the Heart"                                                        
## [1049] "Local Hero"                                                                 
## [1050] "Star Maker, The (Uomo delle stelle, L')"                                    
## [1051] "Moonstruck"                                                                 
## [1052] "Last Emperor, The"                                                          
## [1053] "Native Son"                                                                 
## [1054] "Nobody's Fool"                                                              
## [1055] "Cool Runnings"                                                              
## [1056] "42 Up"                                                                      
## [1057] "Foxfire"                                                                    
## [1058] "Nico Icon"                                                                  
## [1059] "Vie est belle, La (Life is Rosey)"                                          
## [1060] "Source, The"                                                                
## [1061] "Spirits of the Dead (Tre Passi nel Delirio)"                                
## [1062] "Fast Times at Ridgemont High"                                               
## [1063] "Born to Win"                                                                
## [1064] "Fantastic Planet, The (La Planète sauvage)"                                 
## [1065] "Producers, The"                                                             
## [1066] "Friday the 13th Part VI: Jason Lives"                                       
## [1067] "Unforgiven"                                                                 
## [1068] "Lawnmower Man, The"                                                         
## [1069] "Simpatico"                                                                  
## [1070] "Can't Hardly Wait"                                                          
## [1071] "Wirey Spindell"                                                             
## [1072] "Another Man's Poison"                                                       
## [1073] "Full Tilt Boogie"                                                           
## [1074] "Fly, The"                                                                   
## [1075] "Santa Claus: The Movie"                                                     
## [1076] "Goonies, The"                                                               
## [1077] "Sixth Man, The"                                                             
## [1078] "Turbulence"                                                                 
## [1079] "Snow Falling on Cedars"                                                     
## [1080] "Caligula"                                                                   
## [1081] "Firestarter"                                                                
## [1082] "Fright Night"                                                               
## [1083] "Innocent Sleep, The"                                                        
## [1084] "Traveller"                                                                  
## [1085] "League of Their Own, A"                                                     
## [1086] "Falling in Love Again"                                                      
## [1087] "Bananas"                                                                    
## [1088] "Fright Night Part II"                                                       
## [1089] "Excalibur"                                                                  
## [1090] "Guns of Navarone, The"                                                      
## [1091] "Joe's Apartment"                                                            
## [1092] "McHale's Navy"                                                              
## [1093] "My Name Is Joe"                                                             
## [1094] "Chopping Mall (a.k.a. Killbots)"                                            
## [1095] "Barefoot Executive, The"                                                    
## [1096] "SubUrbia"                                                                   
## [1097] "Drive Me Crazy"                                                             
## [1098] "Return to Paradise"                                                         
## [1099] "Seven Beauties (Pasqualino Settebellezze)"                                  
## [1100] "Badlands"                                                                   
## [1101] "Battleship Potemkin, The (Bronenosets Potyomkin)"                           
## [1102] "Broadcast News"                                                             
## [1103] "Torn Curtain"                                                               
## [1104] "Muriel's Wedding"                                                           
## [1105] "Do the Right Thing"                                                         
## [1106] "Bride of Chucky"                                                            
## [1107] "Disclosure"                                                                 
## [1108] "Superstar"                                                                  
## [1109] "Baby-Sitters Club, The"                                                     
## [1110] "Death Wish II"                                                              
## [1111] "Death Wish 3"                                                               
## [1112] "Devil's Brigade, The"                                                       
## [1113] "Dracula"                                                                    
## [1114] "Death Wish 4: The Crackdown"                                                
## [1115] "Death Wish V: The Face of Death"                                            
## [1116] "Young Doctors in Love"                                                      
## [1117] "Teenage Mutant Ninja Turtles"                                               
## [1118] "We're No Angels"                                                            
## [1119] "City, The"                                                                  
## [1120] "My Life as a Dog (Mitt liv som hund)"                                       
## [1121] "If...."                                                                     
## [1122] "Suicide Kings"                                                              
## [1123] "3 Ninjas: High Noon On Mega Mountain"                                       
## [1124] "G. I. Blues"                                                                
## [1125] "Adrenalin: Fear the Rush"                                                   
## [1126] "Slumber Party Massacre II, The"                                             
## [1127] "7th Voyage of Sinbad, The"                                                  
## [1128] "Piranha"                                                                    
## [1129] "13th Warrior, The"                                                          
## [1130] "Nadja"                                                                      
## [1131] "Love and Basketball"                                                        
## [1132] "Phantom Love (Ai No Borei)"                                                 
## [1133] "Coyote Ugly"                                                                
## [1134] "Shanghai Noon"                                                              
## [1135] "Stacy's Knights"                                                            
## [1136] "Cemetery Man (Dellamorte Dellamore)"                                        
## [1137] "Minus Man, The"                                                             
## [1138] "Whole Nine Yards, The"                                                      
## [1139] "Shawshank Redemption, The"                                                  
## [1140] "Flawless"                                                                   
## [1141] "Communion (a.k.a. Alice, Sweet Alice/Holy Terror)"                          
## [1142] "Mad Max Beyond Thunderdome"                                                 
## [1143] "Photographer (Fotoamator)"                                                  
## [1144] "Life and Times of Hank Greenberg, The"                                      
## [1145] "Bye-Bye"                                                                    
## [1146] "Pretty in Pink"                                                             
## [1147] "Lightning Jack"                                                             
## [1148] "Analyze This"                                                               
## [1149] "Flashdance"                                                                 
## [1150] "Mortal Kombat"                                                              
## [1151] "Out-of-Towners, The"                                                        
## [1152] "Emerald Forest, The"                                                        
## [1153] "War at Home, The"                                                           
## [1154] "Boricua's Bond"                                                             
## [1155] "Last September, The"                                                        
## [1156] "Pokémon the Movie 2000"                                                     
## [1157] "Coneheads"                                                                  
## [1158] "Singin' in the Rain"                                                        
## [1159] "Radioland Murders"                                                          
## [1160] "Star Wars: Episode I - The Phantom Menace"                                  
## [1161] "Howling, The"                                                               
## [1162] "Just Cause"                                                                 
## [1163] "Pulp Fiction"                                                               
## [1164] "Fitzcarraldo"                                                               
## [1165] "Elephant Man, The"                                                          
## [1166] "Missing in Action"                                                          
## [1167] "One Crazy Summer"                                                           
## [1168] "Tokyo Fist"                                                                 
## [1169] "Heat"                                                                       
## [1170] "Stag"                                                                       
## [1171] "Heathers"                                                                   
## [1172] "Children Are Watching us, The (Bambini ci guardano, I)"                     
## [1173] "Robocop"                                                                    
## [1174] "Living Out Loud"                                                            
## [1175] "Pork Chop Hill"                                                             
## [1176] "Boat, The (Das Boot)"                                                       
## [1177] "Dear God"                                                                   
## [1178] "Being John Malkovich"                                                       
## [1179] "Naked Man, The"                                                             
## [1180] "Mad Love"                                                                   
## [1181] "Whatever It Takes"                                                          
## [1182] "Truce, The"                                                                 
## [1183] "Sandpiper, The"                                                             
## [1184] "Full Speed"                                                                 
## [1185] "Cool Dry Place, A"                                                          
## [1186] "S.F.W."                                                                     
## [1187] "What Lies Beneath"                                                          
## [1188] "Drugstore Cowboy"                                                           
## [1189] "Jury Duty"                                                                  
## [1190] "Mars Attacks!"                                                              
## [1191] "March of the Wooden Soldiers (a.k.a. Laurel & Hardy in Toyland)"            
## [1192] "Harvey"                                                                     
## [1193] "Strange Days"                                                               
## [1194] "Hard Rain"                                                                  
## [1195] "Half Baked"                                                                 
## [1196] "Halloween"                                                                  
## [1197] "Slumber Party Massacre III, The"                                            
## [1198] "Night Tide"                                                                 
## [1199] "Zed & Two Noughts, A"                                                       
## [1200] "Child's Play"                                                               
## [1201] "Midnight Dancers (Sibak)"                                                   
## [1202] "Somebody to Love"                                                           
## [1203] "Crow: Salvation, The"                                                       
## [1204] "Stir of Echoes"                                                             
## [1205] "Where the Heart Is"                                                         
## [1206] "Everyone Says I Love You"                                                   
## [1207] "Who's Afraid of Virginia Woolf?"                                            
## [1208] "Age of Innocence, The"                                                      
## [1209] "Girl, Interrupted"                                                          
## [1210] "Machine, The"                                                               
## [1211] "For the Moment"                                                             
## [1212] "Body Snatchers"                                                             
## [1213] "Days of Thunder"                                                            
## [1214] "Mr. Smith Goes to Washington"                                               
## [1215] "House"                                                                      
## [1216] "Baby, The"                                                                  
## [1217] "Next Stop, Wonderland"                                                      
## [1218] "Strike! (a.k.a. All I Wanna Do, The Hairy Bird)"                            
## [1219] "Foreign Student"                                                            
## [1220] "Moll Flanders"                                                              
## [1221] "Dead Zone, The"                                                             
## [1222] "301, 302"                                                                   
## [1223] "Home Alone"                                                                 
## [1224] "L.A. Confidential"                                                          
## [1225] "Yojimbo"                                                                    
## [1226] "I Married A Strange Person"                                                 
## [1227] "First Love, Last Rites"                                                     
## [1228] "Champ, The"                                                                 
## [1229] "Losing Isaiah"                                                              
## [1230] "Ipcress File, The"                                                          
## [1231] "On Any Sunday"                                                              
## [1232] "Underground"                                                                
## [1233] "Assignment, The"                                                            
## [1234] "Solaris (Solyaris)"                                                         
## [1235] "Gladiator"                                                                  
## [1236] "And God Created Woman (Et Dieu&#8230;Créa la Femme)"                        
## [1237] "American Dream"                                                             
## [1238] "Junk Mail"                                                                  
## [1239] "Carmen Miranda: Bananas Is My Business"                                     
## [1240] "Scream of Stone (Schrei aus Stein)"                                         
## [1241] "Apple Dumpling Gang, The"                                                   
## [1242] "Tora! Tora! Tora!"                                                          
## [1243] "Groove"                                                                     
## [1244] "Enchanted April"                                                            
## [1245] "Black Dog"                                                                  
## [1246] "American Movie"                                                             
## [1247] "True Crime"                                                                 
## [1248] "Sabrina"                                                                    
## [1249] "With Friends Like These..."                                                 
## [1250] "Tarzan the Fearless"                                                        
## [1251] "World Is Not Enough, The"                                                   
## [1252] "Battle of the Sexes, The"                                                   
## [1253] "Costa Brava"                                                                
## [1254] "Emma"                                                                       
## [1255] "Iron Eagle IV"                                                              
## [1256] "Star Is Born, A"                                                            
## [1257] "Eraserhead"                                                                 
## [1258] "Jerry Maguire"                                                              
## [1259] "Poison"                                                                     
## [1260] "Pacific Heights"                                                            
## [1261] "Unhook the Stars"                                                           
## [1262] "Hellhounds on My Trail"                                                     
## [1263] "Henry Fool"                                                                 
## [1264] "Van, The"                                                                   
## [1265] "Carpool"                                                                    
## [1266] "Death in Brunswick"                                                         
## [1267] "Tom & Viv"                                                                  
## [1268] "Secret of NIMH, The"                                                        
## [1269] "Sixteen Candles"                                                            
## [1270] "1-900"                                                                      
## [1271] "Air Bud: Golden Receiver"                                                   
## [1272] "Breakdown"                                                                  
## [1273] "Mr. Wonderful"                                                              
## [1274] "Palookaville"                                                               
## [1275] "Venice/Venice"                                                              
## [1276] "Downhill"                                                                   
## [1277] "Assault on Precinct 13"                                                     
## [1278] "Always Tell Your Wife"                                                      
## [1279] "Someone to Watch Over Me"                                                   
## [1280] "Sparrows"                                                                   
## [1281] "Fausto"                                                                     
## [1282] "Robocop 3"                                                                  
## [1283] "Man and a Woman, A (Un Homme et une Femme)"                                 
## [1284] "Cyrano de Bergerac"                                                         
## [1285] "Pushing Hands"                                                              
## [1286] "Here Comes Cookie"                                                          
## [1287] "Stuart Saves His Family"                                                    
## [1288] "Three Colors: Blue"                                                         
## [1289] "All the Vermeers in New York"                                               
## [1290] "Matilda"                                                                    
## [1291] "Jaws 2"                                                                     
## [1292] "For the Love of Benji"                                                      
## [1293] "Sonic Outlaws"                                                              
## [1294] "Brenda Starr"                                                               
## [1295] "Money Train"                                                                
## [1296] "Jackie Chan's First Strike"                                                 
## [1297] "Deep End of the Ocean, The"                                                 
## [1298] "Hook"                                                                       
## [1299] "Hoogste tijd"                                                               
## [1300] "Never Talk to Strangers"                                                    
## [1301] "Playing God"                                                                
## [1302] "Alaska"                                                                     
## [1303] "Spanking the Monkey"                                                        
## [1304] "Little Rascals, The"                                                        
## [1305] "Incognito"                                                                  
## [1306] "Hour of the Pig, The"                                                       
## [1307] "Widows' Peak"                                                               
## [1308] "Brown's Requiem"                                                            
## [1309] "Raiders of the Lost Ark"                                                    
## [1310] "Dark Half, The"                                                             
## [1311] "Psycho II"                                                                  
## [1312] "First Knight"                                                               
## [1313] "Mission: Impossible"                                                        
## [1314] "Superweib, Das"                                                             
## [1315] "Road to Wellville, The"                                                     
## [1316] "Now and Then"                                                               
## [1317] "City Hall"                                                                  
## [1318] "Associate, The (L'Associe"                                                  
## [1319] "Dangerous Beauty"                                                           
## [1320] "Tough and Deadly"                                                           
## [1321] "Third Man, The"                                                             
## [1322] "Alien"                                                                      
## [1323] "Army of Darkness"                                                           
## [1324] "Brother, Can You Spare a Dime?"                                             
## [1325] "Lay of the Land, The"                                                       
## [1326] "This Is Spinal Tap"                                                         
## [1327] "For Your Eyes Only"                                                         
## [1328] "Saboteur"                                                                   
## [1329] "Chungking Express"                                                          
## [1330] "Vermont Is For Lovers"                                                      
## [1331] "Elstree Calling"                                                            
## [1332] "Country Life"                                                               
## [1333] "Oliver!"                                                                    
## [1334] "Year of the Horse"                                                          
## [1335] "Shower (Xizhao)"                                                            
## [1336] "Bonfire of the Vanities"                                                    
## [1337] "Manhattan"                                                                  
## [1338] "Double Life of Veronique, The (La Double Vie de Véronique)"                 
## [1339] "Stand and Deliver"                                                          
## [1340] "No Mercy"                                                                   
## [1341] "Batman Returns"                                                             
## [1342] "Neon Bible, The"                                                            
## [1343] "Morning After, The"                                                         
## [1344] "Chinatown"                                                                  
## [1345] "Treasure of the Sierra Madre, The"                                          
## [1346] "Criminal Lovers (Les Amants Criminels)"                                     
## [1347] "Overnight Delivery"                                                         
## [1348] "Celebration, The (Festen)"                                                  
## [1349] "Primary Colors"                                                             
## [1350] "Retro Puppetmaster"                                                         
## [1351] "Breakfast Club, The"                                                        
## [1352] "Kids of Survival"                                                           
## [1353] "Jaws"                                                                       
## [1354] "Naked Gun 2 1/2: The Smell of Fear, The"                                    
## [1355] "Cry in the Dark, A"                                                         
## [1356] "Runaway"                                                                    
## [1357] "Sinbad and the Eye of the Tiger"                                            
## [1358] "Home Page"                                                                  
## [1359] "Bedknobs and Broomsticks"                                                   
## [1360] "Living Dead Girl, The (La Morte Vivante)"                                   
## [1361] "King Kong"                                                                  
## [1362] "Dolores Claiborne"                                                          
## [1363] "Bicycle Thief, The (Ladri di biciclette)"                                   
## [1364] "Kagemusha"                                                                  
## [1365] "Cat from Outer Space, The"                                                  
## [1366] "Raising Arizona"                                                            
## [1367] "Marathon Man"                                                               
## [1368] "Blood For Dracula (Andy Warhol's Dracula)"                                  
## [1369] "Swimming with Sharks"                                                       
## [1370] "Tea with Mussolini"                                                         
## [1371] "Requiem for a Dream"                                                        
## [1372] "Down to You"                                                                
## [1373] "Craft, The"                                                                 
## [1374] "Boys Life 2"                                                                
## [1375] "Saint, The"                                                                 
## [1376] "Scream"                                                                     
## [1377] "Not Love, Just Frenzy (Más que amor, frenesí)"                              
## [1378] "Highlander: Endgame"                                                        
## [1379] "Carrie"                                                                     
## [1380] "Glass Shield, The"                                                          
## [1381] "Idiots, The (Idioterne)"                                                    
## [1382] "Nosferatu (Nosferatu, eine Symphonie des Grauens)"                          
## [1383] "Evita"                                                                      
## [1384] "Twin Dragons (Shuang long hui)"                                             
## [1385] "Hudsucker Proxy, The"                                                       
## [1386] "Jude"                                                                       
## [1387] "Bad Company"                                                                
## [1388] "Starman"                                                                    
## [1389] "Moonraker"                                                                  
## [1390] "Last Time I Committed Suicide, The"                                         
## [1391] "Tetsuo II: Body Hammer"                                                     
## [1392] "Inside"                                                                     
## [1393] "Jeanne and the Perfect Guy (Jeanne et le garçon formidable)"                
## [1394] "Alligator"                                                                  
## [1395] "Angel Baby"                                                                 
## [1396] "Woman in Question, The"                                                     
## [1397] "Barcelona"                                                                  
## [1398] "Scent of a Woman"                                                           
## [1399] "Bird on a Wire"                                                             
## [1400] "Power 98"                                                                   
## [1401] "Very Brady Sequel, A"                                                       
## [1402] "Face/Off"                                                                   
## [1403] "Grosse Pointe Blank"                                                        
## [1404] "Death in the Garden (Mort en ce jardin, La)"                                
## [1405] "Beverly Hills Ninja"                                                        
## [1406] "Metro"                                                                      
## [1407] "White Men Can't Jump"                                                       
## [1408] "Gloria"                                                                     
## [1409] "Leopard Son, The"                                                           
## [1410] "Crude Oasis, The"                                                           
## [1411] "A Chef in Love"                                                             
## [1412] "Manhattan Murder Mystery"                                                   
## [1413] "Calendar Girl"                                                              
## [1414] "Gridlock'd"                                                                 
## [1415] "Mariachi, El"                                                               
## [1416] "Old Man and the Sea, The"                                                   
## [1417] "Taxi Driver"                                                                
## [1418] "Day of the Beast, The (El Día de la bestia)"                                
## [1419] "Angel and the Badman"                                                       
## [1420] "Prisoner of the Mountains (Kavkazsky Plennik)"                              
## [1421] "Dave"                                                                       
## [1422] "Gun Shy"                                                                    
## [1423] "Amazing Panda Adventure, The"                                               
## [1424] "Strictly Ballroom"                                                          
## [1425] "Glory Daze"                                                                 
## [1426] "Browning Version, The"                                                      
## [1427] "Romeo Is Bleeding"                                                          
## [1428] "Five Wives, Three Secretaries and Me"                                       
## [1429] "Aristocats, The"                                                            
## [1430] "Major League: Back to the Minors"                                           
## [1431] "Hanging Garden, The"                                                        
## [1432] "Creepshow"                                                                  
## [1433] "I Got the Hook Up"                                                          
## [1434] "Children of Heaven, The (Bacheha-Ye Aseman)"                                
## [1435] "Godfather, The"                                                             
## [1436] "Resurrection Man"                                                           
## [1437] "Cats Don't Dance"                                                           
## [1438] "Boy Called Hate, A"                                                         
## [1439] "NeverEnding Story, The"                                                     
## [1440] "Charlie, the Lonesome Cougar"                                               
## [1441] "Before and After"                                                           
## [1442] "Nine Months"                                                                
## [1443] "Sour Grapes"                                                                
## [1444] "Solo"                                                                       
## [1445] "Butch Cassidy and the Sundance Kid"                                         
## [1446] "Newton Boys, The"                                                           
## [1447] "Fear and Loathing in Las Vegas"                                             
## [1448] "Doctor Dolittle"                                                            
## [1449] "Before Sunrise"                                                             
## [1450] "Ruby in Paradise"                                                           
## [1451] "Schindler's List"                                                           
## [1452] "Tarantula"                                                                  
## [1453] "Sleepaway Camp"                                                             
## [1454] "MacKenna's Gold"                                                            
## [1455] "Ballad of Narayama, The (Narayama Bushiko)"                                 
## [1456] "Talented Mr. Ripley, The"                                                   
## [1457] "Naked Gun 33 1/3: The Final Insult"                                         
## [1458] "Bringing Up Baby"                                                           
## [1459] "Liebelei"                                                                   
## [1460] "Rapture, The"                                                               
## [1461] "Paris, France"                                                              
## [1462] "Splendor in the Grass"                                                      
## [1463] "Bank Dick, The"                                                             
## [1464] "Hotel de Love"                                                              
## [1465] "Swing Kids"                                                                 
## [1466] "Miami Rhapsody"                                                             
## [1467] "Delta of Venus"                                                             
## [1468] "Halloween 5: The Revenge of Michael Myers"                                  
## [1469] "Corruptor, The"                                                             
## [1470] "Vanya on 42nd Street"                                                       
## [1471] "Quest for Fire"                                                             
## [1472] "Boys Don't Cry"                                                             
## [1473] "Blood In, Blood Out (a.k.a. Bound by Honor)"                                
## [1474] "Dreamscape"                                                                 
## [1475] "Melody Time"                                                                
## [1476] "Under Capricorn"                                                            
## [1477] "Eye for an Eye"                                                             
## [1478] "American Werewolf in Paris, An"                                             
## [1479] "Skin Game, The"                                                             
## [1480] "Sacco and Vanzetti (Sacco e Vanzetti)"                                      
## [1481] "Woo"                                                                        
## [1482] "Giant Gila Monster, The"                                                    
## [1483] "Insomnia"                                                                   
## [1484] "Babyfever"                                                                  
## [1485] "Dudley Do-Right"                                                            
## [1486] "Safe"                                                                       
## [1487] "Decline of Western Civilization Part II: The Metal Years, The"              
## [1488] "Caddyshack"                                                                 
## [1489] "Psycho Beach Party"                                                         
## [1490] "Scarlet Letter, The"                                                        
## [1491] "Class of Nuke 'Em High"                                                     
## [1492] "Toxic Avenger, The"                                                         
## [1493] "Phantasm III: Lord of the Dead"                                             
## [1494] "Picnic at Hanging Rock"                                                     
## [1495] "On Her Majesty's Secret Service"                                            
## [1496] "Airheads"                                                                   
## [1497] "Billy Madison"                                                              
## [1498] "Mrs. Doubtfire"                                                             
## [1499] "Philadelphia"                                                               
## [1500] "Princess Caraboo"                                                           
## [1501] "Flesh and Bone"                                                             
## [1502] "Cinderella"                                                                 
## [1503] "Watcher, The"                                                               
## [1504] "Mark of Zorro, The"                                                         
## [1505] "Some Like It Hot"                                                           
## [1506] "Grand Canyon"                                                               
## [1507] "Bio-Dome"                                                                   
## [1508] "School Daze"                                                                
## [1509] "Breaking Away"                                                              
## [1510] "Everything Relative"                                                        
## [1511] "Following"                                                                  
## [1512] "Robin Hood"                                                                 
## [1513] "My Blue Heaven"                                                             
## [1514] "Missing in Action 2: The Beginning"                                         
## [1515] "Braddock: Missing in Action III"                                            
## [1516] "Mating Habits of the Earthbound Human, The"                                 
## [1517] "Willow"                                                                     
## [1518] "About Last Night..."                                                        
## [1519] "Double Indemnity"                                                           
## [1520] "Hitch-Hiker, The"                                                           
## [1521] "Number Seventeen"                                                           
## [1522] "Wild Man Blues"                                                             
## [1523] "Juno and Paycock"                                                           
## [1524] "Promise, The (Versprechen, Das)"                                            
## [1525] "Idolmaker, The"                                                             
## [1526] "Waking the Dead"                                                            
## [1527] "Little Nemo: Adventures in Slumberland"                                     
## [1528] "Whatever Happened to Aunt Alice?"                                           
## [1529] "I'm the One That I Want"                                                    
## [1530] "Portraits Chinois"                                                          
## [1531] "Klute"                                                                      
## [1532] "Opportunists, The"                                                          
## [1533] "Bride of Re-Animator"                                                       
## [1534] "Bulworth"                                                                   
## [1535] "Peanuts - Die Bank zahlt alles"                                             
## [1536] "Kronos"                                                                     
## [1537] "Almost Heroes"                                                              
## [1538] "Fantasia 2000"                                                              
## [1539] "Maverick"                                                                   
## [1540] "Dream for an Insomniac"                                                     
## [1541] "Every Other Weekend"                                                        
## [1542] "Fabulous Baker Boys, The"                                                   
## [1543] "War Stories"                                                                
## [1544] "Maybe, Maybe Not (Bewegte Mann, Der)"                                       
## [1545] "Mrs. Parker and the Vicious Circle"                                         
## [1546] "House of Exorcism, The (La Casa dell'esorcismo)"                            
## [1547] "Garcu, Le"                                                                  
## [1548] "Homage"                                                                     
## [1549] "Duets"                                                                      
## [1550] "Urbania"                                                                    
## [1551] "Running Man, The"                                                           
## [1552] "Purple Rose of Cairo, The"                                                  
## [1553] "Lethal Weapon 3"                                                            
## [1554] "Magic Hunter"                                                               
## [1555] "Bulletproof"                                                                
## [1556] "Kaspar Hauser"                                                              
## [1557] "101 Dalmatians"                                                             
## [1558] "Lolita"                                                                     
## [1559] "Cutting Edge, The"                                                          
## [1560] "Murder!"                                                                    
## [1561] "They Made Me a Criminal"                                                    
## [1562] "Best Man, The (Il Testimone dello sposo)"                                   
## [1563] "Usual Suspects, The"                                                        
## [1564] "We're Back! A Dinosaur's Story"                                             
## [1565] "Fifth Element, The"                                                         
## [1566] "Cowboy Way, The"                                                            
## [1567] "Serpico"                                                                    
## [1568] "South Park: Bigger, Longer and Uncut"                                       
## [1569] "Son in Law"                                                                 
## [1570] "Love and Death on Long Island"                                              
## [1571] "Light of Day"                                                               
## [1572] "Eyes Without a Face"                                                        
## [1573] "Twister"                                                                    
## [1574] "Bossa Nova"                                                                 
## [1575] "Isn't She Great?"                                                           
## [1576] "Supernova"                                                                  
## [1577] "Quarry, The"                                                                
## [1578] "Creature"                                                                   
## [1579] "Way We Were, The"                                                           
## [1580] "Single White Female"                                                        
## [1581] "Beetlejuice"                                                                
## [1582] "Hideous Sun Demon, The"                                                     
## [1583] "Mummy, The"                                                                 
## [1584] "Romper Stomper"                                                             
## [1585] "Margaret's Museum"                                                          
## [1586] "Bliss"                                                                      
## [1587] "Duoluo tianshi"                                                             
## [1588] "Strangers on a Train"                                                       
## [1589] "Yankee Zulu"                                                                
## [1590] "Big Country, The"                                                           
## [1591] "Shattered Image"                                                            
## [1592] "Ayn Rand: A Sense of Life"                                                  
## [1593] "Reds"                                                                       
## [1594] "Black and White"                                                            
## [1595] "Loaded"                                                                     
## [1596] "Poison Ivy"                                                                 
## [1597] "Poison Ivy: New Seduction"                                                  
## [1598] "Mr. Holland's Opus"                                                         
## [1599] "Under the Domin Tree (Etz Hadomim Tafus)"                                   
## [1600] "Pelican Brief, The"                                                         
## [1601] "My Best Fiend (Mein liebster Feind)"                                        
## [1602] "Solar Crisis"                                                               
## [1603] "Oliver & Company"                                                           
## [1604] "Aces: Iron Eagle III"                                                       
## [1605] "H.O.T.S."                                                                   
## [1606] "Green Mile, The"                                                            
## [1607] "Knightriders"                                                               
## [1608] "Meet the Deedles"                                                           
## [1609] "Man with the Golden Arm, The"                                               
## [1610] "Cable Guy, The"                                                             
## [1611] "Ace Ventura: When Nature Calls"                                             
## [1612] "Dingo"                                                                      
## [1613] "Dear Jesse"                                                                 
## [1614] "Brain That Wouldn't Die, The"                                               
## [1615] "Armageddon"                                                                 
## [1616] "Killing Zoe"                                                                
## [1617] "Bambi"                                                                      
## [1618] "Stalag 17"                                                                  
## [1619] "Thin Man, The"                                                              
## [1620] "Dangerous Liaisons"                                                         
## [1621] "Live Virgin"                                                                
## [1622] "Bird of Prey"                                                               
## [1623] "Rocketeer, The"                                                             
## [1624] "War, The"                                                                   
## [1625] "Beloved/Friend (Amigo/Amado)"                                               
## [1626] "Hate (Haine, La)"                                                           
## [1627] "Steamboat Willie"                                                           
## [1628] "Charade"                                                                    
## [1629] "Big Daddy"                                                                  
## [1630] "Wings of Desire (Der Himmel über Berlin)"                                   
## [1631] "Invitation, The (Zaproszenie)"                                              
## [1632] "Six of a Kind"                                                              
## [1633] "Priest"                                                                     
## [1634] "Breathless"                                                                 
## [1635] "Assassination"                                                              
## [1636] "Last Supper, The"                                                           
## [1637] "Jeremiah Johnson"                                                           
## [1638] "Chain Reaction"                                                             
## [1639] "Iron Eagle II"                                                              
## [1640] "20 Dates"                                                                   
## [1641] "Specialist, The"                                                            
## [1642] "Love Is the Devil"                                                          
## [1643] "Prick Up Your Ears"                                                         
## [1644] "Salut cousin!"                                                              
## [1645] "Boys Life"                                                                  
## [1646] "Apple, The (Sib)"                                                           
## [1647] "Evening Star, The"                                                          
## [1648] "Police Academy 4: Citizens on Patrol"                                       
## [1649] "Lady and the Tramp"                                                         
## [1650] "Adventures of Pinocchio, The"                                               
## [1651] "Abbott and Costello Meet Frankenstein"                                      
## [1652] "American Pop"                                                               
## [1653] "It Happened Here"                                                           
## [1654] "In the Line of Duty 2"                                                      
## [1655] "Muse, The"                                                                  
## [1656] "Mortal Thoughts"                                                            
## [1657] "Die Hard: With a Vengeance"                                                 
## [1658] "Backbeat"                                                                   
## [1659] "Escape to Witch Mountain"                                                   
## [1660] "Mr. Magoo"                                                                  
## [1661] "Deadtime Stories"                                                           
## [1662] "Synthetic Pleasures"                                                        
## [1663] "Breakfast of Champions"                                                     
## [1664] "Great Race, The"                                                            
## [1665] "Bad Boys"                                                                   
## [1666] "Cabaret"                                                                    
## [1667] "Nurse Betty"                                                                
## [1668] "Pajama Game, The"                                                           
## [1669] "Kids in the Hall: Brain Candy"                                              
## [1670] "Romeo Must Die"                                                             
## [1671] "Harold and Maude"                                                           
## [1672] "Great Locomotive Chase, The"                                                
## [1673] "Taffin"                                                                     
## [1674] "Jack"                                                                       
## [1675] "'Til There Was You"                                                         
## [1676] "Kestrel's Eye (Falkens öga)"                                                
## [1677] "Kurt & Courtney"                                                            
## [1678] "All Quiet on the Western Front"                                             
## [1679] "Waiting Game, The"                                                          
## [1680] "Kids of the Round Table"                                                    
## [1681] "Killing of Sister George, The"                                              
## [1682] "Cronos"                                                                     
## [1683] "Cruel Intentions"                                                           
## [1684] "Born Yesterday"                                                             
## [1685] "Tingler, The"                                                               
## [1686] "Violets Are Blue..."                                                        
## [1687] "To Cross the Rubicon"                                                       
## [1688] "Last Resort"                                                                
## [1689] "Vampyros Lesbos (Las Vampiras)"                                             
## [1690] "Cat People"                                                                 
## [1691] "2001: A Space Odyssey"                                                      
## [1692] "He Got Game"                                                                
## [1693] "Foreign Correspondent"                                                      
## [1694] "Bitter Sugar (Azucar Amargo)"                                               
## [1695] "Sweet Nothing"                                                              
## [1696] "Deterrence"                                                                 
## [1697] "Howards End"                                                                
## [1698] "Dry Cleaning (Nettoyage à sec)"                                             
## [1699] "Operation Condor 2 (Longxiong hudi)"                                        
## [1700] "Vertigo"                                                                    
## [1701] "Sugarland Express, The"                                                     
## [1702] "Kentucky Fried Movie, The"                                                  
## [1703] "Palm Beach Story, The"                                                      
## [1704] "Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb"       
## [1705] "Teaching Mrs. Tingle"                                                       
## [1706] "Saludos Amigos"                                                             
## [1707] "Very Thought of You, The"                                                   
## [1708] "Chill Factor"                                                               
## [1709] "Outside Providence"                                                         
## [1710] "Bedrooms & Hallways"                                                        
## [1711] "I Love You, Don't Touch Me!"                                                
## [1712] "E.T. the Extra-Terrestrial"                                                 
## [1713] "Dead Presidents"                                                            
## [1714] "Buffy the Vampire Slayer"                                                   
## [1715] "Hard-Boiled (Lashou shentan)"                                               
## [1716] "Man Bites Dog (C'est arrivé près de chez vous)"                             
## [1717] "Wild Reeds"                                                                 
## [1718] "Help!"                                                                      
## [1719] "Dr. No"                                                                     
## [1720] "Get Bruce"                                                                  
## [1721] "To Die For"                                                                 
## [1722] "Drop Dead Fred"                                                             
## [1723] "Brazil"                                                                     
## [1724] "Ghost"                                                                      
## [1725] "Big Combo, The"                                                             
## [1726] "Conceiving Ada"                                                             
## [1727] "Spaceballs"                                                                 
## [1728] "Schlafes Bruder (Brother of Sleep)"                                         
## [1729] "Heidi Fleiss: Hollywood Madam"                                              
## [1730] "Detroit Rock City"                                                          
## [1731] "Night on Earth"                                                             
## [1732] "Pleasantville"                                                              
## [1733] "Son of Dracula"                                                             
## [1734] "Home Alone 3"                                                               
## [1735] "Joe Gould's Secret"                                                         
## [1736] "Taste of Cherry"                                                            
## [1737] "Quiz Show"                                                                  
## [1738] "Eye of Vichy, The (Oeil de Vichy, L')"                                      
## [1739] "Tarzan and the Lost City"                                                   
## [1740] "Phantoms"                                                                   
## [1741] "Footloose"                                                                  
## [1742] "Rain Man"                                                                   
## [1743] "Whipped"                                                                    
## [1744] "Repo Man"                                                                   
## [1745] "Central Station (Central do Brasil)"                                        
## [1746] "Immortal Beloved"                                                           
## [1747] "Amityville 3-D"                                                             
## [1748] "Dumbo"                                                                      
## [1749] "Great Escape, The"                                                          
## [1750] "Diva"                                                                       
## [1751] "Blood on the Sun"                                                           
## [1752] "One Tough Cop"                                                              
## [1753] "Perfect Murder, A"                                                          
## [1754] "Honeymoon in Vegas"                                                         
## [1755] "Carmen"                                                                     
## [1756] "Woman in the Dunes (Suna no onna)"                                          
## [1757] "Wolf"                                                                       
## [1758] "Bram Stoker's Dracula"                                                      
## [1759] "Michael"                                                                    
## [1760] "Smashing Time"                                                              
## [1761] "Santa Clause, The"                                                          
## [1762] "Night Shift"                                                                
## [1763] "Places in the Heart"                                                        
## [1764] "Band Wagon, The"                                                            
## [1765] "Hand That Rocks the Cradle, The"                                            
## [1766] "Gospa"                                                                      
## [1767] "Ruthless People"                                                            
## [1768] "Big Momma's House"                                                          
## [1769] "Gods Must Be Crazy II, The"                                                 
## [1770] "Last Temptation of Christ, The"                                             
## [1771] "Waiting for Guffman"                                                        
## [1772] "Of Mice and Men"                                                            
## [1773] "Wing Commander"                                                             
## [1774] "Mumford"                                                                    
## [1775] "Picture Perfect"                                                            
## [1776] "Beautician and the Beast, The"                                              
## [1777] "Intimate Relations"                                                         
## [1778] "Sex, Lies, and Videotape"                                                   
## [1779] "Farewell My Concubine"                                                      
## [1780] "Wild Things"                                                                
## [1781] "Male and Female"                                                            
## [1782] "Under Siege 2: Dark Territory"                                              
## [1783] "Medicine Man"                                                               
## [1784] "Spiders, The (Die Spinnen, 1. Teil: Der Goldene See)"                       
## [1785] "On the Ropes"                                                               
## [1786] "Rosie"                                                                      
## [1787] "Astronaut's Wife, The"                                                      
## [1788] "New Jersey Drive"                                                           
## [1789] "Cabaret Balkan (Bure Baruta)"                                               
## [1790] "Birds, The"                                                                 
## [1791] "Dog of Flanders, A"                                                         
## [1792] "Lost Son, The"                                                              
## [1793] "Fled"                                                                       
## [1794] "Mifune (Mifunes sidste sang)"                                               
## [1795] "Second Jungle Book: Mowgli & Baloo, The"                                    
## [1796] "Napoleon and Samantha"                                                      
## [1797] "Wedding Bell Blues"                                                         
## [1798] "Who Framed Roger Rabbit?"                                                   
## [1799] "One True Thing"                                                             
## [1800] "King of New York"                                                           
## [1801] "U.S. Marshalls"                                                             
## [1802] "Welcome to Woop-Woop"                                                       
## [1803] "Karate Kid III, The"                                                        
## [1804] "Glen or Glenda"                                                             
## [1805] "Strawberry and Chocolate (Fresa y chocolate)"                               
## [1806] "Simply Irresistible"                                                        
## [1807] "View to a Kill, A"                                                          
## [1808] "Pagemaster, The"                                                            
## [1809] "Ghost Dog: The Way of the Samurai"                                          
## [1810] "Year My Voice Broke, The"                                                   
## [1811] "Gentleman's Agreement"                                                      
## [1812] "On the Waterfront"                                                          
## [1813] "Edge of Seventeen"                                                          
## [1814] "Mummy's Curse, The"                                                         
## [1815] "Ran"                                                                        
## [1816] "Raw Deal"                                                                   
## [1817] "And Now for Something Completely Different"                                 
## [1818] "Mole People, The"                                                           
## [1819] "Month by the Lake, A"                                                       
## [1820] "Persuasion"                                                                 
## [1821] "Condition Red"                                                              
## [1822] "Until the End of the World (Bis ans Ende der Welt)"                         
## [1823] "Love Bug, The"                                                              
## [1824] "Pollyanna"                                                                  
## [1825] "Terms of Endearment"                                                        
## [1826] "Out of Africa"                                                              
## [1827] "Don't Be a Menace to South Central While Drinking Your Juice in the Hood"   
## [1828] "Held Up"                                                                    
## [1829] "Star Trek: The Wrath of Khan"                                               
## [1830] "Paris Was a Woman"                                                          
## [1831] "Marked for Death"                                                           
## [1832] "Terror in a Texas Town"                                                     
## [1833] "Duck Soup"                                                                  
## [1834] "Amityville Curse, The"                                                      
## [1835] "Possession"                                                                 
## [1836] "Best Men"                                                                   
## [1837] "Jungle2Jungle (a.k.a. Jungle 2 Jungle)"                                     
## [1838] "Private Parts"                                                              
## [1839] "Burnt Offerings"                                                            
## [1840] "Beautiful Thing"                                                            
## [1841] "Mother"                                                                     
## [1842] "Turbo: A Power Rangers Movie"                                               
## [1843] "Anna Karenina"                                                              
## [1844] "Splendor"                                                                   
## [1845] "Love's Labour's Lost"                                                       
## [1846] "Kolya"                                                                      
## [1847] "Three Lives and Only One Death"                                             
## [1848] "Exorcist II: The Heretic"                                                   
## [1849] "Monty Python's Life of Brian"                                               
## [1850] "Blue Angel, The (Blaue Engel, Der)"                                         
## [1851] "Und keiner weint mir nach"                                                  
## [1852] "Harlem"                                                                     
## [1853] "Gold Diggers: The Secret of Bear Mountain"                                  
## [1854] "I Saw What You Did"                                                         
## [1855] "Hero"                                                                       
## [1856] "Tales from the Hood"                                                        
## [1857] "Kim"                                                                        
## [1858] "Feeling Minnesota"                                                          
## [1859] "Nil By Mouth"                                                               
## [1860] "Bonnie and Clyde"                                                           
## [1861] "Halloween: H20"                                                             
## [1862] "Promise, The (La Promesse)"                                                 
## [1863] "Witness"                                                                    
## [1864] "Naturally Native"                                                           
## [1865] "Graveyard Shift"                                                            
## [1866] "Designated Mourner, The"                                                    
## [1867] "Maximum Overdrive"                                                          
## [1868] "Free Willy 2: The Adventure Home"                                           
## [1869] "No Small Affair"                                                            
## [1870] "Life Is Beautiful (La Vita è bella)"                                        
## [1871] "Butcher's Wife, The"                                                        
## [1872] "Castle Freak"                                                               
## [1873] "Ape, The"                                                                   
## [1874] "Music of the Heart"                                                         
## [1875] "Man of the Century"                                                         
## [1876] "Young and Innocent"                                                         
## [1877] "Ronin"                                                                      
## [1878] "Johnny Mnemonic"                                                            
## [1879] "Daddy Long Legs"                                                            
## [1880] "Extremities"                                                                
## [1881] "Glass Bottom Boat, The"                                                     
## [1882] "Scrooged"                                                                   
## [1883] "White Sands"                                                                
## [1884] "My Favorite Martian"                                                        
## [1885] "Leaving Las Vegas"                                                          
## [1886] "Nutty Professor II: The Klumps"                                             
## [1887] "Topsy-Turvy"                                                                
## [1888] "Hunted, The"                                                                
## [1889] "Pet Sematary"                                                               
## [1890] "Babe: Pig in the City"                                                      
## [1891] "Hav Plenty"                                                                 
## [1892] "House Party 3"                                                              
## [1893] "Bronco Billy"                                                               
## [1894] "Love Walked In"                                                             
## [1895] "Happy Go Lovely"                                                            
## [1896] "Platoon"                                                                    
## [1897] "Beautiful"                                                                  
## [1898] "Conquest of the Planet of the Apes"                                         
## [1899] "M. Butterfly"                                                               
## [1900] "Mighty Peking Man (Hsing hsing wang)"                                       
## [1901] "Muppet Movie, The"                                                          
## [1902] "Associate, The"                                                             
## [1903] "Naked"                                                                      
## [1904] "Taxman"                                                                     
## [1905] "With Honors"                                                                
## [1906] "EDtv"                                                                       
## [1907] "Talking About Sex"                                                          
## [1908] "Aladdin"                                                                    
## [1909] "Superman II"                                                                
## [1910] "Brokedown Palace"                                                           
## [1911] "Eyes Wide Shut"                                                             
## [1912] "Raise the Red Lantern"                                                      
## [1913] "Fantasia"                                                                   
## [1914] "Boomerang"                                                                  
## [1915] "Anne Frank Remembered"                                                      
## [1916] "Brothers in Trouble"                                                        
## [1917] "Hellraiser: Bloodline"                                                      
## [1918] "Runaway Bride"                                                              
## [1919] "Terminal Velocity"                                                          
## [1920] "M*A*S*H"                                                                    
## [1921] "Pink Floyd - The Wall"                                                      
## [1922] "Farmer & Chase"                                                             
## [1923] "Trust"                                                                      
## [1924] "Young Poisoner's Handbook, The"                                             
## [1925] "Asfour Stah"                                                                
## [1926] "Bob Roberts"                                                                
## [1927] "Grosse Fatigue"                                                             
## [1928] "Parent Trap, The"                                                           
## [1929] "Miller's Crossing"                                                          
## [1930] "Jar, The (Khomreh)"                                                         
## [1931] "Substitute, The"                                                            
## [1932] "Bachelor Party"                                                             
## [1933] "Christmas Story, A"                                                         
## [1934] "From Dusk Till Dawn"                                                        
## [1935] "Hype!"                                                                      
## [1936] "Young Guns II"                                                              
## [1937] "Swiss Family Robinson"                                                      
## [1938] "Target"                                                                     
## [1939] "Better Off Dead..."                                                         
## [1940] "Amityville II: The Possession"                                              
## [1941] "Captives"                                                                   
## [1942] "In Love and War"                                                            
## [1943] "Groundhog Day"                                                              
## [1944] "Believers, The"                                                             
## [1945] "Loser"                                                                      
## [1946] "Gossip"                                                                     
## [1947] "Limelight"                                                                  
## [1948] "Awfully Big Adventure, An"                                                  
## [1949] "Birdcage, The"                                                              
## [1950] "Long Kiss Goodnight, The"                                                   
## [1951] "Poltergeist II: The Other Side"                                             
## [1952] "Flintstones in Viva Rock Vegas, The"                                        
## [1953] "Crash"                                                                      
## [1954] "Nosferatu a Venezia"                                                        
## [1955] "Portrait of a Lady, The"                                                    
## [1956] "Shadow of Angels (Schatten der Engel)"                                      
## [1957] "Watership Down"                                                             
## [1958] "Action Jackson"                                                             
## [1959] "Contempt (Le Mépris)"                                                       
## [1960] "For Ever Mozart"                                                            
## [1961] "Nick of Time"                                                               
## [1962] "Prefontaine"                                                                
## [1963] "Farmer's Wife, The"                                                         
## [1964] "Dog Park"                                                                   
## [1965] "Truth or Consequences, N.M."                                                
## [1966] "Blue Lagoon, The"                                                           
## [1967] "Hard 8 (a.k.a. Sydney, a.k.a. Hard Eight)"                                  
## [1968] "Tall Tale"                                                                  
## [1969] "Five Senses, The"                                                           
## [1970] "Get Carter"                                                                 
## [1971] "Forrest Gump"                                                               
## [1972] "She's the One"                                                              
## [1973] "Law, The (Le Legge)"                                                        
## [1974] "Little Princess, The"                                                       
## [1975] "Mary Reilly"                                                                
## [1976] "Map of the World, A"                                                        
## [1977] "Adventures of Robin Hood, The"                                              
## [1978] "Echte Kerle"                                                                
## [1979] "Avengers, The"                                                              
## [1980] "Monkey Shines"                                                              
## [1981] "Hatchet For the Honeymoon (Rosso Segno Della Follia)"                       
## [1982] "Gattaca"                                                                    
## [1983] "Bustin' Loose"                                                              
## [1984] "Next Friday"                                                                
## [1985] "Hurricane, The"                                                             
## [1986] "When Night Is Falling"                                                      
## [1987] "Raise the Titanic"                                                          
## [1988] "Twin Falls Idaho"                                                           
## [1989] "Crooklyn"                                                                   
## [1990] "Old Lady Who Walked in the Sea, The (Vieille qui marchait dans la mer, La)" 
## [1991] "Daylight"                                                                   
## [1992] "Sling Blade"                                                                
## [1993] "City of the Living Dead (Paura nella città dei morti viventi)"              
## [1994] "Pleasure Garden, The"                                                       
## [1995] "Guardian, The"                                                              
## [1996] "Vampires"                                                                   
## [1997] "Invisible Man, The"                                                         
## [1998] "Suddenly, Last Summer"                                                      
## [1999] "Fall Time"                                                                  
## [2000] "Saving Grace"                                                               
## [2001] "Grand Hotel"                                                                
## [2002] "Hellbound: Hellraiser II"                                                   
## [2003] "Kika"                                                                       
## [2004] "Piano, The"                                                                 
## [2005] "Ben-Hur"                                                                    
## [2006] "Airplane!"                                                                  
## [2007] "Friday"                                                                     
## [2008] "Here on Earth"                                                              
## [2009] "Mickey Blue Eyes"                                                           
## [2010] "Juror, The"                                                                 
## [2011] "Vampire in Brooklyn"                                                        
## [2012] "Affair to Remember, An"                                                     
## [2013] "Kansas City"                                                                
## [2014] "Love in the Afternoon"                                                      
## [2015] "Polish Wedding"                                                             
## [2016] "Mondo"                                                                      
## [2017] "Your Friends and Neighbors"                                                 
## [2018] "Top Gun"                                                                    
## [2019] "Get Real"                                                                   
## [2020] "Destination Moon"                                                           
## [2021] "Set It Off"                                                                 
## [2022] "8 Seconds"                                                                  
## [2023] "Judgment Night"                                                             
## [2024] "Bikini Beach"                                                               
## [2025] "Eddie"                                                                      
## [2026] "Sunset Park"                                                                
## [2027] "Dunston Checks In"                                                          
## [2028] "Gay Divorcee, The"                                                          
## [2029] "Casablanca"                                                                 
## [2030] "Bad Seed, The"                                                              
## [2031] "Dumb & Dumber"                                                              
## [2032] "Living in Oblivion"                                                         
## [2033] "Grandfather, The (El Abuelo)"                                               
## [2034] "Kicked in the Head"                                                         
## [2035] "Roommates"                                                                  
## [2036] "F/X 2"                                                                      
## [2037] "Children of Paradise (Les enfants du paradis)"                              
## [2038] "Steel Magnolias"                                                            
## [2039] "Man of No Importance, A"                                                    
## [2040] "I Still Know What You Did Last Summer"                                      
## [2041] "Soldier's Daughter Never Cries, A"                                          
## [2042] "Urban Legend"                                                               
## [2043] "Last Night"                                                                 
## [2044] "Paulie"                                                                     
## [2045] "Rugrats Movie, The"                                                         
## [2046] "My Life in Pink (Ma vie en rose)"                                           
## [2047] "Opposite of Sex, The"                                                       
## [2048] "Bicentennial Man"                                                           
## [2049] "Police Academy 2: Their First Assignment"                                   
## [2050] "Children of the Corn II: The Final Sacrifice"                               
## [2051] "X-Files: Fight the Future, The"                                             
## [2052] "Cup, The (Phörpa)"                                                          
## [2053] "Relative Fear"                                                              
## [2054] "Out of Sight"                                                               
## [2055] "Pocahontas"                                                                 
## [2056] "Wings"                                                                      
## [2057] "Nô"                                                                         
## [2058] "Singles"                                                                    
## [2059] "Man Without a Face, The"                                                    
## [2060] "Boondock Saints, The"                                                       
## [2061] "Drunks"                                                                     
## [2062] "Celluloid Closet, The"                                                      
## [2063] "Lawrence of Arabia"                                                         
## [2064] "Fear"                                                                       
## [2065] "Tarantella"                                                                 
## [2066] "North Dallas Forty"                                                         
## [2067] "Ghost in the Shell (Kokaku kidotai)"                                        
## [2068] "Final Conflict, The (a.k.a. Omen III: The Final Conflict)"                  
## [2069] "European Vacation"                                                          
## [2070] "Right Stuff, The"                                                           
## [2071] "Two or Three Things I Know About Her"                                       
## [2072] "Careful"                                                                    
## [2073] "King of Marvin Gardens, The"                                                
## [2074] "Kill, Baby... Kill! (Operazione Paura)"                                     
## [2075] "Crow, The"                                                                  
## [2076] "Grease 2"                                                                   
## [2077] "Bad Taste"                                                                  
## [2078] "Marvin's Room"                                                              
## [2079] "Tin Cup"                                                                    
## [2080] "Crows and Sparrows"                                                         
## [2081] "Poltergeist"                                                                
## [2082] "Small Faces"                                                                
## [2083] "Night of the Creeps"                                                        
## [2084] "Some Mother's Son"                                                          
## [2085] "Predator 2"                                                                 
## [2086] "Nutty Professor, The"                                                       
## [2087] "Murder, My Sweet"                                                           
## [2088] "Muppet Treasure Island"                                                     
## [2089] "Willy Wonka and the Chocolate Factory"                                      
## [2090] "Dune"                                                                       
## [2091] "In the Bleak Midwinter"                                                     
## [2092] "Return to Oz"                                                               
## [2093] "Event Horizon"                                                              
## [2094] "Candleshoe"                                                                 
## [2095] "Casino"                                                                     
## [2096] "Asylum"                                                                     
## [2097] "Affair of Love, An (Une Liaison Pornographique)"                            
## [2098] "Blue Collar"                                                                
## [2099] "Goodbye, Lover"                                                             
## [2100] "Happiness Is in the Field"                                                  
## [2101] "Who's Harry Crumb?"                                                         
## [2102] "Buddy Boy"                                                                  
## [2103] "Hillbillys in a Haunted House"                                              
## [2104] "Cruise, The"                                                                
## [2105] "Benji"                                                                      
## [2106] "D3: The Mighty Ducks"                                                       
## [2107] "Carnosaur 2"                                                                
## [2108] "Cobra"                                                                      
## [2109] "Conspiracy Theory"                                                          
## [2110] "Plan 9 from Outer Space"                                                    
## [2111] "Vacation"                                                                   
## [2112] "Light Years"                                                                
## [2113] "Tao of Steve, The"                                                          
## [2114] "Everything You Always Wanted to Know About Sex"                             
## [2115] "Tomb of Ligeia, The"                                                        
## [2116] "Close Encounters of the Third Kind"                                         
## [2117] "Big Tease, The"                                                             
## [2118] "I Am Cuba (Soy Cuba/Ya Kuba)"                                               
## [2119] "Thomas Crown Affair, The"                                                   
## [2120] "Fox and the Hound, The"                                                     
## [2121] "Butterfly (La Lengua de las Mariposas)"                                     
## [2122] "Die Hard"                                                                   
## [2123] "Cutter's Way"                                                               
## [2124] "Lord of the Rings, The"                                                     
## [2125] "Cell, The"                                                                  
## [2126] "Glimmer Man, The"                                                           
## [2127] "Washington Square"                                                          
## [2128] "Suburbans, The"                                                             
## [2129] "Rough Night in Jericho"                                                     
## [2130] "Wayne's World"                                                              
## [2131] "Smoke"                                                                      
## [2132] "Bear, The"                                                                  
## [2133] "Snow White and the Seven Dwarfs"                                            
## [2134] "Killer, The (Die xue shuang xiong)"                                         
## [2135] "Trick"                                                                      
## [2136] "Midnight Cowboy"                                                            
## [2137] "Woman of Paris, A"                                                          
## [2138] "Fierce Creatures"                                                           
## [2139] "Twelfth Night"                                                              
## [2140] "Mother Night"                                                               
## [2141] "Switchback"                                                                 
## [2142] "Mute Witness"                                                               
## [2143] "Hi-Yo Silver"                                                               
## [2144] "Belle de jour"                                                              
## [2145] "Shooting Fish"                                                              
## [2146] "Days of Heaven"                                                             
## [2147] "It Takes Two"                                                               
## [2148] "At First Sight"                                                             
## [2149] "Easy Rider"                                                                 
## [2150] "Goofy Movie, A"                                                             
## [2151] "Zero Effect"                                                                
## [2152] "Rushmore"                                                                   
## [2153] "Soul Man"                                                                   
## [2154] "Trial by Jury"                                                              
## [2155] "Still Crazy"                                                                
## [2156] "Closer You Get, The"                                                        
## [2157] "Jean de Florette"                                                           
## [2158] "Big Blue, The (Le Grand Bleu)"                                              
## [2159] "He Walked by Night"                                                         
## [2160] "Mina Tannenbaum"                                                            
## [2161] "Unforgotten: Twenty-Five Years After Willowbrook"                           
## [2162] "Nueba Yol"                                                                  
## [2163] "Fish Called Wanda, A"                                                       
## [2164] "Wes Craven's New Nightmare"                                                 
## [2165] "Addiction, The"                                                             
## [2166] "Life Less Ordinary, A"                                                      
## [2167] "Jakob the Liar"                                                             
## [2168] "Spawn"                                                                      
## [2169] "In the Company of Men"                                                      
## [2170] "Hugo Pool"                                                                  
## [2171] "One Night Stand"                                                            
## [2172] "Last Summer in the Hamptons"                                                
## [2173] "Inventing the Abbotts"                                                      
## [2174] "Gaslight"                                                                   
## [2175] "Shakes the Clown"                                                           
## [2176] "Out of the Past"                                                            
## [2177] "Fearless"                                                                   
## [2178] "Wings of Courage"                                                           
## [2179] "Entertaining Angels: The Dorothy Day Story"                                 
## [2180] "Close Shave, A"                                                             
## [2181] "Vibes"                                                                      
## [2182] "Grease"                                                                     
## [2183] "Nightmare on Elm Street 3: Dream Warriors, A"                               
## [2184] "Patton"                                                                     
## [2185] "Johns"                                                                      
## [2186] "Innocents, The"                                                             
## [2187] "Coming Home"                                                                
## [2188] "Batman Forever"                                                             
## [2189] "Those Who Love Me Can Take the Train (Ceux qui m'aiment prendront le train)"
## [2190] "Deliverance"                                                                
## [2191] "L.A. Story"                                                                 
## [2192] "Wrong Man, The"                                                             
## [2193] "Crush, The"                                                                 
## [2194] "Magnum Force"                                                               
## [2195] "Swingers"                                                                   
## [2196] "Wonder Boys"                                                                
## [2197] "Better Living Through Circuitry"                                            
## [2198] "Cousin Bette"                                                               
## [2199] "Life with Mikey"                                                            
## [2200] "Guilty as Sin"                                                              
## [2201] "Top Hat"                                                                    
## [2202] "Farewell to Arms, A"                                                        
## [2203] "Ill Gotten Gains"                                                           
## [2204] "Cecil B. Demented"                                                          
## [2205] "Schizopolis"                                                                
## [2206] "Live and Let Die"                                                           
## [2207] "Little City"                                                                
## [2208] "Onegin"                                                                     
## [2209] "Big Green, The"                                                             
## [2210] "Nixon"                                                                      
## [2211] "Happiest Millionaire, The"                                                  
## [2212] "Star Trek: First Contact"                                                   
## [2213] "Prizzi's Honor"                                                             
## [2214] "Psycho III"                                                                 
## [2215] "Hard Core Logo"                                                             
## [2216] "Queen Margot (La Reine Margot)"                                             
## [2217] "Art of War, The"                                                            
## [2218] "Rent-a-Kid"                                                                 
## [2219] "All the King's Men"                                                         
## [2220] "Daughters of the Dust"                                                      
## [2221] "Davy Crockett, King of the Wild Frontier"                                   
## [2222] "Arsenic and Old Lace"                                                       
## [2223] "Bride of Frankenstein"                                                      
## [2224] "Shadow Conspiracy"                                                          
## [2225] "In Old California"                                                          
## [2226] "Dante's Peak"                                                               
## [2227] "Risky Business"                                                             
## [2228] "NeverEnding Story II: The Next Chapter, The"                                
## [2229] "Letter From Death Row, A"                                                   
## [2230] "Conformist, The (Il Conformista)"                                           
## [2231] "Princess Mononoke, The (Mononoke Hime)"                                     
## [2232] "Last Days, The"                                                             
## [2233] "Fear, The"                                                                  
## [2234] "She's All That"                                                             
## [2235] "After Life"                                                                 
## [2236] "Walking Dead, The"                                                          
## [2237] "Lawnmower Man 2: Beyond Cyberspace"                                         
## [2238] "Torso (Corpi Presentano Tracce di Violenza Carnale)"                        
## [2239] "Wooden Man's Bride, The (Wu Kui)"                                           
## [2240] "Great Day in Harlem, A"                                                     
## [2241] "Ménage (Tenue de soirée)"                                                   
## [2242] "Man with the Golden Gun, The"                                               
## [2243] "Lost Horizon"                                                               
## [2244] "South Pacific"                                                              
## [2245] "Journey of August King, The"                                                
## [2246] "Conversation, The"                                                          
## [2247] "Plunkett & MaCleane"                                                        
## [2248] "Game, The"                                                                  
## [2249] "Spanish Fly"                                                                
## [2250] "Knockout"                                                                   
## [2251] "My Left Foot"                                                               
## [2252] "Bandits"                                                                    
## [2253] "Inferno"                                                                    
## [2254] "Seventh Sign, The"                                                          
## [2255] "Penny Serenade"                                                             
## [2256] "Wilde"                                                                      
## [2257] "Repossessed"                                                                
## [2258] "Fly Away Home"                                                              
## [2259] "Omega Man, The"                                                             
## [2260] "Face in the Crowd, A"                                                       
## [2261] "Jewel of the Nile, The"                                                     
## [2262] "Ugly, The"                                                                  
## [2263] "Passion in the Desert"                                                      
## [2264] "24-hour Woman"                                                              
## [2265] "Haunted Honeymoon"                                                          
## [2266] "Airplane II: The Sequel"                                                    
## [2267] "Funny Farm"                                                                 
## [2268] "Puppet Master 5: The Final Chapter"                                         
## [2269] "Celestial Clockwork"                                                        
## [2270] "Toxic Avenger, Part II, The"                                                
## [2271] "Legend of 1900, The (Leggenda del pianista sull'oceano)"                    
## [2272] "Toxic Avenger Part III: The Last Temptation of Toxie, The"                  
## [2273] "My Tutor"                                                                   
## [2274] "Sunchaser, The"                                                             
## [2275] "My Fair Lady"                                                               
## [2276] "Grace of My Heart"                                                          
## [2277] "Mighty Morphin Power Rangers: The Movie"                                    
## [2278] "Dancer in the Dark"                                                         
## [2279] "Remains of the Day, The"                                                    
## [2280] "Love Letter, The"                                                           
## [2281] "Wallace & Gromit: The Best of Aardman Animation"                            
## [2282] "Lion King, The"                                                             
## [2283] "Black Sheep"                                                                
## [2284] "Awakenings"                                                                 
## [2285] "West Side Story"                                                            
## [2286] "Nekromantik"                                                                
## [2287] "Rocky V"                                                                    
## [2288] "They Shoot Horses, Don't They?"                                             
## [2289] "School of Flesh, The (L' École de la chair)"                                
## [2290] "Pit and the Pendulum"                                                       
## [2291] "Problem Child"                                                              
## [2292] "Song of the South"                                                          
## [2293] "Man Facing Southeast (Hombre Mirando al Sudeste)"                           
## [2294] "Where's Marlowe?"                                                           
## [2295] "River Runs Through It, A"                                                   
## [2296] "Mystery Men"                                                                
## [2297] "Class"                                                                      
## [2298] "Battling Butler"                                                            
## [2299] "2 Days in the Valley"                                                       
## [2300] "Made in America"                                                            
## [2301] "Niagara"                                                                    
## [2302] "Alphaville"                                                                 
## [2303] "White Boys"                                                                 
## [2304] "Fight Club"                                                                 
## [2305] "Swept Away (Travolti da un insolito destino nell'azzurro mare d'Agosto)"    
## [2306] "Grandview, U.S.A."                                                          
## [2307] "Allnighter, The"                                                            
## [2308] "Working Girl"                                                               
## [2309] "Daughter of Dr. Jeckyll"                                                    
## [2310] "Hell in the Pacific"                                                        
## [2311] "Jerry & Tom"                                                                
## [2312] "Play it to the Bone"                                                        
## [2313] "Class Reunion"                                                              
## [2314] "Patriot Games"                                                              
## [2315] "3 Strikes"                                                                  
## [2316] "Mamma Roma"                                                                 
## [2317] "...And Justice for All"                                                     
## [2318] "Animal House"                                                               
## [2319] "Death Wish"                                                                 
## [2320] "Cool as Ice"                                                                
## [2321] "Return to Me"                                                               
## [2322] "Good Mother, The"                                                           
## [2323] "Room with a View, A"                                                        
## [2324] "Raging Bull"                                                                
## [2325] "Balto"                                                                      
## [2326] "Field of Dreams"                                                            
## [2327] "Star Trek: The Motion Picture"                                              
## [2328] "Flirting With Disaster"                                                     
## [2329] "Sgt. Bilko"                                                                 
## [2330] "My Life and Times With Antonin Artaud (En compagnie d'Antonin Artaud)"      
## [2331] "I Woke Up Early the Day I Died"                                             
## [2332] "West Beirut (West Beyrouth)"                                                
## [2333] "Rebecca"                                                                    
## [2334] "Best Laid Plans"                                                            
## [2335] "Seventh Heaven (Le Septième ciel)"                                          
## [2336] "Black Cat, White Cat (Crna macka, beli macor)"                              
## [2337] "Adventures of Milo and Otis, The"                                           
## [2338] "Queens Logic"                                                               
## [2339] "King in New York, A"                                                        
## [2340] "MURDER and murder"                                                          
## [2341] "Lulu on the Bridge"                                                         
## [2342] "Of Human Bondage"                                                           
## [2343] "Little Lord Fauntleroy"                                                     
## [2344] "Indochine"                                                                  
## [2345] "Fistful of Dollars, A"                                                      
## [2346] "Leather Jacket Love Story"                                                  
## [2347] "Clean Slate (Coup de Torchon)"                                              
## [2348] "Big Bang Theory, The"                                                       
## [2349] "Chicken Run"                                                                
## [2350] "F/X"                                                                        
## [2351] "Indecent Proposal"                                                          
## [2352] "Rocket Man"                                                                 
## [2353] "Women on the Verge of a Nervous Breakdown"                                  
## [2354] "Assassins"                                                                  
## [2355] "All About My Mother (Todo Sobre Mi Madre)"                                  
## [2356] "Red Firecracker, Green Firecracker"                                         
## [2357] "Sorority House Massacre"                                                    
## [2358] "Emperor and the Assassin, The (Jing ke ci qin wang)"                        
## [2359] "City of Angels"                                                             
## [2360] "Englishman Who Went Up a Hill, But Came Down a Mountain, The"               
## [2361] "Reindeer Games"                                                             
## [2362] "Weekend at Bernie's"                                                        
## [2363] "Terrorist, The (Malli)"                                                     
## [2364] "Instinct"                                                                   
## [2365] "Color Me Blood Red"                                                         
## [2366] "American Pie"                                                               
## [2367] "Striking Distance"                                                          
## [2368] "Pallbearer, The"                                                            
## [2369] "Bells, The"                                                                 
## [2370] "How to Stuff a Wild Bikini"                                                 
## [2371] "Arthur"                                                                     
## [2372] "Homeward Bound: The Incredible Journey"                                     
## [2373] "Radio Days"                                                                 
## [2374] "Roula"                                                                      
## [2375] "Flatliners"                                                                 
## [2376] "Wonderful, Horrible Life of Leni Riefenstahl, The (Die Macht der Bilder)"   
## [2377] "Shadows (Cienie)"                                                           
## [2378] "Mirror Has Two Faces, The"                                                  
## [2379] "Head Above Water"                                                           
## [2380] "Hollow Reed"                                                                
## [2381] "Halloween: The Curse of Michael Myers"                                      
## [2382] "Outlaw, The"                                                                
## [2383] "Godzilla 2000 (Gojira ni-sen mireniamu)"                                    
## [2384] "Soldier"                                                                    
## [2385] "T-Men"                                                                      
## [2386] "Kindred, The"                                                               
## [2387] "Gremlins 2: The New Batch"                                                  
## [2388] "Deceiver"                                                                   
## [2389] "Dangerous Ground"                                                           
## [2390] "Meet Me in St. Louis"                                                       
## [2391] "Went to Coney Island on a Mission From God... Be Back by Five"              
## [2392] "Midnight Run"                                                               
## [2393] "Ninth Gate, The"                                                            
## [2394] "Mixed Nuts"                                                                 
## [2395] "Coming Apart"                                                               
## [2396] "Escape from L.A."                                                           
## [2397] "Laura"                                                                      
## [2398] "First Kid"                                                                  
## [2399] "Algiers"                                                                    
## [2400] "Our Town"                                                                   
## [2401] "Other Side of Sunday, The (Søndagsengler)"                                  
## [2402] "Back Stage"                                                                 
## [2403] "In the Line of Fire"                                                        
## [2404] "Children of a Lesser God"                                                   
## [2405] "Runaway Train"                                                              
## [2406] "Gung Ho"                                                                    
## [2407] "Battle for the Planet of the Apes"                                          
## [2408] "Relax... It's Just Sex"                                                     
## [2409] "Exorcist, The"                                                              
## [2410] "Omen, The"                                                                  
## [2411] "Twin Peaks: Fire Walk with Me"                                              
## [2412] "Teenage Mutant Ninja Turtles II: The Secret of the Ooze"                    
## [2413] "Haunting, The"                                                              
## [2414] "Mimic"                                                                      
## [2415] "Shall We Dance? (Shall We Dansu?)"                                          
## [2416] "Air Force One"                                                              
## [2417] "Wishmaster"                                                                 
## [2418] "Fire Down Below"                                                            
## [2419] "Few Good Men, A"                                                            
## [2420] "Afterglow"                                                                  
## [2421] "Outside Ozona"                                                              
## [2422] "Children of the Corn IV: The Gathering"                                     
## [2423] "Sesame Street Presents Follow That Bird"                                    
## [2424] "Little Big League"                                                          
## [2425] "Madonna: Truth or Dare"                                                     
## [2426] "Manon of the Spring (Manon des sources)"                                    
## [2427] "Milk Money"                                                                 
## [2428] "Jupiter's Wife"                                                             
## [2429] "Deep Blue Sea"                                                              
## [2430] "Die Hard 2"                                                                 
## [2431] "Empire Records"                                                             
## [2432] "Bamba, La"                                                                  
## [2433] "Blood Simple"                                                               
## [2434] "Beautiful Girls"                                                            
## [2435] "Aiqing wansui"                                                              
## [2436] "Double Team"                                                                
## [2437] "George of the Jungle"                                                       
## [2438] "Dirty Dancing"                                                              
## [2439] "Man on the Moon"                                                            
## [2440] "Two Women (La Ciociara)"                                                    
## [2441] "Single Girl, A (La Fille Seule)"                                            
## [2442] "My Boyfriend's Back"                                                        
## [2443] "Kicking and Screaming"                                                      
## [2444] "Spitfire Grill, The"                                                        
## [2445] "City of Industry"                                                           
## [2446] "Rob Roy"                                                                    
## [2447] "Lady Vanishes, The"                                                         
## [2448] "Savior"                                                                     
## [2449] "Wag the Dog"                                                                
## [2450] "Kelly's Heroes"                                                             
## [2451] "Frankenstein Meets the Wolf Man"                                            
## [2452] "Madness of King George, The"                                                
## [2453] "Damien: Omen II"                                                            
## [2454] "Surviving the Game"                                                         
## [2455] "Georgia"                                                                    
## [2456] "Great Mouse Detective, The"                                                 
## [2457] "Father of the Bride"                                                        
## [2458] "Brother's Kiss, A"                                                          
## [2459] "Time Regained (Le Temps Retrouvé)"                                          
## [2460] "Any Given Sunday"                                                           
## [2461] "One Flew Over the Cuckoo's Nest"                                            
## [2462] "Passion Fish"                                                               
## [2463] "To Sir with Love"                                                           
## [2464] "Tigrero: A Film That Was Never Made"                                        
## [2465] "Heavy"                                                                      
## [2466] "Kazaam"                                                                     
## [2467] "Big"                                                                        
## [2468] "Citizen Kane"                                                               
## [2469] "Cat Ballou"                                                                 
## [2470] "Little Boy Blue"                                                            
## [2471] "Fugitive, The"                                                              
## [2472] "Scout, The"                                                                 
## [2473] "American Graffiti"                                                          
## [2474] "Time Code"                                                                  
## [2475] "8 1/2 Women"                                                                
## [2476] "It's a Wonderful Life"                                                      
## [2477] "Beat the Devil"                                                             
## [2478] "Last Time I Saw Paris, The"                                                 
## [2479] "Children of the Corn"                                                       
## [2480] "Never Met Picasso"                                                          
## [2481] "Jerky Boys, The"                                                            
## [2482] "Deep Rising"                                                                
## [2483] "Brief Encounter"                                                            
## [2484] "Ten Benny"                                                                  
## [2485] "Ed Wood"                                                                    
## [2486] "Bootmen"                                                                    
## [2487] "Follow the Bitch"                                                           
## [2488] "Impact"                                                                     
## [2489] "Metisse (Café au Lait)"                                                     
## [2490] "Dear Diary (Caro Diario)"                                                   
## [2491] "I Don't Want to Talk About It (De eso no se habla)"                         
## [2492] "Brady Bunch Movie, The"                                                     
## [2493] "Raining Stones"                                                             
## [2494] "Clockwork Orange, A"                                                        
## [2495] "Sliver"                                                                     
## [2496] "Thirty-Two Short Films About Glenn Gould"                                   
## [2497] "Ordinary People"                                                            
## [2498] "Star Trek VI: The Undiscovered Country"                                     
## [2499] "I Shot a Man in Vegas"                                                      
## [2500] "Jaws 3-D"                                                                   
## [2501] "Great White Hype, The"                                                      
## [2502] "Grateful Dead"                                                              
## [2503] "Bless the Child"                                                            
## [2504] "Clue"                                                                       
## [2505] "True Lies"                                                                  
## [2506] "Men of Means"                                                               
## [2507] "Thomas and the Magic Railroad"                                              
## [2508] "Pajama Party"                                                               
## [2509] "Dangerous Game"                                                             
## [2510] "Dazed and Confused"                                                         
## [2511] "Final Destination"                                                          
## [2512] "Yards, The"                                                                 
## [2513] "Great Dictator, The"                                                        
## [2514] "Odd Couple, The"                                                            
## [2515] "Being There"                                                                
## [2516] "Volunteers"                                                                 
## [2517] "Human Traffic"                                                              
## [2518] "Striptease"                                                                 
## [2519] "Big Squeeze, The"                                                           
## [2520] "April Fool's Day"                                                           
## [2521] "Mission: Impossible 2"                                                      
## [2522] "Exorcist III, The"                                                          
## [2523] "Burnt By the Sun (Utomlyonnye solntsem)"                                    
## [2524] "Shine"                                                                      
## [2525] "Something to Sing About"                                                    
## [2526] "Mouse Hunt"                                                                 
## [2527] "Moonlight Murder"                                                           
## [2528] "Prancer"                                                                    
## [2529] "Down in the Delta"                                                          
## [2530] "Heavyweights"                                                               
## [2531] "Hot Shots! Part Deux"                                                       
## [2532] "Running Scared"                                                             
## [2533] "Going My Way"                                                               
## [2534] "They Might Be Giants"                                                       
## [2535] "Enfer, L'"                                                                  
## [2536] "Tin Drum, The (Blechtrommel, Die)"                                          
## [2537] "Wanted: Dead or Alive"                                                      
## [2538] "Halfmoon (Paul Bowles - Halbmond)"                                          
## [2539] "Barb Wire"                                                                  
## [2540] "Nobody Loves Me (Keiner liebt mich)"                                        
## [2541] "Nothing to Lose"                                                            
## [2542] "Ghost and Mrs. Muir, The"                                                   
## [2543] "Fatal Beauty"                                                               
## [2544] "Rosemary's Baby"                                                            
## [2545] "Desperate Measures"                                                         
## [2546] "Real Genius"                                                                
## [2547] "Bridge on the River Kwai, The"                                              
## [2548] "Tough Guys"                                                                 
## [2549] "Bhaji on the Beach"                                                         
## [2550] "Flower of My Secret, The (La Flor de Mi Secreto)"                           
## [2551] "Untouchables, The"                                                          
## [2552] "Mansfield Park"                                                             
## [2553] "Toy Story 2"                                                                
## [2554] "Creature From the Black Lagoon, The"                                        
## [2555] "Steal Big, Steal Little"                                                    
## [2556] "Touch of Evil"                                                              
## [2557] "Curse of the Puppet Master"                                                 
## [2558] "Alien³"                                                                     
## [2559] "Red Rock West"                                                              
## [2560] "Fathers' Day"                                                               
## [2561] "Steel"                                                                      
## [2562] "Floating"                                                                   
## [2563] "Richie Rich"                                                                
## [2564] "Ennui, L'"                                                                  
## [2565] "Locusts, The"                                                               
## [2566] "Lilian's Story"                                                             
## [2567] "Deuce Bigalow: Male Gigolo"                                                 
## [2568] "Up in Smoke"                                                                
## [2569] "Daytrippers, The"                                                           
## [2570] "Roseanna's Grave (For Roseanna)"                                            
## [2571] "Sexual Life of the Belgians, The"                                           
## [2572] "Nina Takes a Lover"                                                         
## [2573] "Tainted"                                                                    
## [2574] "French Kiss"                                                                
## [2575] "Harmonists, The"                                                            
## [2576] "King Kong Lives"                                                            
## [2577] "Money Pit, The"                                                             
## [2578] "Lifeforce"                                                                  
## [2579] "Mass Appeal"                                                                
## [2580] "McCullochs, The"                                                            
## [2581] "Big Trees, The"                                                             
## [2582] "Mascara"                                                                    
## [2583] "And God Created Woman"                                                      
## [2584] "Iron Giant, The"                                                            
## [2585] "Hustler White"                                                              
## [2586] "Superman IV: The Quest for Peace"                                           
## [2587] "Harry and the Hendersons"                                                   
## [2588] "400 Blows, The (Les Quatre cents coups)"                                    
## [2589] "Crow: City of Angels, The"                                                  
## [2590] "Castaway Cowboy, The"                                                       
## [2591] "Hot Lead and Cold Feet"                                                     
## [2592] "Brothers McMullen, The"                                                     
## [2593] "Quatermass II"                                                              
## [2594] "Reservoir Dogs"                                                             
## [2595] "Near Dark"                                                                  
## [2596] "It Happened One Night"                                                      
## [2597] "Jesus' Son"                                                                 
## [2598] "Frenzy"                                                                     
## [2599] "Faithful"                                                                   
## [2600] "Teenage Mutant Ninja Turtles III"                                           
## [2601] "Adventures of Priscilla, Queen of the Desert, The"                          
## [2602] "Heaven's Prisoners"                                                         
## [2603] "Double Jeopardy"                                                            
## [2604] "Men With Guns"                                                              
## [2605] "Jawbreaker"                                                                 
## [2606] "Black Tar Heroin: The Dark End of the Street"                               
## [2607] "Man from Laramie, The"                                                      
## [2608] "You Can't Take It With You"                                                 
## [2609] "True Romance"                                                               
## [2610] "Restoration"                                                                
## [2611] "Time to Kill, A"                                                            
## [2612] "American Buffalo"                                                           
## [2613] "Shooter, The"                                                               
## [2614] "Last Days of Disco, The"                                                    
## [2615] "Montana"                                                                    
## [2616] "Malice"                                                                     
## [2617] "Let it Come Down: The Life of Paul Bowles"                                  
## [2618] "Tales of Terror"                                                            
## [2619] "On the Beach"                                                               
## [2620] "Cat's Eye"                                                                  
## [2621] "My Family"                                                                  
## [2622] "Stalker"                                                                    
## [2623] "Problem Child 2"                                                            
## [2624] "My Favorite Season"                                                         
## [2625] "Wild Bill"                                                                  
## [2626] "Me, Myself and Irene"                                                       
## [2627] "U Turn"                                                                     
## [2628] "River Wild, The"                                                            
## [2629] "Apt Pupil"                                                                  
## [2630] "Devil in a Blue Dress"                                                      
## [2631] "Scary Movie"                                                                
## [2632] "X-Men"                                                                      
## [2633] "Rosetta"                                                                    
## [2634] "Wide Awake"                                                                 
## [2635] "Tampopo"                                                                    
## [2636] "Boiling Point"                                                              
## [2637] "Highlander III: The Sorcerer"                                               
## [2638] "Christine"                                                                  
## [2639] "Black Sabbath (Tre Volti Della Paura, I)"                                   
## [2640] "Addams Family Values"                                                       
## [2641] "Malcolm X"                                                                  
## [2642] "Trick or Treat"                                                             
## [2643] "Pi"                                                                         
## [2644] "Faraway, So Close (In Weiter Ferne, So Nah!)"                               
## [2645] "Star Trek: Generations"                                                     
## [2646] "Crimson Pirate, The"                                                        
## [2647] "Short Cuts"                                                                 
## [2648] "Bound for Glory"                                                            
## [2649] "Criminals"                                                                  
## [2650] "Born American"                                                              
## [2651] "Bloody Child, The"                                                          
## [2652] "Mrs. Winterbourne"                                                          
## [2653] "Time Masters (Les Maîtres du Temps)"                                        
## [2654] "Daens"                                                                      
## [2655] "Dersu Uzala"                                                                
## [2656] "Fair Game"                                                                  
## [2657] "Hippie Revolution, The"                                                     
## [2658] "Flying Tigers"                                                              
## [2659] "Doctor Zhivago"                                                             
## [2660] "Soylent Green"                                                              
## [2661] "Bewegte Mann, Der"                                                          
## [2662] "Headless Body in Topless Bar"                                               
## [2663] "Chambermaid on the Titanic, The"                                            
## [2664] "Sunset Strip"                                                               
## [2665] "McCabe & Mrs. Miller"                                                       
## [2666] "Favor, The"                                                                 
## [2667] "Slaves to the Underground"                                                  
## [2668] "Prom Night III: The Last Kiss"                                              
## [2669] "Blowup"                                                                     
## [2670] "Super Mario Bros."                                                          
## [2671] "Story of Xinghua, The"                                                      
## [2672] "Bittersweet Motel"                                                          
## [2673] "Run Lola Run (Lola rennt)"                                                  
## [2674] "Sword in the Stone, The"                                                    
## [2675] "Don Juan DeMarco"                                                           
## [2676] "Gendernauts"                                                                
## [2677] "Wild Bunch, The"                                                            
## [2678] "Never Cry Wolf"                                                             
## [2679] "Love! Valour! Compassion!"                                                  
## [2680] "Titan A.E."                                                                 
## [2681] "Communion"                                                                  
## [2682] "I Know What You Did Last Summer"                                            
## [2683] "All the Rage (a.k.a. It's the Rage)"                                        
## [2684] "Holy Man"                                                                   
## [2685] "Detroit 9000"                                                               
## [2686] "Natural, The"                                                               
## [2687] "Hi-Lo Country, The"                                                         
## [2688] "Police Academy 6: City Under Siege"                                         
## [2689] "Little Bit of Soul, A"                                                      
## [2690] "Lost Weekend, The"                                                          
## [2691] "Babe"                                                                       
## [2692] "Little Odessa"                                                              
## [2693] "Brandon Teena Story, The"                                                   
## [2694] "Eaten Alive"                                                                
## [2695] "Horror Express"                                                             
## [2696] "Highlander"                                                                 
## [2697] "Lord of the Flies"                                                          
## [2698] "House Arrest"                                                               
## [2699] "Body Parts"                                                                 
## [2700] "Bridges of Madison County, The"                                             
## [2701] "Breaking the Waves"                                                         
## [2702] "Fall"                                                                       
## [2703] "Gone in 60 Seconds"                                                         
## [2704] "Wild America"                                                               
## [2705] "Tickle in the Heart, A"                                                     
## [2706] "Lady of Burlesque"                                                          
## [2707] "Star Trek V: The Final Frontier"                                            
## [2708] "Gremlins"                                                                   
## [2709] "Office Killer"                                                              
## [2710] "Turtle Diary"                                                               
## [2711] "Lost Highway"                                                               
## [2712] "Spellbound"                                                                 
## [2713] "All Over Me"                                                                
## [2714] "Blow-Out (La Grande Bouffe)"                                                
## [2715] "Jackie Brown"                                                               
## [2716] "Perfect Candidate, A"                                                       
## [2717] "Deconstructing Harry"                                                       
## [2718] "Boogie Nights"                                                              
## [2719] "Madagascar Skin"                                                            
## [2720] "Wizard of Oz, The"                                                          
## [2721] "Sliding Doors"                                                              
## [2722] "Project Moon Base"                                                          
## [2723] "Spring Fever USA (a.k.a. Lauderdale)"                                       
## [2724] "Pale Rider"                                                                 
## [2725] "Time Tracers"                                                               
## [2726] "Horse Whisperer, The"                                                       
## [2727] "Seven Chances"                                                              
## [2728] "Solas"                                                                      
## [2729] "Air Up There, The"                                                          
## [2730] "Broken Hearts Club, The"                                                    
## [2731] "She-Devil"                                                                  
## [2732] "Dick Tracy"                                                                 
## [2733] "Babymother"                                                                 
## [2734] "Beach, The"                                                                 
## [2735] "Muppets From Space"                                                         
## [2736] "Century"                                                                    
## [2737] "Hangmen Also Die"                                                           
## [2738] "Hostile Intentions"                                                         
## [2739] "Mary Shelley's Frankenstein"                                                
## [2740] "Earth Vs. the Flying Saucers"                                               
## [2741] "It Conquered the World"                                                     
## [2742] "From the Hip"                                                               
## [2743] "Third World Cop"                                                            
## [2744] "It Could Happen to You"                                                     
## [2745] "D2: The Mighty Ducks"                                                       
## [2746] "Tie That Binds, The"                                                        
## [2747] "Quiet Room, The"                                                            
## [2748] "William Shakespeare's Romeo and Juliet"                                     
## [2749] "Macao"                                                                      
## [2750] "Fire on the Mountain"                                                       
## [2751] "Air Bud"                                                                    
## [2752] "Free Willy 3: The Rescue"                                                   
## [2753] "Pest, The"                                                                  
## [2754] "Beneath the Planet of the Apes"                                             
## [2755] "24 7: Twenty Four Seven"                                                    
## [2756] "Alive"                                                                      
## [2757] "Angels in the Outfield"                                                     
## [2758] "Silence of the Palace, The (Saimt el Qusur)"                                
## [2759] "Talk of Angels"                                                             
## [2760] "Bait"                                                                       
## [2761] "Brighton Beach Memoirs"                                                     
## [2762] "Gold Rush, The"                                                             
## [2763] "Wisdom"                                                                     
## [2764] "Barenaked in America"                                                       
## [2765] "This Is My Father"                                                          
## [2766] "Dying Young"                                                                
## [2767] "Drop Dead Gorgeous"                                                         
## [2768] "Symphonie pastorale, La"                                                    
## [2769] "Cinema Paradiso"                                                            
## [2770] "Young Guns"                                                                 
## [2771] "Amityville 1992: It's About Time"                                           
## [2772] "Citizen Ruth"                                                               
## [2773] "Tin Men"                                                                    
## [2774] "Anguish (Angustia)"                                                         
## [2775] "Mad City"                                                                   
## [2776] "Westworld"                                                                  
## [2777] "Railroaded!"                                                                
## [2778] "Jacob's Ladder"                                                             
## [2779] "End of the Affair, The"                                                     
## [2780] "Faster Pussycat! Kill! Kill!"                                               
## [2781] "Straight Story, The"                                                        
## [2782] "Johnny 100 Pesos"                                                           
## [2783] "Steal This Movie!"                                                          
## [2784] "Jennifer 8"                                                                 
## [2785] "Gods and Monsters"                                                          
## [2786] "Still Breathing"                                                            
## [2787] "Farinelli: il castrato"                                                     
## [2788] "Thin Red Line, The"                                                         
## [2789] "Safe Passage"                                                               
## [2790] "Happiness"                                                                  
## [2791] "I Dreamed of Africa"                                                        
## [2792] "Let's Talk About Sex"                                                       
## [2793] "Alvarez Kelly"                                                              
## [2794] "Robocop 2"                                                                  
## [2795] "Pawnbroker, The"                                                            
## [2796] "Timecop"                                                                    
## [2797] "Chuck & Buck"                                                               
## [2798] "Stargate"                                                                   
## [2799] "Come See the Paradise"                                                      
## [2800] "Almost Famous"                                                              
## [2801] "Buffalo 66"                                                                 
## [2802] "Basic Instinct"                                                             
## [2803] "Tashunga"                                                                   
## [2804] "Scorta, La"                                                                 
## [2805] "Lovers on the Bridge, The (Les Amants du Pont-Neuf)"                        
## [2806] "Inspector Gadget"                                                           
## [2807] "Killing, The"                                                               
## [2808] "Screamers"                                                                  
## [2809] "Hud"                                                                        
## [2810] "Little Indian, Big City (Un indien dans la ville)"                          
## [2811] "Rosewood"                                                                   
## [2812] "Cérémonie, La"                                                              
## [2813] "All About Eve"                                                              
## [2814] "Anaconda"                                                                   
## [2815] "Diebinnen"                                                                  
## [2816] "Hell Night"                                                                 
## [2817] "Operation Condor (Feiying gaiwak)"                                          
## [2818] "Puppet Master"                                                              
## [2819] "Home Fries"                                                                 
## [2820] "Apple Dumpling Gang Rides Again, The"                                       
## [2821] "Sprung"                                                                     
## [2822] "Kiss the Girls"                                                             
## [2823] "Red Sonja"                                                                  
## [2824] "Odd Couple II, The"                                                         
## [2825] "Secret Adventures of Tom Thumb, The"                                        
## [2826] "Heartbreak Ridge"                                                           
## [2827] "Harvest"                                                                    
## [2828] "Next Karate Kid, The"                                                       
## [2829] "Terminator, The"                                                            
## [2830] "Quest, The"                                                                 
## [2831] "Day the Sun Turned Cold, The (Tianguo niezi)"                               
## [2832] "Herbie Goes Bananas"                                                        
## [2833] "Muppet Christmas Carol, The"                                                
## [2834] "American Pimp"                                                              
## [2835] "Sanjuro"                                                                    
## [2836] "Wisdom of Crocodiles, The (a.k.a. Immortality)"                             
## [2837] "Dog Day Afternoon"                                                          
## [2838] "Cosi"                                                                       
## [2839] "King Creole"                                                                
## [2840] "African Queen, The"                                                         
## [2841] "Body Heat"                                                                  
## [2842] "Operation Dumbo Drop"                                                       
## [2843] "Music From Another Room"                                                    
## [2844] "Guy"                                                                        
## [2845] "Romancing the Stone"                                                        
## [2846] "Tigerland"                                                                  
## [2847] "I Went Down"                                                                
## [2848] "Sleepover"                                                                  
## [2849] "Visitors, The (Les Visiteurs)"                                              
## [2850] "Yellow Submarine"                                                           
## [2851] "Crossfire"                                                                  
## [2852] "Broken Arrow"                                                               
## [2853] "Quatermass and the Pit"                                                     
## [2854] "Cat on a Hot Tin Roof"                                                      
## [2855] "Once Upon a Time in America"                                                
## [2856] "Exotica"                                                                    
## [2857] "Mallrats"                                                                   
## [2858] "Digimon: The Movie"                                                         
## [2859] "Two Family House"                                                           
## [2860] "Specials, The"                                                              
## [2861] "Lotto Land"                                                                 
## [2862] "Last Dance"                                                                 
## [2863] "Supercop"                                                                   
## [2864] "Skulls, The"                                                                
## [2865] "Shadrach"                                                                   
## [2866] "Supergirl"                                                                  
## [2867] "Tales From the Crypt Presents: Demon Knight"                                
## [2868] "Rudy"                                                                       
## [2869] "Ace Ventura: Pet Detective"                                                 
## [2870] "Denise Calls Up"                                                            
## [2871] "Puppet Master III: Toulon's Revenge"                                        
## [2872] "White Squall"                                                               
## [2873] "Tie Me Up! Tie Me Down!"                                                    
## [2874] "Love Is a Many-Splendored Thing"                                            
## [2875] "White Balloon, The (Badkonake Sefid )"                                      
## [2876] "Jules and Jim (Jules et Jim)"                                               
## [2877] "Parenthood"                                                                 
## [2878] "Trip to Bountiful, The"                                                     
## [2879] "Baton Rouge"                                                                
## [2880] "Of Love and Shadows"                                                        
## [2881] "Four Weddings and a Funeral"                                                
## [2882] "Men in Black"                                                               
## [2883] "Things Change"                                                              
## [2884] "Peeping Tom"                                                                
## [2885] "Boys and Girls"                                                             
## [2886] "Stars and Bars"                                                             
## [2887] "Men Don't Leave"                                                            
## [2888] "Speed"                                                                      
## [2889] "Hush"                                                                       
## [2890] "Freejack"                                                                   
## [2891] "Guardian Angel"                                                             
## [2892] "She's Gotta Have It"                                                        
## [2893] "Bell, Book and Candle"                                                      
## [2894] "Theodore Rex"                                                               
## [2895] "Puppet Master 4"                                                            
## [2896] "Dead Man"                                                                   
## [2897] "Virgin Suicides, The"                                                       
## [2898] "Around the World in 80 Days"                                                
## [2899] "Collectionneuse, La"                                                        
## [2900] "Lady Eve, The"                                                              
## [2901] "Father of the Bride Part II"                                                
## [2902] "Galaxy Quest"                                                               
## [2903] "Pal Joey"                                                                   
## [2904] "Celtic Pride"                                                               
## [2905] "Hunt for Red October, The"                                                  
## [2906] "Legal Deceit"                                                               
## [2907] "Twilight"                                                                   
## [2908] "Illtown"                                                                    
## [2909] "No Escape"                                                                  
## [2910] "Abominable Snowman, The"                                                    
## [2911] "American in Paris, An"                                                      
## [2912] "Shakespeare in Love"                                                        
## [2913] "In & Out"                                                                   
## [2914] "Eyes of Tammy Faye, The"                                                    
## [2915] "Jurassic Park"                                                              
## [2916] "What Happened Was..."                                                       
## [2917] "New Age, The"                                                               
## [2918] "North"                                                                      
## [2919] "Bride of the Monster"                                                       
## [2920] "Castle, The"                                                                
## [2921] "Dreamlife of Angels, The (La Vie rêvée des anges)"                          
## [2922] "Frisk"                                                                      
## [2923] "Eraser"                                                                     
## [2924] "Jungle Book, The"                                                           
## [2925] "New York Cop"                                                               
## [2926] "Stigmata"                                                                   
## [2927] "It's in the Water"                                                          
## [2928] "Batman & Robin"                                                             
## [2929] "Something Wicked This Way Comes"                                            
## [2930] "In the Heat of the Night"                                                   
## [2931] "Searching for Bobby Fischer"                                                
## [2932] "Everest"                                                                    
## [2933] "Player's Club, The"                                                         
## [2934] "Big One, The"                                                               
## [2935] "Agnes of God"                                                               
## [2936] "Ravenous"                                                                   
## [2937] "My Crazy Life (Mi vida loca)"                                               
## [2938] "Barbarella"                                                                 
## [2939] "Independence Day (ID4)"                                                     
## [2940] "Harriet the Spy"                                                            
## [2941] "Sleeping Beauty"                                                            
## [2942] "Six-String Samurai"                                                         
## [2943] "Pinocchio"                                                                  
## [2944] "How to Be a Player"                                                         
## [2945] "Say Anything..."                                                            
## [2946] "Stop! Or My Mom Will Shoot"                                                 
## [2947] "Demolition Man"                                                             
## [2948] "Shane"                                                                      
## [2949] "In the Army Now"                                                            
## [2950] "Frighteners, The"                                                           
## [2951] "Rear Window"                                                                
## [2952] "Trekkies"                                                                   
## [2953] "Female Perversions"                                                         
## [2954] "Guantanamera"                                                               
## [2955] "Incredibly True Adventure of Two Girls in Love, The"                        
## [2956] "Three Colors: White"                                                        
## [2957] "Mortal Kombat: Annihilation"                                                
## [2958] "Ilsa, She Wolf of the SS"                                                   
## [2959] "Longest Day, The"                                                           
## [2960] "Judy Berlin"                                                                
## [2961] "Presidio, The"                                                              
## [2962] "Firm, The"                                                                  
## [2963] "Beauty and the Beast"                                                       
## [2964] "Forbidden Planet"                                                           
## [2965] "Candyman"                                                                   
## [2966] "Joy Luck Club, The"                                                         
## [2967] "Impostors, The"                                                             
## [2968] "Color of Paradise, The (Rang-e Khoda)"                                      
## [2969] "Battlefield Earth"                                                          
## [2970] "Regret to Inform"                                                           
## [2971] "Halloween III: Season of the Witch"                                         
## [2972] "Porky's Revenge"                                                            
## [2973] "Shaggy D.A., The"                                                           
## [2974] "Karate Kid, Part II, The"                                                   
## [2975] "Above the Rim"                                                              
## [2976] "Little Voice"                                                               
## [2977] "Richard III"                                                                
## [2978] "Blues Brothers, The"                                                        
## [2979] "Swamp Thing"                                                                
## [2980] "Underworld"                                                                 
## [2981] "Nothing But Trouble"                                                        
## [2982] "Rocky II"                                                                   
## [2983] "Insider, The"                                                               
## [2984] "Fatal Attraction"                                                           
## [2985] "'Night Mother"                                                              
## [2986] "Earthquake"                                                                 
## [2987] "Remember the Titans"                                                        
## [2988] "Marty"                                                                      
## [2989] "Erin Brockovich"                                                            
## [2990] "It Came from Outer Space"                                                   
## [2991] "Rocky"                                                                      
## [2992] "Paris, Texas"                                                               
## [2993] "Under Siege"                                                                
## [2994] "Shall We Dance?"                                                            
## [2995] "Running Free"                                                               
## [2996] "Loves of Carmen, The"                                                       
## [2997] "This World, Then the Fireworks"                                             
## [2998] "Corrina, Corrina"                                                           
## [2999] "One Magic Christmas"                                                        
## [3000] "Antz"                                                                       
## [3001] "Higher Learning"                                                            
## [3002] "Secrets & Lies"                                                             
## [3003] "Six Days Seven Nights"                                                      
## [3004] "Red Dwarf, The (Le Nain rouge)"                                             
## [3005] "Trippin'"                                                                   
## [3006] "Phenomenon"                                                                 
## [3007] "Microcosmos (Microcosmos: Le peuple de l'herbe)"                            
## [3008] "Lock, Stock & Two Smoking Barrels"                                          
## [3009] "To Kill a Mockingbird"                                                      
## [3010] "Passion of Mind"                                                            
## [3011] "Stand by Me"                                                                
## [3012] "Meet Wally Sparks"                                                          
## [3013] "Ride with the Devil"                                                        
## [3014] "Gingerbread Man, The"                                                       
## [3015] "Last of the High Kings, The (a.k.a. Summer Fling)"                          
## [3016] "Spice World"                                                                
## [3017] "City of Lost Children, The"                                                 
## [3018] "Pandora and the Flying Dutchman"                                            
## [3019] "Acid House, The"                                                            
## [3020] "Madame Butterfly"                                                           
## [3021] "Beloved"                                                                    
## [3022] "Random Hearts"                                                              
## [3023] "Gigi"                                                                       
## [3024] "Mighty Ducks, The"                                                          
## [3025] "Parasite"                                                                   
## [3026] "Forever Young"                                                              
## [3027] "Postman Always Rings Twice, The"                                            
## [3028] "Touki Bouki (Journey of the Hyena)"                                         
## [3029] "Small Time Crooks"                                                          
## [3030] "Hackers"                                                                    
## [3031] "Captain Horatio Hornblower"                                                 
## [3032] "Second Best"                                                                
## [3033] "Sophie's Choice"                                                            
## [3034] "Desert Blue"                                                                
## [3035] "Waking Ned Devine"                                                          
## [3036] "Gang Related"                                                               
## [3037] "True Grit"                                                                  
## [3038] "Frogs for Snakes"                                                           
## [3039] "As Good As It Gets"                                                         
## [3040] "Gay Deceivers, The"                                                         
## [3041] "Conan the Barbarian"                                                        
## [3042] "Spartacus"                                                                  
## [3043] "Bound"                                                                      
## [3044] "Picture Bride"                                                              
## [3045] "Geronimo: An American Legend"                                               
## [3046] "Total Recall"                                                               
## [3047] "Civil Action, A"                                                            
## [3048] "Maltese Falcon, The"                                                        
## [3049] "Postino, Il (The Postman)"                                                  
## [3050] "Slipper and the Rose, The"                                                  
## [3051] "Hollywood Knights, The"                                                     
## [3052] "His Girl Friday"                                                            
## [3053] "Hedd Wyn"                                                                   
## [3054] "Omega Code, The"                                                            
## [3055] "Cabinet of Dr. Ramirez, The"                                                
## [3056] "U2: Rattle and Hum"                                                         
## [3057] "Rules of Engagement"                                                        
## [3058] "Angels and Insects"                                                         
## [3059] "Only You"                                                                   
## [3060] "Prince of the City"                                                         
## [3061] "Shop Around the Corner, The"                                                
## [3062] "Summer of Sam"                                                              
## [3063] "Unstrung Heroes"                                                            
## [3064] "Fanny and Alexander"                                                        
## [3065] "Anna"                                                                       
## [3066] "Pushing Tin"                                                                
## [3067] "Desert Bloom"                                                               
## [3068] "Addicted to Love"                                                           
## [3069] "Germinal"                                                                   
## [3070] "Star Trek III: The Search for Spock"                                        
## [3071] "Peacemaker, The"                                                            
## [3072] "Porky's II: The Next Day"                                                   
## [3073] "Story of G.I. Joe, The"                                                     
## [3074] "Replacements, The"                                                          
## [3075] "Ballad of Ramblin' Jack, The"                                               
## [3076] "Saint of Fort Washington, The"                                              
## [3077] "Minnie and Moskowitz"                                                       
## [3078] "Quick and the Dead, The"                                                    
## [3079] "Stuart Little"                                                              
## [3080] "Jefferson in Paris"                                                         
## [3081] "Clerks"                                                                     
## [3082] "Man for All Seasons, A"                                                     
## [3083] "Paris Is Burning"                                                           
## [3084] "Ghost and the Darkness, The"                                                
## [3085] "Eat Drink Man Woman"                                                        
## [3086] "Hearts and Minds"                                                           
## [3087] "Charm's Incidents"                                                          
## [3088] "Hunchback of Notre Dame, The"                                               
## [3089] "Bedroom Window, The"                                                        
## [3090] "Rainmaker, The"                                                             
## [3091] "Ogre, The (Der Unhold)"                                                     
## [3092] "Crocodile Dundee"                                                           
## [3093] "Village of the Damned"                                                      
## [3094] "Pete's Dragon"                                                              
## [3095] "Not One Less (Yi ge dou bu neng shao)"                                      
## [3096] "Once Were Warriors"                                                         
## [3097] "Time Bandits"                                                               
## [3098] "Red Corner"                                                                 
## [3099] "Nights of Cabiria (Le Notti di Cabiria)"                                    
## [3100] "Dinosaur"                                                                   
## [3101] "Women, The"                                                                 
## [3102] "Hideaway"                                                                   
## [3103] "Doors, The"                                                                 
## [3104] "Hunger, The"                                                                
## [3105] "Jumanji"                                                                    
## [3106] "Normal Life"                                                                
## [3107] "Sixth Sense, The"                                                           
## [3108] "Cotton Mary"                                                                
## [3109] "Blue Hawaii"                                                                
## [3110] "Hear My Song"                                                               
## [3111] "Graduate, The"                                                              
## [3112] "Dick"                                                                       
## [3113] "Rawhead Rex"                                                                
## [3114] "Wife, The"                                                                  
## [3115] "Maximum Risk"                                                               
## [3116] "Disturbing Behavior"                                                        
## [3117] "Force 10 from Navarone"                                                     
## [3118] "American Strays"                                                            
## [3119] "Beavis and Butt-head Do America"                                            
## [3120] "Trading Places"                                                             
## [3121] "Messenger: The Story of Joan of Arc, The"                                   
## [3122] "Fresh"                                                                      
## [3123] "Batman"                                                                     
## [3124] "Ghostbusters II"                                                            
## [3125] "Dreaming of Joseph Lees"                                                    
## [3126] "Inkwell, The"                                                               
## [3127] "Prince of Tides, The"                                                       
## [3128] "Marlene Dietrich: Shadow and Light"                                         
## [3129] "What's Love Got to Do with It?"                                             
## [3130] "Band of the Hand"                                                           
## [3131] "Big Bully"                                                                  
## [3132] "Infinity"                                                                   
## [3133] "Borrowers, The"                                                             
## [3134] "Return of the Texas Chainsaw Massacre, The"                                 
## [3135] "Bogus"                                                                      
## [3136] "Sudden Death"                                                               
## [3137] "Walk in the Sun, A"                                                         
## [3138] "Devil Rides Out, The"                                                       
## [3139] "Twin Town"                                                                  
## [3140] "Thelma & Louise"                                                            
## [3141] "Fried Green Tomatoes"                                                       
## [3142] "Gate, The"                                                                  
## [3143] "Armed and Dangerous"                                                        
## [3144] "Cold Comfort Farm"                                                          
## [3145] "Playing by Heart"                                                           
## [3146] "10 Things I Hate About You"                                                 
## [3147] "Get Shorty"                                                                 
## [3148] "Monsieur Verdoux"                                                           
## [3149] "Nothing Personal"                                                           
## [3150] "X: The Unknown"                                                             
## [3151] "Endless Summer, The"                                                        
## [3152] "Picnic"                                                                     
## [3153] "Nightmare on Elm Street 5: The Dream Child, A"                              
## [3154] "House Party"                                                                
## [3155] "Beans of Egypt, Maine, The"                                                 
## [3156] "Heavy Metal"                                                                
## [3157] "Splash"                                                                     
## [3158] "Young Frankenstein"                                                         
## [3159] "Bodyguard, The"                                                             
## [3160] "Pee-wee's Big Adventure"                                                    
## [3161] "Contender, The"                                                             
## [3162] "Blair Witch Project, The"                                                   
## [3163] "Hoop Dreams"                                                                
## [3164] "Plutonium Circus"                                                           
## [3165] "Shadowlands"                                                                
## [3166] "Open Season"                                                                
## [3167] "Pink Flamingos"                                                             
## [3168] "Tank Girl"                                                                  
## [3169] "Legend of Lobo, The"                                                        
## [3170] "Albino Alligator"                                                           
## [3171] "Children of the Revolution"                                                 
## [3172] "Nighthawks"                                                                 
## [3173] "Hana-bi"                                                                    
## [3174] "Paradise Road"                                                              
## [3175] "Honey, I Blew Up the Kid"                                                   
## [3176] "Gabbeh"                                                                     
## [3177] "What's Eating Gilbert Grape"                                                
## [3178] "Return of Martin Guerre, The (Retour de Martin Guerre, Le)"                 
## [3179] "What Ever Happened to Baby Jane?"                                           
## [3180] "Halloween 4: The Return of Michael Myers"                                   
## [3181] "Blood & Wine"                                                               
## [3182] "Golden Earrings"                                                            
## [3183] "Taking of Pelham One Two Three, The"                                        
## [3184] "28 Days"                                                                    
## [3185] "Love, etc."                                                                 
## [3186] "Against All Odds"                                                           
## [3187] "Smoking/No Smoking"                                                         
## [3188] "Switchblade Sisters"                                                        
## [3189] "Inspector General, The"                                                     
## [3190] "Seven (Se7en)"                                                              
## [3191] "Mister Roberts"                                                             
## [3192] "Prince of Egypt, The"                                                       
## [3193] "Flamingo Kid, The"                                                          
## [3194] "Hellraiser"                                                                 
## [3195] "Fan, The"                                                                   
## [3196] "Network"                                                                    
## [3197] "Godfather: Part II, The"                                                    
## [3198] "Santa Fe Trail"                                                             
## [3199] "Benji the Hunted"                                                           
## [3200] "Clara's Heart"                                                              
## [3201] "Rain"                                                                       
## [3202] "Grand Illusion (Grande illusion, La)"                                       
## [3203] "Coma"                                                                       
## [3204] "Creepshow 2"                                                                
## [3205] "Killer's Kiss"                                                              
## [3206] "Man Who Knew Too Little, The"                                               
## [3207] "Meatballs III"                                                              
## [3208] "Star Trek: Insurrection"                                                    
## [3209] "Dinner Game, The (Le Dîner de cons)"                                        
## [3210] "Blood and Sand (Sangre y Arena)"                                            
## [3211] "Newsies"                                                                    
## [3212] "Dog's Life, A"                                                              
## [3213] "It Came from Beneath the Sea"                                               
## [3214] "Flipper"                                                                    
## [3215] "House Party 2"                                                              
## [3216] "Licence to Kill"                                                            
## [3217] "Repulsion"                                                                  
## [3218] "Last Man Standing"                                                          
## [3219] "Limbo"                                                                      
## [3220] "Diabolique"                                                                 
## [3221] "Rocky IV"                                                                   
## [3222] "Kalifornia"                                                                 
## [3223] "Amityville Horror, The"                                                     
## [3224] "Brother from Another Planet, The"                                           
## [3225] "High Noon"                                                                  
## [3226] "Quest for Camelot"                                                          
## [3227] "Gambler, The (A Játékos)"                                                   
## [3228] "Betrayed"                                                                   
## [3229] "Trees Lounge"                                                               
## [3230] "Bloodsport 2"                                                               
## [3231] "Stanley & Iris"                                                             
## [3232] "Wayne's World 2"                                                            
## [3233] "Whole Wide World, The"                                                      
## [3234] "Effect of Gamma Rays on Man-in-the-Moon Marigolds, The"                     
## [3235] "Garden of Finzi-Contini, The (Giardino dei Finzi-Contini, Il)"              
## [3236] "Cider House Rules, The"                                                     
## [3237] "Name of the Rose, The"                                                      
## [3238] "Jeffrey"                                                                    
## [3239] "Horror Hotel (a.k.a. The City of the Dead)"                                 
## [3240] "Jails, Hospitals & Hip-Hop"                                                 
## [3241] "Key Largo"                                                                  
## [3242] "Falling Down"                                                               
## [3243] "Senseless"                                                                  
## [3244] "Meet the Parents"                                                           
## [3245] "War Room, The"                                                              
## [3246] "Sunset Blvd. (a.k.a. Sunset Boulevard)"                                     
## [3247] "Night of the Living Dead"                                                   
## [3248] "Executive Decision"                                                         
## [3249] "Paper Chase, The"                                                           
## [3250] "Wedding Singer, The"                                                        
## [3251] "Race the Sun"                                                               
## [3252] "Belly"                                                                      
## [3253] "Alley Cats, The"                                                            
## [3254] "Raisin in the Sun, A"                                                       
## [3255] "Private School"                                                             
## [3256] "Eight Days a Week"                                                          
## [3257] "Airport 1975"                                                               
## [3258] "Cavalcade"                                                                  
## [3259] "Lord of Illusions"                                                          
## [3260] "From the Journals of Jean Seberg"                                           
## [3261] "Doom Generation, The"                                                       
## [3262] "Butcher Boy, The"                                                           
## [3263] "Adventures of Buckaroo Bonzai Across the 8th Dimension, The"                
## [3264] "Very Bad Things"                                                            
## [3265] "Peter Pan"                                                                  
## [3266] "Return of Jafar, The"                                                       
## [3267] "Rambo: First Blood Part II"                                                 
## [3268] "Slaughterhouse"                                                             
## [3269] "Belizaire the Cajun"                                                        
## [3270] "Robin Hood: Prince of Thieves"                                              
## [3271] "Horseman on the Roof, The (Hussard sur le toit, Le)"                        
## [3272] "Auntie Mame"                                                                
## [3273] "Nightwatch"                                                                 
## [3274] "Back to the Future Part II"                                                 
## [3275] "Commandments"                                                               
## [3276] "Rude"                                                                       
## [3277] "Original Kings of Comedy, The"                                              
## [3278] "My Bodyguard"                                                               
## [3279] "Best of the Best 3: No Turning Back"                                        
## [3280] "Tex"                                                                        
## [3281] "Plenty"                                                                     
## [3282] "Search for One-eye Jimmy, The"                                              
## [3283] "Freedom for Us (À nous la liberté )"                                        
## [3284] "Coldblooded"                                                                
## [3285] "Funhouse, The"                                                              
## [3286] "Back to School"                                                             
## [3287] "Blank Check"                                                                
## [3288] "Monument Ave."                                                              
## [3289] "Skipped Parts"                                                              
## [3290] "Halloween II"                                                               
## [3291] "For a Few Dollars More"                                                     
## [3292] "Make Mine Music"                                                            
## [3293] "I'll Be Home For Christmas"                                                 
## [3294] "39 Steps, The"                                                              
## [3295] "Trigger Effect, The"                                                        
## [3296] "For Whom the Bell Tolls"                                                    
## [3297] "Goodbye, 20th Century (Zbogum na dvadesetiot vek)"                          
## [3298] "Jack and Sarah"                                                             
## [3299] "Blue in the Face"                                                           
## [3300] "Warriors of Virtue"                                                         
## [3301] "Mrs. Dalloway"                                                              
## [3302] "I Love You, I Love You Not"                                                 
## [3303] "Midnight in the Garden of Good and Evil"                                    
## [3304] "British Intelligence"                                                       
## [3305] "Three Colors: Red"                                                          
## [3306] "Sister Act"                                                                 
## [3307] "Full Metal Jacket"                                                          
## [3308] "Mosquito Coast, The"                                                        
## [3309] "Braindead"                                                                  
## [3310] "Quartier Mozart"                                                            
## [3311] "Hocus Pocus"                                                                
## [3312] "Desperado"                                                                  
## [3313] "Full Monty, The"                                                            
## [3314] "Swept from the Sea"                                                         
## [3315] "Autumn Heart"                                                               
## [3316] "Object of My Affection, The"                                                
## [3317] "Beautiful People"                                                           
## [3318] "Kissing a Fool"                                                             
## [3319] "Two Thousand Maniacs!"                                                      
## [3320] "Autopsy (Macchie Solari)"                                                   
## [3321] "Paths of Glory"                                                             
## [3322] "Filth and the Fury, The"                                                    
## [3323] "Love Is All There Is"                                                       
## [3324] "Selena"                                                                     
## [3325] "Victor/Victoria"                                                            
## [3326] "Nowhere"                                                                    
## [3327] "Smoke Signals"                                                              
## [3328] "Casper"                                                                     
## [3329] "Train of Life (Train De Vie)"                                               
## [3330] "Prerokbe Ognja"                                                             
## [3331] "Little Big Man"                                                             
## [3332] "Eve's Bayou"                                                                
## [3333] "Patriot, The"                                                               
## [3334] "Madeline"                                                                   
## [3335] "Black Tights (Les Collants Noirs)"                                          
## [3336] "Mr. Mom"                                                                    
## [3337] "Land and Freedom (Tierra y libertad)"                                       
## [3338] "Trainspotting"                                                              
## [3339] "There's Something About Mary"                                               
## [3340] "Congo"                                                                      
## [3341] "Muppets Take Manhattan, The"                                                
## [3342] "Mona Lisa"                                                                  
## [3343] "Wood, The"                                                                  
## [3344] "Last Klezmer: Leopold Kozlowski, His Life and Music, The"                   
## [3345] "'burbs, The"                                                                
## [3346] "Guys and Dolls"                                                             
## [3347] "Dancing at Lughnasa"                                                        
## [3348] "Tomorrow Never Dies"                                                        
## [3349] "How Green Was My Valley"                                                    
## [3350] "I Stand Alone (Seul contre tous)"                                           
## [3351] "Any Number Can Win (Mélodie en sous-sol )"                                  
## [3352] "It's My Party"                                                              
## [3353] "Cook the Thief His Wife & Her Lover, The"                                   
## [3354] "Julien Donkey-Boy"                                                          
## [3355] "Ratchet"                                                                    
## [3356] "Tango Lesson, The"                                                          
## [3357] "Anchors Aweigh"                                                             
## [3358] "Room at the Top"                                                            
## [3359] "Lake Placid"                                                                
## [3360] "Incredible Journey, The"                                                    
## [3361] "Dead Men Don't Wear Plaid"                                                  
## [3362] "Condo Painting"                                                             
## [3363] "Phantom of the Opera, The"                                                  
## [3364] "Crossing Guard, The"                                                        
## [3365] "Crucible, The"                                                              
## [3366] "Under the Rainbow"                                                          
## [3367] "Low Down Dirty Shame, A"                                                    
## [3368] "Waiting to Exhale"                                                          
## [3369] "Cube"                                                                       
## [3370] "Homegrown"                                                                  
## [3371] "Dracula: Dead and Loving It"                                                
## [3372] "Getaway, The"                                                               
## [3373] "House of the Spirits, The"                                                  
## [3374] "Simple Twist of Fate, A"                                                    
## [3375] "Hoodlum"                                                                    
## [3376] "Wedding Gift, The"                                                          
## [3377] "Something for Everyone"                                                     
## [3378] "Love & Human Remains"                                                       
## [3379] "Carriers Are Waiting, The (Les Convoyeurs Attendent)"                       
## [3380] "Color Purple, The"                                                          
## [3381] "Cyclo"                                                                      
## [3382] "Gilda"                                                                      
## [3383] "East-West (Est-ouest)"                                                      
## [3384] "Niagara, Niagara"                                                           
## [3385] "Hilary and Jackie"                                                          
## [3386] "Man of the House"                                                           
## [3387] "Thousand Acres, A"                                                          
## [3388] "Rush Hour"                                                                  
## [3389] "Somewhere in the City"                                                      
## [3390] "FairyTale: A True Story"                                                    
## [3391] "James and the Giant Peach"                                                  
## [3392] "Big Carnival, The"                                                          
## [3393] "Bed of Roses"                                                               
## [3394] "Lassie"                                                                     
## [3395] "Live Nude Girls"                                                            
## [3396] "Brassed Off"                                                                
## [3397] "Commitments, The"                                                           
## [3398] "Caught Up"                                                                  
## [3399] "Kids"                                                                       
## [3400] "Sorority House Massacre II"                                                 
## [3401] "Shampoo"                                                                    
## [3402] "Desert Winds"                                                               
## [3403] "Children of the Corn III"                                                   
## [3404] "Cimarron"                                                                   
## [3405] "Concorde: Airport '79, The"                                                 
## [3406] "Snow Day"                                                                   
## [3407] "Ghostbusters"                                                               
## [3408] "Maya Lin: A Strong Clear Vision"                                            
## [3409] "I Like It Like That"                                                        
## [3410] "Girl 6"                                                                     
## [3411] "Pharaoh's Army"                                                             
## [3412] "Herbie Goes to Monte Carlo"                                                 
## [3413] "Honey, I Shrunk the Kids"                                                   
## [3414] "Dream With the Fishes"                                                      
## [3415] "Giant"                                                                      
## [3416] "Blue Velvet"                                                                
## [3417] "Kidnapped"                                                                  
## [3418] "Stefano Quantestorie"                                                       
## [3419] "American Gigolo"                                                            
## [3420] "Contact"                                                                    
## [3421] "Leave It to Beaver"                                                         
## [3422] "Crimson Tide"                                                               
## [3423] "Michael Collins"                                                            
## [3424] "Seven Years in Tibet"                                                       
## [3425] "Cocoon: The Return"                                                         
## [3426] "What Dreams May Come"                                                       
## [3427] "Storefront Hitchcock"                                                       
## [3428] "Couch in New York, A"                                                       
## [3429] "Varsity Blues"                                                              
## [3430] "Crime and Punishment in Suburbia"                                           
## [3431] "In the Name of the Father"                                                  
## [3432] "SLC Punk!"                                                                  
## [3433] "King of the Hill"                                                           
## [3434] "Fog, The"                                                                   
## [3435] "Illuminata"                                                                 
## [3436] "Mummy's Hand, The"                                                          
## [3437] "Fantastic Night, The (La Nuit Fantastique)"                                 
## [3438] "Flying Saucer, The"                                                         
## [3439] "End of Violence, The"                                                       
## [3440] "Funny Face"                                                                 
## [3441] "Fear of a Black Hat"                                                        
## [3442] "Live Flesh"                                                                 
## [3443] "Better Living"                                                              
## [3444] "Doug's 1st Movie"                                                           
## [3445] "Killer: A Journal of Murder"                                                
## [3446] "You've Got Mail"                                                            
## [3447] "Song of Freedom"                                                            
## [3448] "Getting Away With Murder"                                                   
## [3449] "B. Monkey"                                                                  
## [3450] "Liberty Heights"                                                            
## [3451] "Tumbleweeds"                                                                
## [3452] "Heartburn"                                                                  
## [3453] "Winter Guest, The"                                                          
## [3454] "Tales from the Darkside: The Movie"                                         
## [3455] "Mrs. Brown (Her Majesty, Mrs. Brown)"                                       
## [3456] "Fast, Cheap & Out of Control"                                               
## [3457] "Gordy"                                                                      
## [3458] "Gate of Heavenly Peace, The"                                                
## [3459] "Endless Summer 2, The"                                                      
## [3460] "Tom and Huck"                                                               
## [3461] "All Things Fair"                                                            
## [3462] "How to Make an American Quilt"                                              
## [3463] "Shadow, The"                                                                
## [3464] "Broken English"                                                             
## [3465] "Devil and Max Devlin, The"                                                  
## [3466] "Ripe"                                                                       
## [3467] "Candyman: Farewell to the Flesh"                                            
## [3468] "Stripes"                                                                    
## [3469] "Dogma"                                                                      
## [3470] "Austin Powers: The Spy Who Shagged Me"                                      
## [3471] "National Lampoon's Senior Trip"                                             
## [3472] "Program, The"                                                               
## [3473] "For Love or Money"                                                          
## [3474] "Poetic Justice"                                                             
## [3475] "Superman III"                                                               
## [3476] "Preacher's Wife, The"                                                       
## [3477] "Best Years of Our Lives, The"                                               
## [3478] "U-571"                                                                      
## [3479] "Faculty, The"                                                               
## [3480] "Lethal Weapon 4"                                                            
## [3481] "Spanish Prisoner, The"                                                      
## [3482] "Little Shop of Horrors, The"                                                
## [3483] "Big Fella"                                                                  
## [3484] "Edge, The"                                                                  
## [3485] "Guinevere"                                                                  
## [3486] "For Richer or Poorer"                                                       
## [3487] "Reluctant Debutante, The"                                                   
## [3488] "Ponette"                                                                    
## [3489] "She's So Lovely"                                                            
## [3490] "Go Fish"                                                                    
## [3491] "Jackal, The"                                                                
## [3492] "Porky's"                                                                    
## [3493] "Anatomy of a Murder"                                                        
## [3494] "Ed"                                                                         
## [3495] "Eighth Day, The (Le Huitième jour )"                                        
## [3496] "Return of the Pink Panther, The"                                            
## [3497] "Paper, The"                                                                 
## [3498] "Star Maps"                                                                  
## [3499] "All Dogs Go to Heaven 2"                                                    
## [3500] "Arguing the World"                                                          
## [3501] "Alice and Martin (Alice et Martin)"                                         
## [3502] "I Love Trouble"                                                             
## [3503] "Joyriders, The"                                                             
## [3504] "Slumber Party Massacre, The"                                                
## [3505] "You So Crazy"                                                               
## [3506] "They Bite"                                                                  
## [3507] "Velocity of Gary, The"                                                      
## [3508] "My Son the Fanatic"                                                         
## [3509] "Caught"                                                                     
## [3510] "Flintstones, The"                                                           
## [3511] "Silence of the Lambs, The"                                                  
## [3512] "Man of Her Dreams"                                                          
## [3513] "Lured"                                                                      
## [3514] "Prince Valiant"                                                             
## [3515] "Fletch Lives"                                                               
## [3516] "Mutiny on the Bounty"                                                       
## [3517] "Chasing Amy"                                                                
## [3518] "Lamerica"                                                                   
## [3519] "Heaven's Burning"                                                           
## [3520] "Sirens"                                                                     
## [3521] "Garbage Pail Kids Movie, The"                                               
## [3522] "Breakfast at Tiffany's"                                                     
## [3523] "Firestorm"                                                                  
## [3524] "Arrival, The"                                                               
## [3525] "Elizabeth"                                                                  
## [3526] "eXistenZ"                                                                   
## [3527] "Ready to Wear (Pret-A-Porter)"                                              
## [3528] "Two Crimes"                                                                 
## [3529] "In the Mouth of Madness"                                                    
## [3530] "Land Before Time III: The Time of the Great Giving"                         
## [3531] "Under Suspicion"                                                            
## [3532] "What About Bob?"                                                            
## [3533] "Someone Else's America"                                                     
## [3534] "Fandango"                                                                   
## [3535] "Thinner"                                                                    
## [3536] "Angel on My Shoulder"                                                       
## [3537] "Bonheur, Le"                                                                
## [3538] "Meatballs"                                                                  
## [3539] "Dead Again"                                                                 
## [3540] "Killer Shrews, The"                                                         
## [3541] "Broadway Melody, The"                                                       
## [3542] "Serial Mom"                                                                 
## [3543] "Big Sleep, The"                                                             
## [3544] "Sleepy Hollow"                                                              
## [3545] "Soft Fruit"                                                                 
## [3546] "Federal Hill"                                                               
## [3547] "Paradise Lost: The Child Murders at Robin Hood Hills"                       
## [3548] "Chinese Box"                                                                
## [3549] "Naked in New York"                                                          
## [3550] "Digging to China"                                                           
## [3551] "My Giant"                                                                   
## [3552] "Transformers: The Movie, The"                                               
## [3553] "Up at the Villa"                                                            
## [3554] "Palmetto"                                                                   
## [3555] "Shiloh"                                                                     
## [3556] "General's Daughter, The"                                                    
## [3557] "My Best Friend's Wedding"                                                   
## [3558] "Hurricane Streets"                                                          
## [3559] "Meatballs Part II"                                                          
## [3560] "Scream 2"                                                                   
## [3561] "Cry, the Beloved Country"                                                   
## [3562] "Ride"                                                                       
## [3563] "Mr. Jones"                                                                  
## [3564] "On Golden Pond"                                                             
## [3565] "B*A*P*S"                                                                    
## [3566] "Metropolis"                                                                 
## [3567] "Four Rooms"                                                                 
## [3568] "Much Ado About Nothing"                                                     
## [3569] "Fargo"                                                                      
## [3570] "About Adam"                                                                 
## [3571] "Rumble in the Bronx"                                                        
## [3572] "Free Willy"                                                                 
## [3573] "Clubland"                                                                   
## [3574] "Clueless"                                                                   
## [3575] "Jimmy Hollywood"                                                            
## [3576] "Arachnophobia"                                                              
## [3577] "Up Close and Personal"                                                      
## [3578] "People vs. Larry Flynt, The"                                                
## [3579] "Different for Girls"                                                        
## [3580] "Arlington Road"                                                             
## [3581] "Holy Smoke"                                                                 
## [3582] "Space Cowboys"                                                              
## [3583] "Peter's Friends"                                                            
## [3584] "Love Serenade"                                                              
## [3585] "Exit to Eden"                                                               
## [3586] "Barry Lyndon"                                                               
## [3587] "I Can't Sleep (J'ai pas sommeil)"                                           
## [3588] "Cross of Iron"                                                              
## [3589] "Late Bloomers"                                                              
## [3590] "An Unforgettable Summer"                                                    
## [3591] "Goodbye Girl, The"                                                          
## [3592] "Last Action Hero"                                                           
## [3593] "Funeral, The"                                                               
## [3594] "Six Degrees of Separation"                                                  
## [3595] "Autumn Tale, An (Conte d'automne)"                                          
## [3596] "Evil Dead II (Dead By Dawn)"                                                
## [3597] "Space Jam"                                                                  
## [3598] "Slaughterhouse 2"                                                           
## [3599] "Road to El Dorado, The"                                                     
## [3600] "Roustabout"                                                                 
## [3601] "Force of Evil"                                                              
## [3602] "Stay Tuned"
```

```r
# changing all NAs to 0
full_mov_df[is.na(full_mov_df)] <- 0

# running the utility function again to do the check for NAs
check_df(full_mov_df, 2)
```

```
## [1] "Checking for the total number of missing values (NA) in the dataframe if any:"
## 
##       UserID      MovieID       Rating    TimeStamp         Date 
##            0            0            0            0            0 
##         Time          Age     AgeRange       Gender OccupationID 
##            0            0            0            0            0 
##   Occupation      ZipCode   MovieTitle  ReleaseYear       Action 
##            0            0            0            0            0 
##    Adventure    Animation   Children's       Comedy        Crime 
##            0            0            0            0            0 
##  Documentary        Drama      Fantasy    Film-Noir       Horror 
##            0            0            0            0            0 
##      Musical      Mystery      Romance       Sci-Fi     Thriller 
##            0            0            0            0            0 
##          War      Western 
##            0            0
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
##  $ MovieTitle  : Factor w/ 3602 levels "'burbs, The",..: 805 799 1233 1772 1085 1050 3142 590 3023 879 ...
##  $ ReleaseYear : Factor w/ 82 levels "0","1919","1920",..: 65 76 79 76 66 80 23 78 79 80 ...
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




You can also embed plots, for example:

![](Movies_Recommender_System_files/figure-html/unnamed-chunk-2-1.png)\


