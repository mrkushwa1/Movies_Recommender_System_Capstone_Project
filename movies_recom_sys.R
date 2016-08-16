# Installing certain packages and loading all required libraries
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
install.packages("sqldf")
install.packages("reshape2")
suppressMessages(library (sqldf))
suppressMessages(library(reshape2))
suppressMessages(library(splitstackshape))
suppressMessages(library(scales))

# Clearing the R-console window
cat("\014")

# Clearing the working environment
rm(list = ls())

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

# reading the movies.dat file
mov_lines <- readLines("movies.dat")

# looking at the first few lines of the movie data to find out how it looks and what transformations will be necessary
head(mov_lines)

# Transforming the lines into a Matrix and then into a mov_df dataframe
Matrix <- do.call(rbind, strsplit(mov_lines,"::", fixed=TRUE))
mov_df <- as.data.frame(Matrix)

# Checking the first few records of the mov_df dataframe
head(mov_df)

# From the readme file included, assigning the relevant column/variable names
names(mov_df) <- c("MovieID", "MovieTitle","Genere")

# Running the utility function to check the mov_df dataframe
check_df(mov_df, 0)
str(mov_df)

# since all variable types are factors, some will need appropriate conversion
# converting MovieTitle to character vector
mov_df$MovieTitle <- as.character(mov_df$MovieTitle)

# converting MovieID to numeric vector
mov_df$MovieID <- as.numeric(mov_df$MovieID)

# Extracting the release year portion from the MovieTitle variable 
mov_year <- extract_numeric(substr(mov_df$MovieTitle, nchar(mov_df$MovieTitle)-5, nchar(mov_df$MovieTitle)))
mov_title <- substr(mov_df$MovieTitle, 1, nchar(mov_df$MovieTitle)-7)

# Reassigning the data back to the MovieTitle variable without the year
mov_df$MovieTitle <- mov_title

# Creating a Release year column in the mov_df
mov_df$ReleaseYear <- mov_year
head(mov_df)

# checking the transformed structure of the mov_df dataframe
sapply(mov_df, class)

# tidying up the environment
rm(mov_title, mov_year, mov_lines, Matrix)


# checks for special chars in the entire mov_df
# the result of the above will indicate which columns have issues
sapply(mov_df, function(y) sum(length(grep("[^[:alnum:]]", y))))


# split the Genere column to corresponding Generes which are separated by "|"
mov_df <- cSplit(mov_df, "Genere", sep="|")
#sapply(mov_df, class)
#sapply(mov_df, function(y) sum(length(which(is.na(y)))))
test(mov_df,0)

mdata <- melt(mov_df, id=c("MovieID","MovieTitle","ReleaseYear"))
#sapply(mdata, function(y) sum(length(which(is.na(y)))))
test(mdata, 0)
# removing records where value is NA
mdata <- mdata[!is.na(mdata$value),]
#sapply(mdata, function(y) sum(length(which(is.na(y)))))
test(mdata, 0)
# The 1 indicates that the movie has been classified into certain Generes
mdata$Type <- 1

mov_df <- dcast(mdata, MovieID + MovieTitle + ReleaseYear ~ value, value.var="Type")
test(mov_df,0)

# replacing all NA values with 0
mov_df[is.na(mov_df)] <- 0
str(mov_df)


# check to see if any punctuation characters are present in the dataframe
sapply(mov_df, function(y) sum(length(grep("[^[:alnum:]]", y))))
#rm(mdata)


# ================= READING USRS.DAT FILE, DISCOVERY AND TRANSFORMATION ==========================================
# reading the users.dat file
usrs_lines <- readLines("users.dat")
head(usrs_lines)
Matrix <- do.call(rbind, strsplit(usrs_lines,"::", fixed=TRUE))
usrs_df <- data.frame(Matrix, stringsAsFactors = FALSE)
names(usrs_df) <- c("UserID", "Gender", "Age", "OccupationID", "ZipCode")
str(usrs_df)

# Before convering certain variables to their correct types it is important to
# check for special chars in the entire usr_df dataframe
# the result of the above will indicate which columns have issues
sapply(usrs_df, function(y) sum(length(grep("[^[:alnum:]]", y))))

# we can use:
head(grep("[^[:alnum:]]", usrs_df$ZipCode, value = TRUE))

# since all US Zipcodes are 5 digits, selecting only the LHS values before the "-" (which are 5 digits)
# and replacing them back in the same place
usrs_df$ZipCode <- sub("(\\d{5}).*", "\\1", usrs_df$ZipCode)


# changing all columns but Gender to numeric
usrs_df[, colnames(usrs_df) != "Gender"] <- lapply(usrs_df[, colnames(usrs_df) != "Gender"], as.numeric)# converting certain variables to correct types
str(usrs_df)

#checking to see if there is any variation in how the Gender is entered
unique(usrs_df$Gender)

rm(usrs_lines, Matrix)



# reading the ratings.dat file
ratings_lines <- readLines("ratings.dat")
head(ratings_lines)
Matrix <- do.call(rbind, strsplit(ratings_lines,"::", fixed=TRUE))
ratings_df <- data.frame(Matrix, stringsAsFactors = FALSE)
names(ratings_df) <- c("UserID", "MovieID", "Rating", "TimeStamp")
str(ratings_df)
rm(ratings_lines, Matrix)
check_df(ratings_df, 0)

# Again changing all columns to numeric
ratings_df[, colnames(ratings_df)] <- lapply(ratings_df[, colnames(ratings_df)], as.numeric)

# Now epoch or UNIX time is given in the TimeStamp - so extracting the date and time from it
ratings_df$Date <- strftime(as.POSIXct(ratings_df$TimeStamp, origin = "1970-01-01", tz =""),"%Y-%m-%d")
ratings_df$Time <- strftime(as.POSIXct(ratings_df$TimeStamp, origin = "1970-01-01", tz =""),"%H:%M:%S")


# creating the occupation ref dataframe
occup_df <- read.csv("occupation.dat", sep = ":", header = FALSE, stringsAsFactors = FALSE, colClasses = c("numeric","character"))
names(occup_df) <- c("OccupationID", "Occupation")
str(occup_df)

# creating the ages ref dataframe
age_df <- read.csv("ages.dat", sep = ":", header = FALSE, stringsAsFactors = FALSE, colClasses = c("numeric","character"))
names(age_df) <- c("Age", "AgeRange")
str(age_df)


# once all the dataframes are prepared and checked, we can now join them to the ratings_df (as this is the base df which
# brings all the other dfs together)
usrs_df <- left_join(usrs_df, age_df, by = "Age")
usrs_df <- left_join(usrs_df, occup_df, by = "OccupationID")
head(usrs_df)
# reordering columns
usrs_df <- usrs_df[, c("UserID", "Age", "AgeRange", "Gender", "OccupationID", "Occupation", "ZipCode")]

full_mov_df <- left_join(ratings_df, usrs_df, by = "UserID")
full_mov_df <- left_join(full_mov_df, mov_df, by = "MovieID")

check_df(full_mov_df, 0)
#rm(age_df,occup_df,usrs_df,mov_df,ratings_df)


#cols_names_to_factor

full_mov_df$Rating <- as.factor(full_mov_df$Rating)
full_mov_df$AgeRange <- as.factor(full_mov_df$AgeRange)
full_mov_df$Gender <- as.factor(full_mov_df$Gender)
full_mov_df$Occupation <- as.factor(full_mov_df$Occupation)
full_mov_df$MovieTitle <- as.factor(full_mov_df$MovieTitle)
full_mov_df$ReleaseYear <- as.factor(full_mov_df$ReleaseYear)
# converting generes to factors which are from col 15 to 32 in the full_mov_df
full_mov_df[15:32] <- lapply(full_mov_df[15:32], as.factor)


