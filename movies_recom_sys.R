# Installing certain packages and loading all required libraries
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
#install.packages("sqldf")
#install.packages("reshape2")
suppressMessages(library(reshape2))
suppressMessages(library(splitstackshape))
suppressMessages(library(scales))
suppressMessages(library(ggplot2))

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
mov_df <- as.data.frame(Matrix, row.names = NULL, stringsAsFactors = FALSE)

# Checking the first few records of the mov_df dataframe
head(mov_df)

# From the readme file included, assigning the relevant column/variable names
names(mov_df) <- c("MovieID", "MovieTitle","Genere")

# Running the utility function to check the mov_df dataframe
check_df(mov_df, 0)
str(mov_df)

# since all variable types are factors, some will need appropriate conversion
# converting MovieTitle to character vector
#mov_df$MovieTitle <- as.character(mov_df$MovieTitle)

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
#sapply(mov_df, class)

# tidying up the environment
rm(mov_title, mov_year, mov_lines, Matrix)


# checks for special chars in the entire mov_df
# the result of the above will indicate which columns have issues
sapply(mov_df, function(y) sum(length(grep("[^[:alnum:]]", y))))


# split the Genere column to corresponding Generes which are separated by "|"
mov_df <- cSplit(mov_df, "Genere", sep="|")
#sapply(mov_df, class)
#sapply(mov_df, function(y) sum(length(which(is.na(y)))))
check_df(mov_df,0)

mdata <- melt(mov_df, id=c("MovieID","MovieTitle","ReleaseYear"))
#sapply(mdata, function(y) sum(length(which(is.na(y)))))
check_df(mdata, 0)
# removing records where value is NA
mdata <- mdata[!is.na(mdata$value),]
#sapply(mdata, function(y) sum(length(which(is.na(y)))))
check_df(mdata, 0)
# The 1 indicates that the movie has been classified into certain Generes
mdata$Type <- 1

mov_df <- dcast(mdata, MovieID + MovieTitle + ReleaseYear ~ value, value.var="Type")
check_df(mov_df,0)

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
usrs_df <- as.data.frame(Matrix, row.names = NULL, stringsAsFactors = FALSE)
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
ratings_df <- as.data.frame(Matrix, row.names = NULL, stringsAsFactors = FALSE)
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

#full_mov_df$Rating <- as.factor(full_mov_df$Rating)
#full_mov_df$AgeRange <- as.factor(full_mov_df$AgeRange)
#full_mov_df$Gender <- as.factor(full_mov_df$Gender)
#full_mov_df$Occupation <- as.factor(full_mov_df$Occupation)
#full_mov_df$MovieTitle <- as.factor(full_mov_df$MovieTitle)
#full_mov_df$ReleaseYear <- as.factor(full_mov_df$ReleaseYear)
# converting generes to factors which are from col 15 to 32 in the full_mov_df
#full_mov_df[15:32] <- lapply(full_mov_df[15:32], as.factor)




# 1. How the data is distributed out in the dataframe (a plot of count vs generes)

# converting the generes back to numeric to allow for counting
#full_mov_df[15:32] <- lapply(full_mov_df[15:32], as.numeric)
# creating a genere distribution dataframe
genere_dist <- full_mov_df %>% 
  select(Action:Western) %>% 
  summarise_each(funs(sum)) %>% 
  gather(Genere, Freq, Action:Western) %>%
  arrange(desc(Freq))


#Plotting the genere distribution of the dataset
ggplot(aes(x=reorder(Genere, -Freq), y=Freq), data = genere_dist) + 
  geom_bar(stat = "identity") +
  xlab("Generes") +
  ylab("Count") +
  scale_y_continuous(labels = comma)

# to show %
ggplot(aes(x=reorder(Genere, -Freq), y=(Freq/sum(Freq))), data = genere_dist) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent) +
  xlab("Generes") +
  ylab("Percent")


#The dataset shows a Right skewed distribution, where the Comedy and Drama generes make up a noticable part of the full_mov_df (approx. 35%), followed by Action and Thriller. The other generes appear grouped with similar number of occurances in the dataset (i.e. Scifi, Romance and Adventure seem to have between 5%-7% share each, Crime, Horror, Children's and War seem to be between 2.5%-3.5%, and so forth) 

#2. Gender vs Age distributions - It would be interesting to see how the users are distirbuted across the genders 

ggplot(full_mov_df, aes(AgeRange, fill = Gender)) + geom_bar() + scale_y_continuous(labels = comma)

#It appears that there is a significantly larger portion of male users in the full_mov_df than female users. It appears that the agerange from 18 to 44 makes up the most of the watched movies in the dataset, with 25-34 years of age leading the group. It would make sense as this is the age group where most of us are working in full swing and perhaps carving paths for raising families. As the users grow older, the number of movies watched decreases significantly; again, a not so surprising fact, that people would be busy raising families and enjoying time performaing other activities (outdoor perhaps). 

#3. Occupation Gender and Age distributions together - The idea here is to see if there is any relationship between age, gender and occupations. Perhaps, a particular occupation would house a greater agerange than others, and that in turn might prefer to watch a particular kind of genere.

ggplot(full_mov_df, aes(AgeRange, fill = Gender)) + facet_wrap(~Occupation, nrow = 7, ncol = 3) + geom_bar() + scale_y_continuous(labels = comma)


#A few occupations stand out where quite a lot of movies have been watched. A large number of them fall under college/grad student occupation, followed by other or not specified, executive/managerial, technician/engineer and the rest are relatively small proportion. As indicated above, the popular age groups obtained above seem to hold. It makes sense that 18-24 is the range where most people go to college. The gender disribution does not change from before either.

#4. Trends:

#Age, Gender, Occupation vs Generes - to find out a little bit about users' and if a particular agerange has preference on particular generes

age_gndr_occp_genre_dist <- full_mov_df %>%
  select(AgeRange, Gender, Occupation, Action:Western) %>% 
  group_by(AgeRange, Gender, Occupation) %>% 
  summarise_each(funs(sum), Action:Western) %>% 
  gather(Genere, Freq, Action:Western, -c(AgeRange, Gender, Occupation))


ggplot(age_gndr_occp_genre_dist, aes(x=reorder(Genere, -Freq), y=Freq, fill= AgeRange)) + geom_bar(stat = "identity") + scale_y_continuous(labels = comma)


#The output does not say much except that 25-34 agerange makes up the largest chunk of the distribution across all the generes. This was also noticed in the above plots.


#* Generes over Release years - to find out if there is there a change/popularity of Generes over time

genere_relyear_dist <- full_mov_df %>% select(ReleaseYear, Action:Western) %>% group_by(ReleaseYear) %>% summarise_each(funs(sum), Action:Western)  %>% gather(Genere, Freq, Action:Western, -ReleaseYear)

# plotting the distribution
genere_relyear_dist$ReleaseYear <- as.character(genere_relyear_dist$ReleaseYear)
ggplot(genere_relyear_dist, aes(ReleaseYear, Freq)) + geom_bar(stat = "identity") + facet_wrap(~Genere, nrow = 7, ncol=3)


#It looks like there is not much sensible data present before 1970, to be able to really observe a trend if there way any. 

#Refining the plot and taking years from 1970 onwards:
genere_relyear_dist$ReleaseYear <- as.character(genere_relyear_dist$ReleaseYear)
ggplot(subset(genere_relyear_dist, ReleaseYear > "1969"), aes(ReleaseYear, Freq)) + geom_bar(stat = "identity") + facet_wrap(~Genere, ncol=2)



usr_mov_rtg_df <- full_mov_df %>% select(UserID, MovieID, Rating)
str(usr_mov_rtg_df)

suppressMessages(library(recommenderlab))
#***
#The recommenderlab package from CRAN (hitherto referred to as "RLp"), will be used going forward, to:
#1. look at the rating behaviours of users through preliminary EDA
#2. pick 2000 users and build a popularity based model to recommend top 5 most popular movies for 20 active users
#3. come up with the top 5 movies for about 20 users. There are some assumptions that have been taken into account

#Addressing point 1 above:

# Creating the rating df consisting of UserID, MovieID and Rating columns
usr_mov_rtg_df <- full_mov_df %>% select(UserID, MovieID, Rating)
# checking structure
str(usr_mov_rtg_df)

#In order to use the RLp functionality, the above usr_mov_rtg_df dataframe first needs to be coerced to a "realRatingMatrix". 

rating_rRM <- as(usr_mov_rtg_df,"realRatingMatrix")
rating_rRM

# Looking at the format of rating_rRM
head(as(rating_rRM, "data.frame"))

# visualize the ratings_rRM matrix
image(rating_rRM, main = "Raw Ratings")

#There are 6040 users who have rated 3706 movies and contains 1,000,209 ratings (as orginally seen in the full_mov_df). Also note, t
#he RLp automatically creates the user, item and rating columns.

#Looking at the rating distribution of the dataset:

hist(getRatings(rating_rRM), breaks = 5, xlab = "Rating", main = "Histogram of Ratings and their Occurances")

#The histogram shows that users might be more biased towards giving ratings between 3 and 4 and not so inclined towards a rating of 1. 5 
#is also not given as frequently but is more often than 1.

#Normalizing the distribution to become more 0 centred:
hist(getRatings(normalize(rating_rRM)), breaks = 100, xlab = "Normalized Rating", main = "Histogram of Normalized Ratings")

#Here, the distribution is closer to that of a normal one (although it seems more right skewed towards the positive side), with interestingly high 
#peaks occuring between 0 and 1.

#Looking at the distribution of how many movies each user has rated:
hist(rowCounts(rating_rRM), breaks = 100, xlim = c(0,1000), xlab = "Number of Movies Rated per User", ylab = "Number of Users", main = "Distribution of Number of Movies Rated by each User")

#The plot shows that majority of the users lie in the region where they have rated atleast 20 movies (known fact from the MovieLens dataset), 
#and as the number of movies increase, the users count falls. This makes sense otherwise we would all be glued to the screens watching and rating movies all day long. 

hist(rowMeans(rating_rRM), breaks = 100, xlab = "Average Rating per User", main = "Distribution of Average Movie Rating by each User")

#This again shows that majority of the users have a tendency to rate between 3 and 4, with steep decline when rating 5. The range from 1 to 3 is the lowest, 
#perhaps meaning that users only prefer to watch movies that are rated 3 and above.

#Finally, looking at the mean rating per movie:
hist(colMeans(rating_rRM), breaks = 50, xlab = "Average Rating per Movie", main = "Distribution of Average Rating per Movie")

#Here it again shows that most of the movies lie between the rating of 3 and 4, with a few exceptions at 1 and 2.

#***
# EDA CONCLUSION

#Based on the EDA of the full_mov_df dataset, the following preliminary conclusions can be made:

#1. Out of the 18 Generes available, only about 5-7 of them are of significance and make up 76% of the entire dataset and have gained in popularity over 
#years (i.e. more of these generes were released after 1970). Releases from 1919 to 1969 are not at all significant
#2. Users in the age range from 18 to 44 are the ones who watch the most movies and gender distribution is not significant
#3. Out of the 21 occupations, only about 4 occupations stand out with another 4 at the next level down
#4. Finally, a large portion of movies fall between the ratings of 3, 4 and 5, and these also happen to be for the significant generes identified earlier

#Hence, if a realiable prediction model were to be built with some level of accuracy, it would make sense to conduct some feature engineering where insignificant 
#variables are dropped; i.e. such as 11 generes, movies released between 1919 to 1969, age groups below 18 and above 44, genders, 13 occupations and ratings of 1 and 2.

#For the purpose of this project, the above suggestion will not be employed.

#***
### BUILDING THE RECOMMENDER SYSTEM

#The recommender system will be built and then validated using the RLp. The steps are outlined below:

#  1. The rating_rRM realRatingMatrix created above contains the complete data set. The view of this matrix:
# visualize the ratings_rRM matrix for the 1st 5 users (out of 6040) and 10 items (out of 3706)
as(rating_rRM, "matrix")[1:5,1:10]

#The structure of the matrix is: UserIDs in rows, MovieIDs in the cols and Ratings filling up this matrix. As seen, most of it is filled with NAs. 
#Again, the Rmd output is not showing correctly. The console output would give the correct result.

#2. The User Based Collaborative Filtering model will be created and trained with the first 1000 users. This model noramalizes the data dand computes 
#the cosine similarity between the 1000 users.

# building the ubcf recommender model
rating1k_mod_ubcf <- Recommender(rating_rRM[1:1000], method = "UBCF")


#3  3. Now using the model to recommend the top 5 movies to the next 10 users (i.e. users 1001 to 1010):

# predicting the top 5 items for the 10 users
rating1k_rec_ubcf <- predict(rating1k_mod_ubcf, rating_rRM[1001:1010], n=5)

#putting the data in a presentable format
rating1k_mov_rec_df <- data.frame(matrix(nrow = 10, ncol = 5))
rownames(rating1k_mov_rec_df) <- names(rating1k_rec_ubcf@items)[1:10]
for(i in 1:10) {
  for (j in 1:5) {
    rating1k_mov_rec_df[i, j] <- paste0("m", rating1k_rec_ubcf@items[[i]][j])
  }
}
names(rating1k_mov_rec_df) <- as.character(1:5)
rating1k_mov_rec_df

#TThe top 5 recommendations for each of the 10 users is given above. There is a problem with this Rmd output (please check the result in the console window 
#by running the code chunk). If the movie names are required, the above dataframe can be semi-joined with the mov_df dataframe.

#4. In order to validate the rating1k_mod_ubcf model created above, it needs to be evaluated against the original data (rating_rRM matix). An evaluation scheme 
#in RLp is created where the same 1000 users matrix will be split into a 90% chunk (900 users) for training the model and 10% for testing (100 users). 
#For the test set, 20 items per user will be given to the recommender alogrithm (since this is the minimum number of movies each user has rated) while the other 
#will be used to compute the errors. In this model, a rating of 3 or above is considered good. k = 1 in the method signifies a single split of the matrix with no cross validation schemes.
(rating1k_es <- evaluationScheme(rating_rRM[1:1000,], method = "split", train = 0.9, k=1, given = 20, goodRating = 3))
# known data breakup
rating1k_es@knownData
# unknown data breakup
rating1k_es@unknownData

#5. Creating the recommender model based on the "UBCF" method. Here the data is already normalized (as seen).
# creating the User Based recommender using the training data and the cosine similarity method
rating1k_mod_train_ubcf<- Recommender(getData(rating1k_es, "train"), "UBCF")
getModel(rating1k_mod_train_ubcf)$data

#6. Making predictions on the test set using the UBCF model for the known part of the test data (20 items per user)
(rating1k_rec_ubcf_knwn <- predict(rating1k_mod_ubcf, getData(rating1k_es, "known"), type = "ratings"))


#7. Finally, calculating the prediction accuracy between the predicted and the unknown part of the test data:
rating1k_rec_ubcf_errs <- calcPredictionAccuracy(rating1k_rec_ubcf_knwn, getData(rating1k_es, "unknown"))
rating1k_rec_ubcf_errs


#Overall, the prediction errors produced by the UBCF recommendation model are not that high.

#***
### OVERALL CONCLUSION

#  If feature engineering were to be performed based on the preliminary conclusions stated in the EDA section, where users, movies and ratings would be selected for datasets which are more meaningful, the belief would be that the predictions can be made more accurate and relevant.

# Creating the user rating vs generes df 

#mv_usr_rtn_gen_df <- full_mov_df %>% select(UserID, MovieID, Rating, Action:Western) %>% 
#  gather(Genere, Yes.No, Action:Western, -c(UserID, MovieID, Rating)) %>% 
#  mutate(Rating.Yes.No = Rating * Yes.No) %>% 
#  filter(Rating.Yes.No !=0) %>% select(UserID, MovieID, Genere, Rating.Yes.No) %>% 
#  spread(Genere, Rating.Yes.No)
