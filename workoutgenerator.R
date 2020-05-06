# Daily Workout Generator

# Library loading

# --- Loading Packages --- 


library(dplyr)
library(purrr)
library(tidyr)
library(tidyverse)
library(e1071)

# ---- Loading Data ----

setwd("C:/Users/User/Documents/Data Science Projects/Workout Generator")

wodf <- read.csv("exercisesdataset.csv")
wodf <- data.frame(wodf)

# ---- Viewing Data ---- 

str(wodf)
head(wodf)

# ---- Reformatting Data ----

for (i in 1:ncol(wodf)) {
  wodf[,i] <- factor(wodf[,i])
}

wodf$bodyarea <- NA

wodf$body_area[wodf$muscle_general == "arms" |
                wodf$muscle_general == "chest" |
                wodf$muscle_general == "back" |
                wodf$muscle_general == "shoulders"]   <- "upper"

wodf$body_area[wodf$muscle_general == "legs" |
                wodf$muscle_general == "full"]        <- "lower"

wodf$body_area[wodf$muscle_general == "core"]         <- "core"

wodf$body_area <- factor(wodf$body_area)


# ---- Workout Selection Function ----

# Simplified workout generating function

# todaysworkout <- function(x) {
  
#  print(sample(wodf$exercise_name, 6))
  
# }




# ---- Next Steps ----


# Write a function that can throw together "Upper", "Leg", "Core", "All" workouts

# Something like 
# workoutgenerator <- function(x, All = FALSE, Upper = FALSE, Lower = FALSE, Core = FALSE) {
# if all = TRUE { sample from all exercises }
# if upper = TRUE { sample 2 chest, 2 back, 2 bicep, 2 tricep }
# if lower = TRUE { sample 5 leg, 2 core }
# if core = TRUE { sample 4 core, 2 random}
# }


todaysworkout <- function(x, full = FALSE, upper = FALSE, lower = FALSE, core = FALSE) {
  
  
  if (upper == TRUE) {
    exercisecount <- 8
  }
  if (core == TRUE) {
    exercisecount <- 6
  }
  if (lower == TRUE | full == TRUE) {
  exercisecount  <- sample(5:7, 1)
  }
  
  my.df   <- data.frame(matrix(NA, ncol = 4, nrow = exercisecount))
  colnames(my.df) <- c("Exercise", "Sets", "Difficulty", "Reps")
  
  
  if (full == TRUE) {
    
    # Logic here needs to include - selecting a random number of exercises
    # Selecting a random number of sets (same number for all exercises)
    # Number of reps will be a factor of difficulty and number of sets
    # Example: if sets = 4, then for my.df[i,3], reps = (8/sets * 15/difficulty)
      # would need to figure out how to make sure the final output in my.df is
      # rounded up to full numbers
    
    exerciseinfo <- wodf[sample(nrow(wodf),exercisecount),]
    exerciseinfo$difficulty <- as.numeric(exerciseinfo$difficulty)
    
    my.df[,1] <- exerciseinfo$exercise_name
    my.df[,2] <- sample(5:7, 1)
    my.df[,3] <- exerciseinfo$difficulty
    my.df[,4] <- ((8/my.df$Sets)*(12/exerciseinfo$difficulty))
    my.df[,4] <- format(my.df$Reps, digits = 0)
    my.df     <- my.df[ , -which(names(my.df) %in% c("Difficulty"))]
   
    print(my.df)
  }
  
  
  if (upper == TRUE) { 
    
    # Logic should select (at random) 2 chest, then 2 back, then 2 bicep, then 2 tricep exercises
    # can do it 1 at a time from my.df[1,1] , my.df[2,1], etc.
    # should do it in order, i.e. [1,1] is chest, [2,1] is back, [3,1] is tricep etc
    
    chestexercises  <- wodf[wodf$muscle_specific == "chest",]
    backexercises   <- wodf[wodf$muscle_specific == "back",]
    tricepexercises <- wodf[wodf$muscle_specific == "triceps",]
    bicepexercises  <- wodf[wodf$muscle_specific == "biceps",]
    
    selectedchest   <- chestexercises[sample(nrow(chestexercises),2),]
    selectedback    <- backexercises[sample(nrow(backexercises),2),]
    selectedtricep  <- tricepexercises[sample(nrow(tricepexercises),2),]
    selectedbicep   <- bicepexercises[sample(nrow(bicepexercises),2),]
    
    my.df1          <- rbind(selectedchest, selectedback, selectedtricep, selectedbicep)
    
    exerciseinfo    <- my.df1[c(1,3,5,7,2,4,6,8),]
    exerciseinfo$difficulty <- as.numeric(exerciseinfo$difficulty)
    
    my.df[,1] <- exerciseinfo$exercise_name
    my.df[,2] <- sample(4:5, 1)
    my.df[,3] <- exerciseinfo$difficulty
    my.df[,4] <- ((8/my.df$Sets)*(12/my.df$Difficulty))
    my.df[,4] <- format(my.df$Reps, digits = 0)
    my.df     <- subset(my.df, select = -c(Difficulty))
    
    print(my.df)
  }
  
  if (lower == TRUE) {
    
    legexercises  <- wodf[wodf$muscle_general == "legs",]
    fullexercises <- wodf[wodf$muscle_specific == "full",]
    
    selectedfull  <- fullexercises[sample(nrow(fullexercises),1),]
    selectedleg   <- legexercises[sample(nrow(legexercises), exercisecount - 1), ]
    
    my.df1        <- rbind(selectedfull, selectedleg)
    
    exerciseinfo  <- my.df1[c(2,3,1,4:exercisecount),]
    exerciseinfo$difficulty <- as.numeric(exerciseinfo$difficulty)
    
    my.df[,1] <- exerciseinfo$exercise_name
    my.df[,2] <- sample(4:5, 1)
    my.df[,3] <- exerciseinfo$difficulty
    my.df[,4] <- ((8/my.df$Sets) * (12/my.df$Difficulty))
    my.df[,4] <- format(my.df$Reps, digits = 0)
    my.df     <- subset(my.df, select = -c(Difficulty))
    
    print(my.df)
  }
  
  if (core == TRUE) {
    
    coreexercises   <- wodf[wodf$muscle_general == "core",]
    otherexercises  <- wodf[wodf$muscle_general != "core",]
    
    selectedcore    <- coreexercises[sample(nrow(coreexercises), 4),]
    selectedother   <- otherexercises[sample(nrow(otherexercises), 2),]
    
    my.df1          <- rbind(selectedcore, selectedother)
    
    exerciseinfo    <- my.df1[c(1,2,5,3,4,6),]
    exerciseinfo$difficulty <- as.numeric(exerciseinfo$difficulty)
    
    my.df[,1] <- exerciseinfo$exercise_name
    my.df[,2] <- sample(4:5, 1)
    my.df[,3] <- exerciseinfo$difficulty
    my.df[,4] <- ((8/my.df$Sets) * (12/my.df$Difficulty))
    my.df[,4] <- format(my.df$Reps, digits = 0)
    my.df     <- subset(my.df, select = -c(Difficulty))
    
    print(my.df)
  }
  cat("Keep up the pace throughout the workout!")
}


# ---- Potential Improvements ----

# Including a "goal completion time"

# in the "all" workout type, preventing same type of exercises back to back

# fine tuning the rep counts

# including workout types that involve holds rather than reps

# make adjustment to allow for light, moderate, difficult workouts

# Add an encouragement message, different every day


# To get output:

todaysworkout(upper = TRUE)
# todaysworkout(lower = TRUE)
# todaysworkout(core = TRUE)
# todaysworkout(full = TRUE)



