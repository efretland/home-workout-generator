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

wodf      <- read.csv("exercisesdataset.csv")
wodf      <- data.frame(wodf)
messages  <- read.csv("motivational_messages.csv")


# ---- Viewing Data ---- 

str(wodf)
head(wodf)

# ---- Reformatting Data ----

for (i in 1:ncol(wodf)) {
  wodf[,i] <- factor(wodf[,i])
}

wodf$body_area <- NA

wodf$body_area[wodf$muscle_general == "arms" |
                wodf$muscle_general == "chest" |
                wodf$muscle_general == "back" |
                wodf$muscle_general == "shoulders"]   <- "upper"

wodf$body_area[wodf$muscle_general == "legs" |
                wodf$muscle_general == "full"]        <- "lower"

wodf$body_area[wodf$muscle_general == "core"]         <- "core"

wodf$body_area <- factor(wodf$body_area)


messages$message <- as.character(messages$message)



# ---- Workout Selection Function ----

# todaysworkout <- function(x, hard = TRUE, moderate = FALSE, light = FALSE) {
  
#  print(sample(wodf$exercise_name, 6))
  
# }




# ---- Steps ----


# Write a function that can throw together "Upper", "Leg", "Core", "All" workouts

# Something like 
# workoutgenerator <- function(x, All = FALSE, Upper = FALSE, Lower = FALSE, Core = FALSE) {
# if all = TRUE { sample from all exercises }
# if upper = TRUE { sample 2 chest, 2 back, 2 bicep, 2 tricep }
# if lower = TRUE { sample 5 leg, 2 core }
# if core = TRUE { sample 4 core, 2 random}
# }


todaysworkout <- function(x, full = FALSE, upper = FALSE, lower = FALSE, core = FALSE, 
                          hard = FALSE, light = FALSE) {
  
  
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
    
    
    exerciseinfo <- wodf[sample(nrow(wodf),exercisecount),]
    exerciseinfo$difficulty <- as.numeric(as.character(exerciseinfo$difficulty))
    
    my.df[,1] <- exerciseinfo$exercise_name
    my.df[,2] <- sample(5:7, 1)
    my.df[,3] <- exerciseinfo$difficulty
    my.df[,4] <- ((8/my.df$Sets)*(12/exerciseinfo$difficulty))
    my.df[,4] <- format(my.df$Reps, digits = 0)
    my.df     <- my.df[ , -which(names(my.df) %in% c("Difficulty"))]
   
    time.target      <- as.numeric(as.character(format((1.1 * sum(my.df$Sets)), digits = 0)))
    
    if (hard == TRUE) {
      my.df$Sets <- (1 + my.df$Sets)
      time.target <- (time.target + 4)
    }
    if (light == TRUE) {
      my.df$Reps <- as.numeric(as.character(my.df$Reps))
      my.df$Reps <- my.df$Reps/1.2
      my.df$Reps <- format(my.df$Reps, digits = 0)
      time.target <- (time.target - 2)
    }
    
    print(my.df)
    
  }
  
  
  if (upper == TRUE) { 
    
    
    chestexercises  <- wodf[wodf$muscle_specific == "chest",]
    backexercises   <- wodf[wodf$muscle_specific == "back",]
    tricepexercises <- wodf[wodf$muscle_specific == "triceps" |
                              wodf$muscle_specific == "shoulders",]
    bicepexercises  <- wodf[wodf$muscle_specific == "biceps",]
    
    selectedchest   <- chestexercises[sample(nrow(chestexercises),2),]
    selectedback    <- backexercises[sample(nrow(backexercises),2),]
    selectedtricep  <- tricepexercises[sample(nrow(tricepexercises),2),]
    selectedbicep   <- bicepexercises[sample(nrow(bicepexercises),2),]
    
    my.df1          <- rbind(selectedchest, selectedback, selectedtricep, selectedbicep)
    
    exerciseinfo    <- my.df1[c(1,3,5,7,2,4,6,8),]
    exerciseinfo$difficulty <- as.numeric(as.character(exerciseinfo$difficulty))
    
    my.df[,1] <- exerciseinfo$exercise_name
    my.df[,2] <- sample(4:5, 1)
    my.df[,3] <- exerciseinfo$difficulty
    my.df[,4] <- ((8/my.df$Sets)*(12/my.df$Difficulty))
    my.df[,4] <- format(my.df$Reps, digits = 0)
    my.df     <- subset(my.df, select = -c(Difficulty))
    
    time.target      <- as.numeric(as.character(format((1.1 * sum(my.df$Sets)), digits = 0)))
    
    if (hard == TRUE) {
      my.df$Sets <- (1 + my.df$Sets)
      time.target <- (time.target + 4)
    }
    if (light == TRUE) {
      my.df$Reps <- as.numeric(as.character(my.df$Reps))
      my.df$Reps <- my.df$Reps/1.2
      my.df$Reps <- format(my.df$Reps, digits = 0)
      time.target <- (time.target - 2)
    }
    
    print(my.df)
    
  }
  
  if (lower == TRUE) {
    
    
    
    legexercises  <- wodf[wodf$muscle_general == "legs",]
    fullexercises <- wodf[wodf$muscle_specific == "full",]
    
    selectedfull  <- fullexercises[sample(nrow(fullexercises),1),]
    selectedleg   <- legexercises[sample(nrow(legexercises), exercisecount - 1), ]
    
    my.df1        <- rbind(selectedfull, selectedleg)
    
    exerciseinfo  <- my.df1[c(2,3,1,4:exercisecount),]
    exerciseinfo$difficulty <- as.numeric(as.character(exerciseinfo$difficulty))
    
    my.df[,1] <- exerciseinfo$exercise_name
    my.df[,2] <- sample(4:5, 1)
    my.df[,3] <- exerciseinfo$difficulty
    my.df[,4] <- ((8/my.df$Sets) * (12/my.df$Difficulty))
    my.df[,4] <- format(my.df$Reps, digits = 0)
    my.df     <- subset(my.df, select = -c(Difficulty))
    
    time.target      <- as.numeric(as.character(format((1.1 * sum(my.df$Sets)), digits = 0)))
    
    if (hard == TRUE) {
      my.df$Sets <- (1 + my.df$Sets)
      time.target <- (time.target + 4)
    }
    if (light == TRUE) {
      my.df$Reps <- as.numeric(as.character(my.df$Reps))
      my.df$Reps <- my.df$Reps/1.2
      my.df$Reps <- format(my.df$Reps, digits = 0)
      time.target <- (time.target - 2)
    }
    
    print(my.df)
    
  }
  
  if (core == TRUE) {
    
    
    
    coreexercises   <- wodf[wodf$muscle_general == "core",]
    otherexercises  <- wodf[wodf$muscle_general != "core",]
    
    selectedcore    <- coreexercises[sample(nrow(coreexercises), 4),]
    selectedother   <- otherexercises[sample(nrow(otherexercises), 2),]
    
    my.df1          <- rbind(selectedcore, selectedother)
    
    exerciseinfo    <- my.df1[c(1,2,5,3,4,6),]
    exerciseinfo$difficulty   <- as.numeric(as.character(exerciseinfo$difficulty))
    
    my.df[,1] <- exerciseinfo$exercise_name
    my.df[,2] <- sample(4:5, 1)
    my.df[,3] <- exerciseinfo$difficulty
    my.df[,4] <- ((8/my.df$Sets) * (12/my.df$Difficulty))
    my.df[,4] <- format(my.df$Reps, digits = 0)
    my.df     <- subset(my.df, select = -c(Difficulty))
    
    time.target      <- as.numeric(as.character((1.5 * sum(my.df$Sets))))
    
    if (hard == TRUE) {
      my.df$Sets <- (1 + my.df$Sets)
      time.target <- (time.target + 4)
    }
    if (light == TRUE) {
      my.df$Reps <- as.numeric(as.character(my.df$Reps))
      my.df$Reps <- my.df$Reps/1.2
      my.df$Reps <- format(my.df$Reps, digits = 0)
      time.target <- (time.target - 2)
    }
    
    print(my.df)
    
  }
  
  msg         <- messages[sample(1:nrow(messages), 1),1]

  cat("Try and finish the workout in less than", time.target, "minutes. \n")
  cat(msg)
  
}


# ---- Potential Improvements ----

# (DONE)    Including a "goal completion time"

#           in the "all" workout type, preventing same muscle_general exercises back to back

# (DONE)    fine tuning the rep counts

# (DONE)    including workout types that involve holds rather than reps

#           make adjustment to allow for light, moderate, difficult workouts

# (DONE)    Add an encouragement message, different every day

#           Look into making an app? How does Shiny work?

#           Summary statistic of what the app generates - run it 100 times in each condition, identify:
              # average time of workout
              # frequency of each exercise over all workouts
              






