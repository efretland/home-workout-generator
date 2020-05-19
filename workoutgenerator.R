# Daily Workout Generator

# Library loading

# --- Loading Packages --- 
library(stringr)
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
                wodf$muscle_general == "shoulders" |
                wodf$muscle_general == "upper"]   <- "upper"

wodf$body_area[wodf$muscle_general == "legs" |
                wodf$muscle_general == "full" |
                wodf$muscle_general == "lower"]        <- "lower"

wodf$body_area[wodf$muscle_general == "core"]         <- "core"

wodf$body_area <- factor(wodf$body_area)


messages$message <- as.character(messages$message)


# Designating if a workout can only be done inside

wodf$exercise_name <- as.character(wodf$exercise_name)

wodf$inside <- 0


wodf %>%
  filter(str_detect(wodf$exercise_name, "rb"))

wodf$inside[str_detect(wodf$exercise_name, "rb") == TRUE]   <- 1

wodf$inside[str_detect(wodf$exercise_name, "superman") == TRUE]   <- 1




# ---- Steps ----

# Write a function providing option of "Upper", "Leg", "Core", "Full" workouts

# Structure:
# workoutgenerator <- function(x, Full = FALSE, Upper = FALSE, Lower = FALSE, Core = FALSE) {
# if full = TRUE { sample from all exercises }
# if upper = TRUE { sample 2 chest, 2 back, 2 bicep, 2 tricep }
# if lower = TRUE { sample 5 leg, 2 core }
# if core = TRUE { sample 4 core, 2 random}
# }

todaysworkout <- function(x, full = FALSE, upper = FALSE, lower = FALSE, core = FALSE, hard = FALSE, light = FALSE,  weights = FALSE, summary = FALSE) {
  
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

  if (weights == FALSE)  { wodf <- wodf[wodf$outside == 0,] }
  if (weights == TRUE)   { wodf <- wodf[wodf$inside == 0,] }
  
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
    
    if (summary == FALSE) {
    print(my.df)
    }
  }
  
  if (upper == TRUE) { 
    
    chestexercises  <- wodf[wodf$muscle_specific == "chest",]
    
    if (weights == FALSE) {
    backexercises   <- wodf[wodf$muscle_specific == "back" |
                              wodf$muscle_specific == "biceps",] 
    }
    
    if (weights == TRUE) {
    backexercises   <- wodf[wodf$muscle_specific == "back",] 
    }
    
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
    
    if (summary == FALSE) {
    print(my.df)
    }
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
    
    if (summary == FALSE) {
    print(my.df)
    }
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
    
    if (summary == FALSE) { 
      print(my.df) 
      }
  }
  
  msg         <- messages[sample(1:nrow(messages), 1),1]
  
  if (summary == FALSE) {
  cat("Try and finish the workout in less than", time.target, "minutes. \n")
  cat(msg)
  }
  
  if (summary == TRUE) { 
    return(time.target)
  }
}


# ---- Potential Improvements ----

# (DONE)    Including a "goal completion time"

#           in the "all" workout type, preventing same muscle_general exercises back to back

# (DONE)    fine tuning the rep counts

# (DONE)    including workout types that involve holds rather than reps

# (DONE)    make adjustment to allow for light, moderate, difficult workouts

# (DONE)    Add an encouragement message, different every day

#           Look into making an app - shiny.io

# (DONE)    Summary statistic of what the app generates - run it 100 times in each condition, identify:
              # average time of workout

#           Improve summary dataframe code
    # (DONE)  instead of rewriting the todaysworkout function, figure out a way to change the output
              # create a dataframe that includes logical conditions to write a loop to generate summary df
              

# (DONE)    Include a way to filter exercises that can/cannot be done outside (using weights)




# ---- Workout Time Summary Function ----


# Generating Summary DF

n <- 100

summary.df <- data.frame(matrix(NA, nrow = n, ncol = 24))

colnames(summary.df) <- c("U", "U.H", "U.L", "U.H.W", "U.W", "U.L.W",
                          "L", "L.H", "L.L", "L.H.W", "L.W", "L.L.W",
                          "C", "C.H", "C.L", "C.H.W", "C.W", "C.L.W",
                          "F", "F.H", "F.L", "F.H.W", "F.W", "F.L.W")

summary.df[,1] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, upper = TRUE)))
summary.df[,2] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, upper = TRUE, hard = TRUE)))
summary.df[,3] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, upper = TRUE, light = TRUE)))
summary.df[,4] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, upper = TRUE, weights = TRUE)))
summary.df[,5] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, upper = TRUE, hard = TRUE, weights = TRUE)))
summary.df[,6] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, upper = TRUE, light = TRUE, weights = TRUE)))
summary.df[,7] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, lower = TRUE)))
summary.df[,8] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, lower = TRUE, hard = TRUE)))
summary.df[,9] <- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, lower = TRUE, light = TRUE)))
summary.df[,10]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, lower = TRUE, weights = TRUE)))
summary.df[,11]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, lower = TRUE, hard = TRUE, weights = TRUE)))
summary.df[,12]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, lower = TRUE, light = TRUE, weights = TRUE)))
summary.df[,13]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, core = TRUE)))
summary.df[,14]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, core = TRUE, hard = TRUE)))
summary.df[,15]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, core = TRUE, light = TRUE)))
summary.df[,16]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, core = TRUE, weights = TRUE)))
summary.df[,17]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, core = TRUE, hard = TRUE, weights = TRUE)))
summary.df[,18]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, core = TRUE, light = TRUE, weights = TRUE)))
summary.df[,19]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, full = TRUE)))
summary.df[,20]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, full = TRUE, hard = TRUE)))
summary.df[,21]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, full = TRUE, light = TRUE)))
summary.df[,22]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, full = TRUE, weights = TRUE)))
summary.df[,23]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, full = TRUE, hard = TRUE, weights = TRUE)))
summary.df[,24]<- unlist(lapply(seq_len(n), function(x) todaysworkout(summary = TRUE, full = TRUE, light = TRUE, weights = TRUE)))

# Potential area for improvement - create a dataframe with logical situations ("upper = TRUE, hard = TRUE") 
  # to allow for creation of a loop that can run this without 24 lines of repeated code.

head(summary.df)

stat.df <- data.frame(matrix(NA, nrow = 1, ncol = 24))

for (i in 1:ncol(summary.df)) {
  stat.df[,i] <- (sum(summary.df[,i]) / nrow(summary.df))
}

print(stat.df)

colnames(stat.df) <- colnames(summary.df)
# summary.df <- rbind(stat.df, summary.df)
# rownames(summary.df) <- c("Summary", 1:100)
summaries <- stat.df

# View(summaries)

