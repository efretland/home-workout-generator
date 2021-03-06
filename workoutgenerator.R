# Daily Workout Generator

# ---- Setup ----

# --- Loading Packages --- 
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(tidyverse)
library(e1071)

# --- Loading Data ---
setwd("C:/Users/User/Documents/Data Science Projects/Workout Generator")

wodf      <- read.csv("exercisesdataset.csv")
wodf      <- data.frame(wodf)
messages  <- read.csv("motivational_messages.csv")

# --- Viewing Data --- 
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
                wodf$muscle_general == "upper"]       <- "upper"
wodf$body_area[wodf$muscle_general == "legs" |
                wodf$muscle_general == "full" |
                wodf$muscle_general == "lower"]       <- "lower"
wodf$body_area[wodf$muscle_general == "core"]         <- "core"
wodf$body_area    <- factor(wodf$body_area)

messages$message  <- as.character(messages$message)


# Indoor or Outdoor Exercise

wodf$exercise_name  <- as.character(wodf$exercise_name)
wodf$inside         <- 0
wodf$inside[str_detect(wodf$exercise_name, "rb") == TRUE]         <- 1
wodf$inside[str_detect(wodf$exercise_name, "superman") == TRUE]   <- 1




# ---- Workout Function ----

# Write a function providing option of "Upper", "Leg", "Core", "Full" workouts

# Structure:
# workoutgenerator <- function(x, Full = FALSE, Upper = FALSE, Lower = FALSE, Core = FALSE) {
# if full = TRUE { sample from all exercises }
# if upper = TRUE { sample 2 chest, 2 back, 2 bicep, 2 tricep }
# if lower = TRUE { sample n leg, 2 core }
# if core = TRUE { sample 4 core, 2 random}
# }

todaysworkout <- function(x, full = FALSE, upper = FALSE, lower = FALSE, core = FALSE, 
                          hard = FALSE, light = FALSE,  weights = FALSE, summary = FALSE) {
  
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
  
  msg <- messages[sample(1:nrow(messages), 1),1]
  
  if (summary == FALSE) {
  cat("Try and finish the workout in less than", time.target, "minutes. \n")
  cat(msg)
  }
  
  if (summary == TRUE) { 
    print(time.target)
  }
}




# ---- Workout Time Summary Function ----


# Generating Summary DF


# Building logical dataframe to satisfy all todaysworkout() arg combinations

args.df <- data.frame(matrix(NA, nrow = 24, ncol = 8))
colnames(args.df) <- c("Upper", "Lower", "Core", "Full", "Hard", "Light", "Weights", "Summary")

args.df$Summary           <- TRUE
args.df$Upper[1:6]        <- TRUE
args.df$Upper[7:24]       <- FALSE

args.df$Lower[1:6]        <- FALSE
args.df$Lower[13:24]      <- FALSE
args.df$Lower[7:12]       <- TRUE

args.df$Core[1:12]        <- FALSE
args.df$Core[19:24]       <- FALSE
args.df$Core[13:18]       <- TRUE

args.df$Full[1:18]        <- FALSE
args.df$Full[19:24]       <- TRUE

args.df$Weights[1:3]      <- FALSE
args.df$Weights[4:6]      <- TRUE
args.df$Weights[7:9]      <- FALSE
args.df$Weights[10:12]    <- TRUE
args.df$Weights[13:15]    <- FALSE
args.df$Weights[16:18]    <- TRUE
args.df$Weights[19:21]    <- FALSE
args.df$Weights[22:24]    <- TRUE

args.df$Hard  <- FALSE
args.df$Light <- FALSE

args.df$Hard[c(2,5,8,11,14,17,20,23)]   <- TRUE
args.df$Light[c(3,6,9,12,15,18,21,24)]  <- TRUE



n <- 100
df1 <- data.frame(matrix(NA, nrow = n, ncol = 24))

for (i in 1:nrow(args.df)) {
  
  for (j in 1:nrow(df1)){
  df1[j,i] <- todaysworkout(upper = args.df$Upper[i],
                lower = args.df$Lower[i],
                core = args.df$Core[i],
                full = args.df$Full[i],
                hard = args.df$Hard[i],
                light = args.df$Light[i],
                weights = args.df$Weights[i],
                summary = args.df$Summary[i])
  }
}

colnames(df1) <- c("U", "U.H", "U.L", "U.W", "U.H.W", "U.L.W",
                   "L", "L.H", "L.L", "L.W", "L.H.W", "L.L.W",
                   "C", "C.H", "C.L", "C.W", "C.H.W", "C.L.W",
                   "F", "F.H", "F.L", "F.W", "F.H.W", "F.L.W")


stat.df <- data.frame(matrix(NA, nrow = 1, ncol =24))
colnames(stat.df) <- colnames(df1)

for (i in 1:ncol(df1)) {
  stat.df[,i] <- (sum(df1[,i]) / nrow(df1))
}

stat.df <- t(stat.df)
stat.df <- as.data.frame(stat.df)
stat.df$type <- NA
# stat.df$type <- rep(1:6, 4)
stat.df$type[1:6] <- "Upper"
stat.df$type[7:12] <- "Lower"
stat.df$type[13:18] <- "Core"
stat.df$type[19:24] <- "Full"

stat.df$Args <- colnames(df1)

stat.df$Gen.Args <- rep(c("Base", "Heavy", "Light", "Weights", "Weights, Heavy", "Weights, Light"), 4)
colnames(stat.df) <- c("Avg.Time", "Type", "Args", "Gen.Args")
stat.df <- stat.df[,c("Type", "Gen.Args", "Avg.Time")]

stat.df <- spread(stat.df, Gen.Args, Avg.Time)

# ---- Average Workout Times ---- 
print(stat.df)




# ---- Continued Development ----

# (DONE)    Including a "goal completion time"

# (DONE)    fine tuning the rep counts

# (DONE)    including workout types that involve holds rather than reps

# (DONE)    make adjustment to allow for light, moderate, difficult workouts

# (DONE)    Add an encouragement message, different every day

# (DONE)    Summary statistic of what the app generates - run it 100 times in each condition, identify:
              # average time of workout

# (DONE)    Improve summary dataframe code
              # instead of rewriting the todaysworkout function, figure out a way to change the output
              # create a dataframe that includes logical conditions to write a loop to generate summary df

# (DONE)    Include a way to filter exercises that can/cannot be done outside (using weights)



#    -      In the "all" workout type, preventing same muscle_general exercises back to back

#    -      Look into making an app - shiny.io


