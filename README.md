# home-workout-generator

Home workout generator (from exercises done with minimal equipment)

This workout generator is a script that, when run, creates a comprehensive body-weight workout that should take about 45 minutes to complete. This project stems from the need to quickly come up with effective, thorough, and non-repetitive workouts that are doable at home.
An important component is that none of these exercises require any equipment aside from a few that require a resistance band.
The final product of the script is a function that can generate any of four kinds of workout - core, upper body, lower body, or full body.


Includes:
Source data (csv with variables: exercise_name, muscle_general, muscle_specific, cardio, difficulty)
R script to write functions to generate workout

Final output of the todaysworkout() function is:

> todaysworkout(upper = TRUE)
               Exercise Sets Reps
1               pushups    5   10
2         w supermans      5   19
3    close grip pushups    5    6
4     rb curls moderate    5   10
5              t pushup    5   10
6           y supermans    5   19
7    cross grip pushups    5    6
8 rb hammer curls heavy    5    6
Keep up the pace throughout the workout!
