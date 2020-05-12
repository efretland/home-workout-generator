# home-workout-generator

Home workout generator (from exercises done with minimal equipment)

This workout generator is a script that, when run, creates a comprehensive body-weight workout that should take anywhere from 30-50 minutes to complete. This project stems from the need to quickly come up with effective, thorough, and non-repetitive workouts that are doable at home.
An important component is that none of these exercises require any equipment aside from a few that require a resistance band.
The final product of the script is a function that can generate any of four kinds of workout - core, upper body, lower body, or full body.


Includes:
Source data (csv with variables: exercise_name, muscle_general, muscle_specific, cardio, difficulty)
R script to write functions to generate workout

Final output of the todaysworkout() function is:

- Dataframe with 3 columns (exercise name, number of sets, number of reps)
- Text message with target completion time
- Text message with words of encouragement
