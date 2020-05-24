# home-workout-generator

Home workout generator (from exercises done with minimal equipment)

This workout generator is a script that, when run, creates a comprehensive body-weight workout that takes anywhere from 30-50 minutes to complete. This project stems from the need to quickly come up with effective, thorough, and non-repetitive workouts that are doable at home.
An important component is that most of these exercises don't require any equipment. However, some of the exercises do involve using resistance bands or makeshift weights, and this function provides the capability of selecting workouts with and without equipment.
The final product of the script is a function that can generate any of four kinds of workout - core, upper body, lower body, or full body.


Includes:
Exercise data (csv with variables: exercise_name, muscle_general, muscle_specific, cardio, difficulty, outside)
Messages data (csv with a list of encouraging messages that the function selects from)
R script to write functions to generate workout

Final output of the todaysworkout() function is:

- Dataframe with 3 columns (exercise name, number of sets, number of reps)
- Message with target completion time
- Message with words of encouragement

A secondary function included in this script is a function that outputs an "average target time" for every possible workout type that the todaysworkout() function can generate.
