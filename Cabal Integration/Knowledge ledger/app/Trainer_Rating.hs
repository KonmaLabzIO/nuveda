module Trainer_Rating
    ( User(..),
      Trainer(..),
      TrainerRatings,
      rateTrainer,
      viewTrainerDetails
    ) where

import Data.List (find)

data User = User { userName :: String, completedCourses :: [String], userToken :: Int }
data Trainer = Trainer { trainerName :: String, trainerRatings :: [(User, Int)] }
type TrainerRatings = [Trainer]

data TrainerCourse = TrainerCourse { tcId :: Int, tcName :: String }  -- Rename the Course data type

rateTrainer :: User -> String -> Int -> TrainerRatings -> TrainerRatings
rateTrainer user trainerName rating trainerRatings = updatedTrainerRatings
  where
    updatedTrainerRatings = case matchingTrainer of
        Just (Trainer name ratings) -> (Trainer name $ ratings ++ [(user, rating)]) : otherTrainers
        Nothing -> trainerRatings
    otherTrainers = filter (\(Trainer name _) -> name /= trainerName) trainerRatings
    matchingTrainer = find (\(Trainer name _) -> name == trainerName) trainerRatings

calculateAverageRating :: [(User, Int)] -> String
calculateAverageRating ratings
    | totalRatings == 0 = "Average Rating: No ratings yet"
    | otherwise = "Average Rating: " ++ show (fromIntegral totalRating / fromIntegral totalRatings)
  where
    (totalRating, totalRatings) = foldl (\(total, count) (_, rating) -> (total + rating, count + 1)) (0, 0) ratings

viewTrainerDetails :: String -> TrainerRatings -> String
viewTrainerDetails trainerName trainerRatings =
    case find (\(Trainer name _) -> name == trainerName) trainerRatings of
        Just (Trainer _ ratings) ->
            "Trainer: " ++ trainerName ++ "\n" ++
            calculateAverageRating ratings ++ "\n" ++
            "Ratings:\n" ++
            unlines (map (\(user, rating) -> userName user ++ ": " ++ show rating) ratings)
        Nothing -> "Trainer not found"
