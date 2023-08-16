module FirstContract (runFirstContract) where

import Data.List (intercalate, find)
import Course_Enrollment
    ( Course(..),
      User(..),
      allCourses,
      printCourse,
      enrollInCourse,
      prerequisites,
      coursePrize,
      completedCourses,
      courseName,
    )
import Royalty_Distribution
    ( ContentCreator(..),
      distributeRoyalties,
      getContentCreatorsFromUser,
    )
import System.Exit (exitFailure)

runFirstContract :: IO ()
runFirstContract = do
    
    putStrLn ""
    putStrLn "Course Enrollment"
    putStrLn ""
    putStrLn "Enter your name:"
    userName <- getLine
    let user = User { userName = userName, completedCourses = [] }
    putStrLn "Available courses:"
    mapM_ printCourse allCourses
    putStrLn "Enter the course ID to enroll:"
    courseIdStr <- getLine
    let courseId = read courseIdStr :: Int
    case find (\course -> courseId == courseIdOf course) allCourses of
        Just selectedCourse -> do
            putStrLn $ "Prerequisites: " ++ intercalate ", " (prerequisites selectedCourse)
            putStrLn "Do you meet the prerequisites? (yes/no):"
            prerequisitesAnswer <- getLine
            if prerequisitesAnswer == "no"
                then do
                    putStrLn "Enrollment stopped due to prerequisites not met."
                    putStrLn "Exiting..."
                    exitFailure  -- Terminate the whole program
                else do
                    enrolledUser <- enrollInCourse user selectedCourse
                    putStrLn "Enrollment successful!"
                    print enrolledUser
                    executeRoyaltyDistribution enrolledUser
        Nothing -> do
            putStrLn "Invalid course ID"
            putStrLn "Exiting..."
            exitFailure  -- Terminate the whole program
    where
        courseIdOf course = courseId course

executeRoyaltyDistribution :: User -> IO ()
executeRoyaltyDistribution user = do
    putStrLn ""
    putStrLn "Royalty Distribution"
    putStrLn ""
    let enrolledCourse = head $ completedCourses user
    let revenue = coursePrize enrolledCourse
    creators <- getContentCreatorsFromUser
    let royalties = distributeRoyalties revenue creators
    putStrLn "Royalty distribution:"
    print royalties
