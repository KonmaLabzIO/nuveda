module Course_Enrollment (
  Course(..),
  User(..),
  allCourses,
  printCourse,
  enrollInCourse
) where

import Data.List (intercalate, find)

data Course = Course {
  courseId :: Int,
  courseName :: String,
  coursePrize :: Double,
  prerequisites :: [String]
}
 deriving (Show)

data User = User
  { userName :: String,
    completedCourses :: [Course]
  }
  deriving (Show)

course1 :: Course
course1 = Course {
  courseId = 1,
  courseName = "Advance Haskell",
  coursePrize = 100.0,
  prerequisites = ["basicHaskell"]
}

course2 :: Course
course2 = Course {
  courseId = 2,
  courseName = "Plutus",
  coursePrize = 150.0,
  prerequisites = ["Advance Haskell", "BasicHaskell"]
}

course3 :: Course
course3 = Course {
  courseId = 3,
  courseName = "Marlowe",
  coursePrize = 120.0,
  prerequisites = ["Plutus"]
}

allCourses :: [Course]
allCourses = [course1, course2, course3]

printCourse :: Course -> IO ()
printCourse course = do
  putStrLn $ "Course ID: " ++ show (courseId course)
  putStrLn $ "Course Name: " ++ courseName course
  putStrLn $ "Course Price: " ++ show (coursePrize course)
  putStrLn "Prerequisites:"
  if null (prerequisites course)
    then putStrLn "None"
    else putStrLn $ "Prerequisites: " ++ intercalate ", " (prerequisites course)

  putStrLn "------------------------"

enrollInCourse :: User -> Course -> IO User
enrollInCourse user course = do
  putStrLn "Enrollment confirmed."
  return $ user { completedCourses = course : completedCourses user }

  
