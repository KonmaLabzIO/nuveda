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
  prerequisites = ["baiscHaskell"]
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
  putStrLn $ "Have you completed the prerequisites for course " ++ courseName course ++ "? (yes/no)"
  confirmation <- getLine
  if confirmation == "yes"
    then do
      putStrLn "Enrollment confirmed."
      return $ user { completedCourses = course : completedCourses user }
    else do
      putStrLn "Enrollment canceled."
      return user

main :: IO ()
main = do
  putStrLn "Enter your name:"
  userName <- getLine
  let sampleUser = User { userName = userName, completedCourses = [] }
  putStrLn "Available courses:"
  mapM_ printCourse allCourses
  putStrLn "Enter the course ID to enroll (or 0 to exit):"
  courseIdStr <- getLine
  let courseId = read courseIdStr :: Int
  if courseId == 0
    then putStrLn "Goodbye!"
    else case find (\course -> courseId == courseIdOf course) allCourses of
      Just selectedCourse -> do
        updatedUser <- enrollInCourse sampleUser selectedCourse
        putStrLn $ "Updated user: " ++ show updatedUser
      Nothing -> putStrLn "Invalid course ID"
  where
    courseIdOf course = courseId course
