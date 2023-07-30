import Data.List (find)

data Course = Course
  { courseName :: String
  , prerequisites :: [String]
  } deriving (Eq, Show)

checkPrerequisites :: [String] -> Course -> Bool
checkPrerequisites userTokens course = all (`elem` userTokens) (prerequisites course)

type PaymentResult = Bool

validatePayment :: Bool -> PaymentResult
validatePayment isPaymentSuccessful = isPaymentSuccessful

enrollUser :: [String] -> Course -> Bool -> String
enrollUser userTokens course isPaymentSuccessful
  | not (checkPrerequisites userTokens course) = "You are not qualified for this course"
  | validatePayment isPaymentSuccessful = "Enrolled successfully"
  | otherwise = "Payment failed"

browseCourses :: [Course]
browseCourses =
  [ Course { courseName = "Plutus Programming Course", prerequisites = ["Basic Haskell"] }
  , Course { courseName = "Advanced Haskell Course", prerequisites = ["Intermediate Haskell"] }
  , Course { courseName = "Web Development with Haskell", prerequisites = ["Basic Haskell"] }
  -- Add more courses
  ]

main :: IO ()
main = do
  let userTokens = ["Basic Haskell"]

  putStrLn "Available Courses:"
  mapM_ (putStrLn . courseName) browseCourses

  putStrLn "Enter the name of the course you want to enroll in:"
  selectedCourseName <- getLine

  let maybeSelectedCourse = find (\c -> courseName c == selectedCourseName) browseCourses

  case maybeSelectedCourse of
    Nothing -> putStrLn "Course not found."
    Just selectedCourse -> do
      putStrLn $ enrollUser userTokens selectedCourse True 
