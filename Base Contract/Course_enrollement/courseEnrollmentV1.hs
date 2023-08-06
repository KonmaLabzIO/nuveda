import Data.List (find)

data Course = Course
  { courseId :: Int
  , courseName :: String
  , prerequisites :: [String]
  , courseRating :: Int
  , courseReviews :: [String]
  } deriving (Eq, Show)

data User = User
  { userName :: String
  , userPrerequisites :: [String]
  , userBalance :: Int
  } deriving (Eq, Show)

type PaymentResult = Bool

validatePayment :: Int -> PaymentResult
validatePayment paymentAmount = paymentAmount == 1000

enrollUser :: User -> Course -> Int -> Bool -> String
enrollUser user course paymentAmount isPaymentSuccessful
  | not (checkPrerequisites (userPrerequisites user) course) = "You don't have the required prerequisites for this course."
  | paymentAmount /= 1000 = "Insufficient payment. The course fee is 1000 ada."
  | isPaymentSuccessful = "Enrolled successfully. You now have access to the course."
  | otherwise = "Payment failed. Please try again later or contact customer support."

checkPrerequisites :: [String] -> Course -> Bool
checkPrerequisites userPrereqs course = all (`elem` userPrereqs) (prerequisites course)

viewCourseDetails :: Course -> String
viewCourseDetails course =
  "Course ID: " ++ show (courseId course) ++
  "\nCourse Name: " ++ courseName course ++
  "\nPrerequisites: " ++ show (prerequisites course) ++
  "\nRating: " ++ show (courseRating course) ++ "/5" ++
  "\nReviews: " ++ unlines (courseReviews course)

main :: IO ()
main = do
  let user = User "Saad" ["Basic Haskell"] 1000

  let courses = [ Course 101 "Plutus Programming Course" ["Basic Haskell"] 5 ["Excellent course!", "Highly recommended."]
                , Course 102 "Web Development with Haskell" ["Basic Haskell"] 4 ["Great course!", "Enjoyed learning."]
                , Course 103 "Advanced Haskell Course" ["Intermediate Haskell"] 4 ["Challenging but worth it."]
                ]
  
  putStrLn "Available Courses with Ratings and Reviews:"
  mapM_ (\c -> putStrLn $ viewCourseDetails c) courses

  putStrLn "Enter the course ID you want to enroll in:"
  courseIdInput <- readLn

  putStrLn "Enter the payment amount (in ada) to enroll in the course:"
  paymentAmount <- readLn

  let maybeSelectedCourse = find (\c -> courseId c == courseIdInput) courses

  case maybeSelectedCourse of
    Nothing -> putStrLn "Course not found."
    Just selectedCourse -> do
      let isPaymentSuccessful = validatePayment paymentAmount
      putStrLn $ enrollUser user selectedCourse paymentAmount isPaymentSuccessful
