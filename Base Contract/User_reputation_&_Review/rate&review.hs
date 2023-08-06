import Data.List (find)

data User = User { userName :: String, completedCourses :: [Course], userToken :: Int }
data Course = Course { courseId :: Int, courseName :: String, prerequisites :: [String], courseRating :: Int, courseReviews :: [Review] }
data Instructor = Instructor { instructorName :: String }
data Review = Review { reviewRating :: Int, reviewText :: String }

type CourseReviews = [(Course, [Review])]

getInstructor :: Course -> CourseReviews -> Maybe Instructor
getInstructor course courseReviews = case matchingInstructors of
  [(c, Review _ instructor)] | c `courseMatches` course -> Just (Instructor instructor)
  _ -> Nothing
  where
    matchingInstructors = [(c, r) | (c, reviews) <- courseReviews, r@(Review _ _) <- reviews, c `courseMatches` course]

viewCourseDetails :: Course -> CourseReviews -> String
viewCourseDetails course courseReviews =
  "Course: " ++ courseName course ++
  "\nPrerequisites: " ++ show (prerequisites course) ++
  "\nRating: " ++ show (courseRating course) ++
  (case getInstructor course courseReviews of
    Just instructor -> "\nInstructor: " ++ instructorName instructor
    Nothing -> "\nInstructor information not available"
  ) ++
  "\nReviews:\n" ++ unlines (map formatReview reviews) ++
  "------------------------------------"
  where
    reviews = findReviewsForCourse course courseReviews

findReviewsForCourse :: Course -> CourseReviews -> [Review]
findReviewsForCourse course courseReviews = case matchingReviews of
  Just (_, reviews) -> reviews
  Nothing -> []
  where
    matchingReviews = find (\(c, _) -> c `courseMatches` course) courseReviews

formatReview :: Review -> String
formatReview review = "Rating: " ++ show (reviewRating review) ++ " - " ++ reviewText review

courseMatches :: Course -> Course -> Bool
courseMatches (Course _ name1 _ _ _) (Course _ name2 _ _ _) = name1 == name2

leaveReview :: User -> Course -> Instructor -> Int -> String -> CourseReviews -> CourseReviews
leaveReview user course instructor rating reviewText courseReviews = updatedCourseReviews
  where
    updatedCourseReviews = case matchingCourse of
      Just (c, reviews) -> (newCourse, reviews ++ [Review rating reviewText]) : otherCourses
      Nothing -> courseReviews
    otherCourses = filter (\(c, _) -> not (c `courseMatches` course)) courseReviews
    matchingCourse = find (\(c, _) -> c `courseMatches` course) courseReviews
    newCourse = case matchingCourse of
      Just (c, _) -> c { courseReviews = reviews ++ [Review rating reviewText], courseRating = newRating }
      Nothing -> course { courseReviews = [Review rating reviewText], courseRating = rating }
      where
        reviews = findReviewsForCourse course courseReviews
        newRating = case reviews of
          [] -> rating
          _ -> (sum (map reviewRating reviews) + rating) `div` (length reviews + 1)


main :: IO ()
main = do
  putStrLn "Welcome to the Course Rating System!"
  putStrLn "Please enter your name: "
  userNameInput <- getLine

  -- Assume the user has completed some courses already (you can modify this as needed)
  let completedCourses = [Course 1 "Basic Haskell" [] 5 [], Course 3 "Data Structures" [] 4 []]

  -- Get the user's token (you can modify this as needed)
  let userToken = 1000

  let user = User userNameInput completedCourses userToken
  putStrLn "User logged in successfully!\n"

  -- Now ask the user to enter the details for the new course review
  putStrLn "Please enter the name of the course you want to review: "
  courseNameInput <- getLine

  putStrLn "Please enter the rating for the course (0-5): "
  ratingInput <- getLine
  let rating = read ratingInput :: Int

  putStrLn "Please enter your review for the course: "
  reviewText <- getLine

  -- Create the new Course object with the user input
  let newCourse = Course (length completedCourses + 1) courseNameInput [] rating [Review rating reviewText]

  -- Assume the instructor is fixed for now
  let instructor = Instructor "Santanu Chaterjee"

  -- Add the new course review to the existing list
  let updatedCourseReviews = leaveReview user newCourse instructor rating reviewText []

  putStrLn "\nThank you for your review!\n"

  -- Display the updated course details and reviews
  putStrLn $ viewCourseDetails newCourse updatedCourseReviews
