import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- Data types for courses and users
data Course = Course
  { courseId :: Int,
    courseName :: String,
    coursePrice :: Double,
    prerequisites :: [Course]
  }
  deriving (Show)

data User = User
  { userName :: String,
    completedCourses :: [Course],
    userSubscriptions :: [Subscription]
  }
  deriving (Show)

data Subscription = Subscription
  { subscriptionId :: String,
    subscriberAddress :: String,
    subscriptionPrice :: Double,
    subscriptionStartDate :: Integer, -- Unix timestamp (seconds)
    subscriptionPeriod :: Integer, -- Duration in seconds (e.g., 1 year)
    renewalMechanism :: Integer -- Duration in seconds (e.g., 1 year)
  }
  deriving (Show)

-- Function to get the current Unix timestamp (in seconds)
getCurrentUnixTimestamp :: IO Integer
getCurrentUnixTimestamp = do
  currentTime <- getCurrentTime
  return $ floor $ utcTimeToPOSIXSeconds currentTime

enrollInCourse :: User -> Course -> User
enrollInCourse user course = enrollInSingleCourse user course


-- Function to check if the user has completed the prerequisite courses
hasCompletedPrerequisites :: User -> [Course] -> Bool
hasCompletedPrerequisites user courses =
  all (\prereq -> prereq `elem` completedCourseIds) prerequisiteCourseIds
  where
    completedCourseIds = map courseId (completedCourses user)
    prerequisiteCourseIds = map courseId courses

-- Function to check if a subscription is valid based on the current date
isSubscriptionValid :: Subscription -> IO Bool
isSubscriptionValid Subscription { subscriptionStartDate = startDate, subscriptionPeriod = period } = do
  currentUnixTimestamp <- getCurrentUnixTimestamp
  return $ currentUnixTimestamp <= startDate + period

-- Sample courses data
haskellCourse :: Course
haskellCourse =
  Course
    { courseId = 1,
      courseName = "Haskell",
      coursePrice = 0.0,
      prerequisites = []
    }

advancedHaskellCourse :: Course
advancedHaskellCourse =
  Course
    { courseId = 2,
      courseName = "Advanced Haskell",
      coursePrice = 100.0,
      prerequisites = [haskellCourse]
    }

-- Sample users data
sampleUser :: User
sampleUser =
  User
    { userName = "John",
      completedCourses = [],
      userSubscriptions =
        [ Subscription
            { subscriptionId = "EveHsk",
              subscriberAddress = "addr1qy98k9u6q6yne5s66alxtg82pa0cf6kggplfmrrkh6ceamlhd0h2tx57v4rs204ggvz35txt3e7y26dgg6yq6lah8zsqs4pg7i",
              subscriptionPrice = 2500.0, -- In ADA
              subscriptionStartDate = 1677865600, -- 01/01/2023 00:00:00 (Unix timestamp)
              subscriptionPeriod = 31536000, -- 1 year in seconds
              renewalMechanism = 31536000 -- 1 year in seconds
            }
        ]
    }

-- Function to enroll in a course
enrollInSingleCourse :: User -> Course -> User
enrollInSingleCourse user course
  | not (hasCompletedPrerequisites user (prerequisites course)) = user -- Enrollment failed due to missing prerequisites
  | courseId course `elem` completedCourseIds = user -- Already enrolled in the course
  | otherwise = user {completedCourses = course : completedCourses user}
  where
    completedCourseIds = map courseId (completedCourses user)


-- Main function to demonstrate subscription management
main :: IO ()
main = do
  -- Sample course enrollment
  let userEnrolled = enrollInSingleCourse sampleUser haskellCourse
  putStrLn $ "User after course enrollment: " ++ show userEnrolled

  -- Sample subscription validity check
  let sampleSubscription = head (userSubscriptions userEnrolled)
  isSubscriptionValidResult <- isSubscriptionValid sampleSubscription
  putStrLn $ "Is the subscription valid? " ++ show isSubscriptionValidResult
