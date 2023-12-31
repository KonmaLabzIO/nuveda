import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- Data types for courses, users, certificates, and tokens
data Course = Course
  { courseId :: Int,
    courseName :: String
  }
  deriving (Show)

data User = User
  { userName :: String,
    completedCourses :: [Course]
  }
  deriving (Show)

data Certificate = Certificate
  { certificateId :: String,
    issuedCourse :: Course,
    recipient :: String
  }
  deriving (Show)

data Token = Token
  { tokenId :: String,
    tokenCourse :: Course,
    owner :: String
  }
  deriving (Show)

-- Function to get the current Unix timestamp (in seconds)
getCurrentUnixTimestamp :: IO Integer
getCurrentUnixTimestamp = do
  currentTime <- getCurrentTime
  return $ floor $ utcTimeToPOSIXSeconds currentTime

-- Function to tokenize a certificate
tokenizeCertificate :: User -> Certificate -> IO Token
tokenizeCertificate user certificate = do
  currentUnixTimestamp <- getCurrentUnixTimestamp
  let tokenId = "Token_" ++ certificateId certificate ++ "_" ++ show currentUnixTimestamp
      token = Token
        { tokenId = tokenId,
          tokenCourse = issuedCourse certificate,
          owner = recipient certificate
        }
  return token

main :: IO ()
main = do
  putStrLn "Enter your name:"
  userName <- getLine
  
  putStrLn "Enter the number of completed courses:"
  numCompletedCoursesStr <- getLine
  let numCompletedCourses = read numCompletedCoursesStr :: Int
  
  completedCourses <- mapM (\i -> do
    putStrLn $ "Enter course ID for completed course and course name " ++ show i ++ ":"
    courseIdStr <- getLine
    coursename <- getLine
    let courseId = read courseIdStr
    return Course { courseId = courseId, courseName = coursename }
   ) [1..numCompletedCourses]
  
  let sampleUser = User {userName = userName, completedCourses = completedCourses}
      
      sampleCertificate = Certificate {certificateId = "Cert123", issuedCourse = head completedCourses, recipient = userName}

  tokenizedCertificate <- tokenizeCertificate sampleUser sampleCertificate
  putStrLn $ "Tokenized certificate: " ++ show tokenizedCertificate
