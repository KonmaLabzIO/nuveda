module Certificate_Insurance
    ( Course(..)
    , User(..)
    , Certificate(..)
    , Token(..)
    , issueCertificate
    , generateToken
    , viewCertificate
    , viewToken
    , downloadCertificate
    , loginUser
    , executeCertificateInsuranceContract
    ) where

data Course = Course
    { courseName :: String
    , prerequisites :: [String]
    , courseRating :: Int
    , courseReviews :: [String]
    } deriving (Eq, Show)

data User = User
    { userName :: String
    , userPrerequisites :: [String]
    , userBalance :: Int
    , userCompletedCourses :: [Course]
    } deriving (Eq, Show)

data Certificate = Certificate
    { certificateId :: Int
    , certificateCourse :: Course
    , certificateOwner :: String
    } deriving (Eq, Show)

data Token = Token
    { tokenCourse :: Course
    , tokenOwner :: String
    } deriving (Eq, Show)

issueCertificate :: User -> Course -> Certificate
issueCertificate user course =
    let certificateId = length (userName user) * 100 -- A simple certificate ID generation example
        certificate = Certificate certificateId course (userName user)
    in certificate

generateToken :: User -> Course -> Token
generateToken user course = Token course (userName user)

viewCertificate :: User -> Certificate -> String
viewCertificate user certificate
    | userName user == certificateOwner certificate = "Certificate ID: " ++ show (certificateId certificate) ++ "\nCourse: " ++ courseName (certificateCourse certificate)
    | otherwise = "You are not authorized to view this certificate."

viewToken :: User -> Token -> String
viewToken user token
    | userName user == tokenOwner token = "Token for Course: " ++ courseName (tokenCourse token)
    | otherwise = "You are not authorized to view this token."

downloadCertificate :: User -> Certificate -> String
downloadCertificate user certificate
    | userName user == certificateOwner certificate = "Downloading Certificate...\nCongratulations! You have completed the course.\n" ++ "Certificate issued for " ++ userName user ++ " for completing " ++ courseName (certificateCourse certificate) ++ "\nYou can view and download your certificate from your profile."
    | otherwise = "You are not authorized to download this certificate."

loginUser :: IO User
loginUser = do
    putStrLn "Enter your username:"
    username <- getLine
    return $ User username ["Basic Haskell"] 1000 []

executeCertificateInsuranceContract :: IO ()
executeCertificateInsuranceContract = do
    putStrLn ""
    putStrLn "Certificate Insurance"
    putStrLn ""
    user <- loginUser
    let issuedCertificate = issueCertificate user (Course "Basic Haskell" [] 5 [])
    putStrLn $ viewCertificate user issuedCertificate
    let generatedToken = generateToken user (Course "Data Structures" [] 4 [])
    putStrLn $ viewToken user generatedToken
    putStrLn $ downloadCertificate user issuedCertificate