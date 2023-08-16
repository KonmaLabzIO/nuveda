module Progress_Tracking
    ( Module(..)
    , Assessment(..)
    , Achievement(..)
    , LearningProgress(..)
    , getModulesFromUser
    , getAssessmentsFromUser
    , getAchievementsFromUser
    , executeProgressTrackingContract  
    ) where

data Module = Module
    { moduleName :: String
    , isCompleted :: Bool
    } deriving Show

data Assessment = Assessment
    { assessmentName :: String
    , score :: Int
    } deriving Show

data Achievement = Achievement
    { achievementName :: String
    , description :: String
    } deriving Show

data LearningProgress = LearningProgress
    { userId :: Int
    , completedModules :: [Module]
    , completedAssessments :: [Assessment]
    , achievements :: [Achievement]
    } deriving Show

-- Function to gather module details from user input
getModulesFromUser :: Int -> IO [Module]
getModulesFromUser n = go 1 n []
    where
        go _ 0 acc = return (reverse acc)
        go currentNum remaining acc = do
            putStrLn $ "Enter the name of module " ++ show currentNum ++ ": "
            name <- getLine
            putStrLn $ "Is module " ++ name ++ " completed? (True/False): "
            isCompletedStr <- getLine
            case reads isCompletedStr of
                [(completed, "")] -> go (currentNum + 1) (remaining - 1) (Module name completed : acc)
                _ -> do
                    putStrLn "Invalid input. Please enter a valid completion status."
                    go currentNum remaining acc

-- Function to gather assessment details from user input
getAssessmentsFromUser :: Int -> IO [Assessment]
getAssessmentsFromUser n = go 1 n []
    where
        go _ 0 acc = return (reverse acc)
        go currentNum remaining acc = do
            putStrLn $ "Enter the name of assessment " ++ show currentNum ++ ": "
            name <- getLine
            putStrLn $ "Enter the score for assessment " ++ name ++ ": "
            scoreStr <- getLine
            case reads scoreStr of
                [(assmtScore, "")] -> go (currentNum + 1) (remaining - 1) (Assessment name assmtScore : acc)
                _ -> do
                    putStrLn "Invalid input. Please enter a valid assessment score."
                    go currentNum remaining acc

-- Function to gather achievement details from user input
getAchievementsFromUser :: Int -> IO [Achievement]
getAchievementsFromUser n = go 1 n []
    where
        go _ 0 acc = return (reverse acc)
        go currentNum remaining acc = do
            putStrLn $ "Enter the name of achievement " ++ show currentNum ++ ": "
            name <- getLine
            putStrLn $ "Enter the description for achievement " ++ name ++ ": "
            description <- getLine
            go (currentNum + 1) (remaining - 1) (Achievement name description : acc)

executeProgressTrackingContract :: IO ()
executeProgressTrackingContract = do
    putStrLn ""
    putStrLn "Progress Tracking"
    putStrLn ""
    putStrLn "Enter the user ID: "
    userIdStr <- getLine
    case reads userIdStr of
        [(uId, "")] -> do
            putStrLn "Enter the number of completed modules: "
            numModulesStr <- getLine
            case reads numModulesStr of
                [(numModules, "")] -> do
                    modules <- getModulesFromUser numModules
                    putStrLn "Enter the number of completed assessments: "
                    numAssessmentsStr <- getLine
                    case reads numAssessmentsStr of
                        [(numAssessments, "")] -> do
                            assessments <- getAssessmentsFromUser numAssessments
                            putStrLn "Enter the number of achievements: "
                            numAchievementsStr <- getLine
                            case reads numAchievementsStr of
                                [(numAchievements, "")] -> do
                                    achievements <- getAchievementsFromUser numAchievements
                                    let progress = LearningProgress uId modules assessments achievements
                                    putStrLn $ "Learning Progress:\n" ++ show progress
                                _ -> putStrLn "Invalid input for achievements count."
                        _ -> putStrLn "Invalid input for assessments count."
                _ -> putStrLn "Invalid input for modules count."
        _ -> putStrLn "Invalid input for user ID."