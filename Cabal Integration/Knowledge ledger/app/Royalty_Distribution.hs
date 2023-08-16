-- Royalty_Distribution.hs

module Royalty_Distribution
  ( ContentCreator(..),
    distributeRoyalties,
    getRevenueFromUser,
    getContentCreatorsFromUser
  ) where

data ContentCreator = ContentCreator
    { ccName :: String
    , ccRoyalty :: Double
    } deriving (Show)

distributeRoyalties :: Double -> [ContentCreator] -> [(String, Double)]
distributeRoyalties revenue creators = do
    let totalRoyalties = sum $ map ccRoyalty creators
    let royaltyAmount creator = (ccName creator, revenue * ccRoyalty creator / totalRoyalties)
    map royaltyAmount creators

getRevenueFromUser :: IO Double
getRevenueFromUser = do
    putStrLn "Enter the revenue generated: "
    revenueStr <- getLine
    case reads revenueStr of
        [(revenue, "")] -> return revenue
        _ -> do
            putStrLn "Invalid input. Please enter a valid revenue."
            getRevenueFromUser

getContentCreatorsFromUser :: IO [ContentCreator]
getContentCreatorsFromUser = do
    putStrLn "Enter the number of content creators: "
    numCreatorsStr <- getLine
    case reads numCreatorsStr of
        [(numCreators, "")] -> do
            creators <- getContentCreators [] numCreators
            return creators
        _ -> do
            putStrLn "Invalid input. Please enter a valid number of content creators."
            getContentCreatorsFromUser

getContentCreators :: [ContentCreator] -> Int -> IO [ContentCreator]
getContentCreators acc 0 = return $ reverse acc
getContentCreators acc n = do
    putStrLn $ "Enter the name of content creator " ++ show (length acc + 1) ++ ": "
    name <- getLine
    putStrLn $ "Enter the royalty percentage for " ++ name ++ ": "
    royaltyStr <- getLine
    case reads royaltyStr of
        [(royalty, "")] -> getContentCreators (ContentCreator name royalty : acc) (n - 1)
        _ -> do
            putStrLn "Invalid input. Please enter a valid royalty percentage."
            getContentCreators acc n
