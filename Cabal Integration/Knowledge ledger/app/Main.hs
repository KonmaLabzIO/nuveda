import Data.List (intercalate, find)
import FirstContract (runFirstContract)
import Progress_Tracking (executeProgressTrackingContract)
import Tokenization (executeTokenizationContract)
import Certificate_Insurance (executeCertificateInsuranceContract)

main :: IO ()
main = do
    runFirstContract
    executeProgressTrackingContract
    executeTokenizationContract
    executeCertificateInsuranceContract  