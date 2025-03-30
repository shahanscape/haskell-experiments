--
-- MATHFUN
--

--
-- Imports
--
import Data.Char
import Data.Foldable (minimumBy)
import Data.List
import Data.Ord (comparing)
import Text.Printf (printf)
import Control.Concurrent 

--
-- Types (define your Station type here)
--
-- data Station = Station String Float Float [Float]
--                deriving (Show, Read, Eq, Ord)
data Station = Station {stationName :: String, north :: Float, east :: Float, temperatures :: [Float]}
  deriving (Show, Read, Eq, Ord)

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Show, Read, Eq, Ord, Enum)

testData :: [Station]
testData =
  [ Station "Mumbles Head" 51.565 (-3.981) [8.26, 8.33, 9.84, 12.36, 15.24, 17.83, 19.55, 19.67, 17.97, 14.70, 11.49, 9.09],
    Station "Greenwich Park" 51.477 0.004 [8.47, 9.21, 12.07, 15.35, 18.59, 21.37, 23.75, 23.31, 20.29, 15.83, 11.55, 8.85],
    Station "Solent" 50.807 (-1.208) [8.56, 8.74, 11.01, 13.94, 17.07, 19.59, 21.62, 21.61, 19.38, 15.73, 11.88, 9.17],
    Station "Ronaldsway" 54.085 (-4.632) [8.47, 8.35, 9.44, 11.48, 14.33, 16.52, 18.19, 18.15, 16.56, 13.83, 11.10, 9.17],
    Station "Baltasound" 60.749 (-0.85) [6.55, 6.32, 7.35, 9.16, 11.20, 13.25, 15.08, 15.39, 13.62, 10.88, 8.47, 7.00],
    Station "St Austell" 50.337 (-4.787) [9.46, 9.65, 11.33, 13.30, 16.18, 18.10, 20.60, 20.36, 18.54, 14.99, 12.30, 10.18],
    Station "Heathrow" 51.479 (-0.449) [8.42, 8.98, 11.73, 15.00, 18.37, 21.57, 23.89, 23.40, 20.22, 15.81, 11.47, 8.79],
    Station "Hunstanton" 52.939 0.493 [7.05, 7.45, 9.77, 12.65, 15.96, 18.84, 21.34, 21.28, 18.32, 14.46, 10.29, 7.56],
    Station "Durham" 54.767 (-1.583) [6.86, 7.75, 9.87, 12.49, 15.42, 17.96, 20.24, 19.87, 17.36, 13.51, 9.65, 7.07],
    Station "Monks Wood" 52.400 (-0.233) [7.58, 8.36, 11.05, 14.14, 17.19, 20.01, 22.63, 22.49, 19.50, 15.18, 10.68, 7.85]
  ]

--
--  functional code
--
listStations :: [Station] -> String
listStations = unlines . fmap stationName

addStation :: [Station] -> String -> Float -> Float -> [Float] -> [Station]
addStation [] stationName north east temperature = [Station stationName north east temperature]
addStation xs stationName north east temperature = xs ++ [Station stationName north east temperature]

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * 9 / 5) + 32

dataToFahrenheit :: [Station] -> [Station]
dataToFahrenheit [] = []
dataToFahrenheit stationData = [Station stationName north east (map celsiusToFahrenheit xs) | (Station stationName north east xs) <- stationData]

stationsWithHighTemp :: [Station] -> Month -> Float -> String
stationsWithHighTemp station month celsius = unlines $ [name | (Station name _ _ xs) <- station, xs !! fromEnum month > celsius]

dataToString :: Station -> String
dataToString (Station name north east xs) = printf "%s %.1f %.1f " name north east ++ unwords (printf "%.1f" <$> xs)

stationsToString :: [Station] -> String
stationsToString xs = unlines (map dataToString xs)

replaceValue :: Station -> String -> Month -> Float -> Station
replaceValue station inputName month newTemp
  | inputName == stationName station = station {temperatures = newTemperatures}
  | otherwise = station
  where
    newTemperatures =
      let (x, _ : xs) = splitAt (fromEnum month) (temperatures station)
       in x ++ newTemp : xs

replaceTemperatures :: [Station] -> String -> Month -> Float -> [Station]
replaceTemperatures stations stationName month newTemp = fmap modifyStation stations
  where
    modifyStation :: Station -> Station
    modifyStation station = replaceValue station stationName month newTemp

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

distanceTo :: Float -> Float -> Station -> Float
distanceTo degN degE station = distance degN degE (north station) (east station)

distancesTo :: Float -> Float -> [Station] -> [(Station, Float)]
distancesTo degN degE = map (\station -> (station, distanceTo degN degE station))

filterStation :: Month -> Float -> Station -> Bool
filterStation month temp station = temperatures station !! fromEnum month > temp

filterAllStations :: Month -> Float -> [Station] -> [Station]
filterAllStations month temp = filter (filterStation month temp)

closestTo :: Float -> Float -> Month -> Float -> [Station] -> Station
closestTo degN degE month temp stations = fst $ minimumBy (comparing snd) (distancesTo degN degE (filterAllStations month temp stations))

--
--  Demo
--

demo :: Int -> IO ()
demo 1 = putStrLn $ listStations testData
demo 2 = putStrLn $ stationsToString $ addStation testData "Valley" 53.252 (-4.537) [8.37, 8.44, 9.84, 12.09, 15.01, 17.24, 18.76, 17.26, 14.31, 11.26, 9.09]
demo 3 = putStrLn $ stationsToString $ dataToFahrenheit testData
demo 4 = putStrLn $ stationsWithHighTemp testData August 20
demo 5 = putStrLn $ stationsToString testData
demo 6 = putStrLn $ stationsToString $ replaceTemperatures testData "Heathrow" July 25
demo 7 = putStrLn $ dataToString $ closestTo (50.2) (-0.4) March 10 testData
demo 8 = putStrLn $ makeBarChart testData

--
-- Screen Utilities (use these to do the bar chart)
--

type ScreenPosition = (Int, Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
  goTo position
  putStr text

--
-- bar chart
--

hashString :: [Int] -> String
hashString [] = []
hashString (x : xs) = replicate x '#' ++ "\n" ++ hashString xs

makeChart :: Station -> String
makeChart (Station name _ _ temps) =
  name
    ++ "\n"
    ++ hashString (map round temps)

makeBarChart :: [Station] -> String
makeBarChart station = unlines (map makeChart station)

--
-- interface (and loading/saving)
--

main :: IO ()
main = do
  putStr "Enter a file name: "
  fileName <- getLine
  if fileName == "stations.txt"
    then return ()
    else main
  contents <- readFile fileName
  let stations = (read contents :: [Station])
  putStrLn $ stationsToString stations
  options

options :: IO ()
options = do
  putStrLn "1. List Stations\n2. Add Stations\n3. Convert Temperatures To Fahrenheit\n4. Display Stations With Higher Temperature Given Temp Value"
  putStrLn "5. Display Station Data Neatly As Single String\n6. Replace Temperature Value"
  putStrLn "7. Return Closest Station With Higher Temperature For Given Month\n8. Animated Bar Chart\n9. Exit The Program"
  putStr "Enter an input: "
  inputNumber <- getLine
  if inputNumber `elem` ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    then case inputNumber of
      "1" -> putStrLn $ listStations testData
      "2" -> putStrLn $ stationsToString $ addStation testData "Valley" 53.252 (-4.537) [8.37, 8.44, 9.84, 12.09]
      "3" -> putStrLn $ stationsToString $ dataToFahrenheit testData
      "4" -> putStrLn $ stationsWithHighTemp testData August 20
      "5" -> putStrLn $ stationsToString testData
      "6" -> putStrLn $ stationsToString $ replaceTemperatures testData "Heathrow" July 25
      "7" -> putStrLn $ dataToString $ closestTo (50.2) (-0.4) March 10 testData
      "8" -> putStrLn $ makeBarChart testData
      "9" -> writeFile "stations.txt" (show $ addStation testData "Valley" 53.252 (-4.537) [8.37, 8.44, 9.84, 12.09, 15.01, 17.24, 18.76, 17.26, 14.31, 11.26, 9.09])
    else options
  if inputNumber == "9"
    then return ()
    else options