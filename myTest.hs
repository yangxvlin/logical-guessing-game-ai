module Main where

import Proj1
import Card

main :: IO()
main = do
    print((feedback [(Card Club R3), (Card Heart R4)] [(Card Heart R4), (Card Club R3)]) == (2,0,2,0,2))
    print((feedback [(Card Club R3), (Card Heart R4)] [(Card Club R3), (Card Heart R3)]) == (1,0,1,1,2))
    print((feedback (map read ["3D", "3H"]) (map read ["3S", "3C"])) == (0,0,2,0,0))
    print((feedback (map read ["3C", "4H"]) (map read ["2H", "3H"])) == (0,0,1,1,1))
    print((feedback (map read ["AC", "2C"]) (map read ["3C", "4H"])) == (0,1,0,1,1))
