-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module ReversiAI(State,author,nickname,initial,think) where

import Reversi

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

-- Remember to provide a brief (about 100-500 words) description of
-- your implementation.

-- the internal state of your AI
type State = ()  -- modify as needed

author :: String
author = undefined  -- replace "undefined" with your first and last name

nickname :: String
nickname = undefined  -- replace "undefined" with a nickname for your AI

-- remember to provide a function specification
initial :: Reversi.Player -> State
initial = undefined  -- remove "undefined" and write your function here

-- remember to provide a function specification
think :: State -> Reversi.Move -> Double -> (Reversi.Move,State)
think = undefined  -- remove "undefined" and write your function here
