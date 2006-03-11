---Interprete ---
module Main where

import Parser
import Scanner
import UU.Parsing

main = do
	putStr "\nInterprete MiniL \n$>"
	a <- getLine
	putStr "$>"
	f a
f a = if (head a) == ':' then runCommands (tail a) else (if a /= [] then (if  (last a) == ';' then g a else h a) else h a )

g a = do 
	out <- parseIO pRoot (scanner a)
	out
	main

h a = do
	b <- getLine 
	f (a++b)

runCommands a | a == "q" = leave
			  | a == "h" || a == "?" = help
			  | a == "a" = about
			  | otherwise = badCommand
		
commands = ["h","?","a","q"]
	  
leave = do 
	putStr "Leaving MiniL ... bye\n"

help = do 
	putStr "\nCommands of MiniL\n"
	putStr ":? or :h print help\n"
	putStr ":q leave the program\n"
	putStr ":a about the program an the authors\n"
	main
	
about = do
	putStr "\nAuthors of MiniL:\n"
	putStr "Benjamin Perez\nJuan Jose Olivera\n"
	main
	
badCommand = do
	putStr "Bad command try again\n"
	main
	