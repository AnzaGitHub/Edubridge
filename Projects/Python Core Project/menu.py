from madlibs import madlibs
from rock import rock
from dice import dice
from pong import pong
from hangman import hangman

while True:
    a = int(input("Select the Game you want to play: \n1. Madlibs \n2. Rock Paper Scissors \n3. Roll the Dice \n4. Pong \n5. Hangman >>"))
    if a == 1:
        madlibs()
    elif a == 2:
        rock()
    elif a == 3:
        dice()
    elif a == 4:
        pong()
    elif a == 5:
        hangman()
    else :
        print("Wrong option selected. Try again!")
