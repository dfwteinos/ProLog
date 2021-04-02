# Some problems, solving them by using ProLog. :camel:

# :a: Team: 

In this particular team of excercises, I've had to implement 2 introductory type of problems, and one problem of constraints.

## **First**: ***__diags.pl__***

In the first directory of this team, I've implemented a program who takes as input a matrix (which is a list of lists), and it gives us back the Upper and Lower Diagonals of this very Matrix.

*Example*:

Given a matrix L:

a | b | c | d |
--- | --- | --- | --- |                                   
e | f | g | h |
i | j | k | l |


* **DiagsDown** = [[i],[e,j],[a,f,k],[b,g,l],[c,h],[d]]
* **DiagsUp** = [[a],[b,e],[c,f,i],[d,g,j],[h,k],[l]]

### Compile & Run:

`?- diags([[a,b,c,d],[e,f,g,h],[i,j,k,l]],DiagsDown,DiagsUp).`

## **Second**: ***__hopfield.pl__***

In the second directory if this team , the task was to implement the [Hopfield's Formula](https://en.wikipedia.org/wiki/Hopfield_network).

So here, our program takes as input the training vectors, and returns back the "weight" of the network. This task was most about matrix addition, substraction, inversion and multiplication.

### Compile & Run:
  
`?- hopfield([[+1,-1,-1,+1],[-1,-1,+1,-1],[+1,+1,+1,+1]],W).`

Answer: `W = [[0,1,-1,3],[1,0,1,1],[-1,1,0,-1],[3,1,-1,0]]`.

## **Third**: ***__games.pl__***

In the third and the most demanding excercise in this team of homeworks, we had to implement a constraint problem.

Consider that in an entertainment space there are specific electronics
games, in which you can play in a given order. You start with
first, continue to the second, and so on, until the last. Every game
you can play it more than once, but all will be consecutive.

To play a game once, you have to pay a chip.
Your chips are in a box, which has a capacity of T chips, which
initially it is full. After completing all the times a game has been played,
and before you start the next, you are given a gift of K chips, or less, and
however, no more than can fit in your box. 

Every time you play game i, your pleasure is P_i. 

The pleasure of a game it can also be negative, which means that you did not like the game, or zero, which means that your game is indifferent. 

The question is, considering of T, K and P_i, how many times you have to play each game in order to finally have the maximum overall pleasure. 

You have to play every game at least once so you can find out how much you like it.

So, given {Ps, T, K}, you must find the list(s) Gs, which contains the number of times for each game (inside of Ps), and the P which is equal to the final pleasure of this combination of games.

### Compile and run:

`?- games([4,1,2,3],5,2,Gs,P)`

Answer: `Gs = [5,1,1,4] and P = 35.`

`?- games([2,-3,4,1,-2,2,1],8,3,Gs,P).`

Answer: `Gs = [5,1,8,1,1,7,3] and P = 55.`
