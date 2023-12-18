# game-solver-template

# Project Grade:         82/100
## Functionality               60/73
* Game mechanics:              20
* Exact game solver:           15
  * Looks good, you could have used lookup instead of find.
* Cut-off depth solver:        13
  * Seems to work, even to very high depths. Low branching factor, maybe? Games also very near
    end.
* Evaluation function:         1/2
  * You only take the highest of any row, rather than counting the number of outs.
* Avoiding unnecessary work:   0/3
* Command-line interface:      7/10
  * Prints some extra information (probably getOpt's results)
  * depth flag doesnt' seem to work. 
    * No, it works, it's just very quick when the game is near the end.
  * your move format requires quotes on the command line. But you would just accept 4 indices next
    to eachother, which doesn't need quotes.
* Move and verbose flags:      3/5
  * You always print the resulting game, just sometimes in the internal haskell format when verbose
    isn't passed.
  * Move seems to work, but again prints in the wrong format.
* Error-handling:              1/5
  * Crashes when run with no parameters
  * crashes every time because getMove is broken
  * loadgame/readgame are not safe.
  * updateGameState has some safe calls, but there are still fromJusts floating around. 
    and changeRow crashes on invalid boards


## Design                      22/27
* Well-designed data types:    8
* Well-decomposed functions:   10
  * Definitely some unnecessary functions. toInt is only slightly shorter than inlining it, and is
    called once.
  * changing the game is quite clunky: passing around that many parameters is a good sign that your
    decomposition isn't clean. I think some maybe `do` blocks would have helped.
  * putBestMove is far more complex than needed, and involves multiple calls to your exponential
    functions.
* Good module decomposition:   2
  * Boy is that a giant module.
* Good variable names:         2
  * smth, etc.
* Efficient/idiomatic code:    5
