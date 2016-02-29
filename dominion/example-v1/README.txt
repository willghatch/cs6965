To play a game with the text-only driver, use

 racket text-driver.rkt <prog> ...

with between 2 and 4 player <prog>s.


To play a game with the GUI driver, use

 racket gui-driver.rkt <prog> ...


To use "player-v1.rkt" or "dumb-player.rkt" as a player, create an
executable via

  raco exe --l player-v1.rkt

and then use "player-v1" (Windows: "player-v1.exe") as a player.


To test the driver against a given state and potential move, pipe
inputs such as "tests.rktd" into "external-tests.rkt":

 racket external-tests.rkt < tests.rktd
