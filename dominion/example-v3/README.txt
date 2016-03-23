To play a game with the text-only driver, use

 racket text-driver.rkt <prog> ...

with between 2 and 4 player <prog>s.


To play a game with the GUI driver, use

 racket gui-driver.rkt <prog> ...


To use "player-v1.rkt" as a player, create an executable via

  raco exe -l player-v1.rkt

and then use "player-v1" (Windows: "player-v1.exe") as a player.
Create an executable player from "player-v3-solider.rkt",
"player-v2-random.rkt", or "dummy-player.rkt" similarly.


To test the driver against a given state and potential move, pipe
inputs such as "tests.rktd" into "external-test.rkt":

 racket external-test.rkt < tests.rktd
