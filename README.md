# final_project
Haskell Chess Game Project

# Reference Links
https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
https://www.buffalolib.org/sites/default/files/gaming-unplugged/inst/1%20Basic%20Chess%20Instructions.pdf
https://www.haskell.org/tutorial/io.html

# TODO
* isValidMove is currently failing for Pawn trying to take on the diagonal
* isValidMove is currently failing for Queen trying to move to open diagonal square

# THOUGHTS
* Queen, Bishop, Rook need to deal with valid moves recursively since they can move an unspecified number of tiles
* Other pieces have a certain rangs