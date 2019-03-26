# Tarefa #1 - HASKELL GAME

This game was made in haskell language. Started in course project and your progression, I migrate to personal config.

Asteroids is a space-themed multidirectional shooter arcade game designed by Lyle Rains, Ed Logg, and Dominic Walsh and released in November 1979 by Atari, Inc.[1] The player controls a single spaceship in an asteroid field which is periodically traversed by flying saucers. The object of the game is to shoot and destroy the asteroids and saucers, while not colliding with either, or being hit by the saucers' counter-fire. The game becomes harder as the number of asteroids increases. [More about Asteroids you can found here](https://en.wikipedia.org/wiki/Asteroids_(video_game))

# Development

After install a Haskell dependencies, you will have access to Cabal tool. With this you can give more power to your project, using, for example, others libs to upgrade your work.

You can folow this [article](http://andrew.gibiansky.com/blog/haskell/haskell-gloss/) to init cabal in the project or follow the base local configurations.

Run this to make a local environment of the project
`
cabal sandbox init
`
Run this to initialize the base project
`
cabal init 
`
Run this to install Gloss in project
`
cabal install gloss==1.12.*
`

Run this to generate and run the executable project
`
cabal run Main.h
`

That will create a .exe file to play the game.

## Install - Gloss

[Gloss lib](http://hackage.haskell.org/package/gloss)

# Game

You can control the ship using "arrows" and shoot with "Ctrl" button.
# References

[First Game reference](http://andrew.gibiansky.com/blog/haskell/haskell-gloss/)
[Future Game reference](http://jxv.io/blog/2018-02-28-A-Game-in-Haskell.html)

