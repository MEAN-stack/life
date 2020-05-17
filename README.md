# Life

[Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) in Haskell, using the Gloss 2D drawing package.

## Download
```
git clone https://github.com/MEAN-stack/life.git
```
## Install Dependencies
```
cd life
cabal install gloss
```

## Build
```
ghc life.hs
```
## Run
```
./life
```
## Starting configuration

The contents are centred and padded out with empty cells to an array of size 100 x 100

The given configuration is the [Gosper Glider Gun](https://www.conwaylife.com/wiki/Gosper_glider_gun)

### Changing the Starting configuration

Just edit seed.txt
