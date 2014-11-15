# Brewkit

Now you can keep your code spiffingly clean. brewkit enforces
Brewtown's development guidelines to keep the code history
easily navigable, comprehensible, and clean.

## Setup

    git clone git@github.com:brewtown/brewkit.git
    cd brewkit
    cabal sandbox init
    cabal install --only-dependencies -j
    cabal build

Now add the current working directory's dist folder to your `$PATH`. For example:

    export PATH=$HOME/Code/brewkit/dist/build/brewkit:$PATH;

## Usage

In Brewtown repositories, run `brewkit install` to set it up.
Now, whenever you commit, brewkit will validate that your
commit messages are up to snuff! If you're wondering what
guidelines it enforces, check out the [Hackpad](https://brewtown.hackpad.com/Code-guidelines-1aPcXJYyTAi)


