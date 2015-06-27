# Brewkit

Now you can keep your code spiffingly clean. brewkit enforces
 development guidelines to keep code and its history
easily navigable, comprehensible, and clean.

## Setup

    git clone git@github.com:iand675/brewkit.git
    cd brewkit
    cabal sandbox init
    cabal install --only-dependencies -j
    cabal build

    OR

    cabal install brewkit

Now add the current working directory's dist folder to your `$PATH` or otherwise copy brewkit into your path. For example:

    export PATH=$HOME/Code/brewkit/dist/build/brewkit:$PATH;

## Usage

Run `brewkit install` to set it up while in the root of a git repository..
Now, whenever you commit, brewkit will validate that your
commit messages are up to snuff! 

On commit, it enforces a variant [angular.js commit guidelines](https://github.com/angular/angular.js/blob/master/CONTRIBUTING.md#-git-commit-guidelines) for commit messages.

When you're ready to make a release, you can use brewkit's other commands to do so:

- `brewkit changelog` (TODO)
- `brewkit contributors` (TODO)

