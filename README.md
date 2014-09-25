# elerea-pong

The most important thing to keep in mind while watching or paying attention to
this project is simply the fact that it's not good. It's not well executed,
designed, anything. The code is disgusting, poorly organized, and generally a
very bad base to work off of. Depending on my choice of Netwire 5 vs. Elerea
(in other words the reason I'm writing this project), there will be better
worked examples of Elerea in similar contexts.

### Installation

To run the project:

```bash
>$ git clone git@github.com:crockeo/elerea-pong.git # Getting the repo
>$ cd elerea-pong/                                  # Moving into the directory
>$ cabal sandbox init                               # OPTIONAL - Makes a cabal sandbox for you - I usually do
>$ cabal install --only-dependencies                # Installing the dependencies for the project.

# Option 1 - Running the project directly:
>$ cabal run

# Option 2 - Building the project first. The executable
             for the program should be found in some
             sub-folder of 'dist/':
>$ cabal build
```
