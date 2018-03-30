# upstreamables

This directory, and Cabal package, is where we put our additions to Haskell
dependencies. Take for example, the Turtle package, a Haskell dependency of the
Snowdrift project. There's a lot we wish that package came with, but it doesn't.
So we extend it accordingly with the `AdditionsTo.Turtle` module.

Under the naive ideal, we wouldn't have anything in this directory: all its
contents would be upstreamed into the relevant Hackage package. In the example,
`AdditionsTo.Turtle` would be given to the Turtle project as a merge request and
then be deleted from this repository. Then, when we `import Turtle`, we will get
everything that was in `AdditionsTo.Turtle` just from `Turtle`.

However, things have to go somewhere in the meantime.

Under a more seasoned ideal, upstreaming is a regular part of development, and
we ought to have a place to contain that.

Note this package is strictly local and its contents is temporary. This is not
a package for Hackage.

## Naming
If we're pretending to add stuff to the `Foo` module, we make the
`AdditionsTo.Foo` module. If we'd like the `Foo` module to have a `Bar`
sub-module, we make the `AdditionsTo.Foo.Bar` module, because `Bar` is an
addition to `Foo`.

Note that each module must have a corresponding \*.hs file:
for each `AdditionsTo.Foo.Bar` module, we make an `AdditionsTo/Foo/Bar.hs` file.
So we had to create `AdditionsTo/Turtle.hs`, despite it only containing
sub-modules. And it has to be alongside its corresponding directory, not inside.
Otherwise, that'd be like `AdditionsTo.Turtle.Turtle`.

And strangely, if you make `AdditionsTo.Foo`, and it contains `Bar` and
`Baz` sub-modules, you must list `AdditionsTo.Foo.Bar` and `AdditionsTo.Foo.Baz`
under "other-modules" in the Cabal file, otherwise, other packages that depend
on `upstreamables` will not be able to compile ... at time-of-writing.
