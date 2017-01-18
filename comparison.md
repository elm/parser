## Comparison with Prior Work

I have not seen the [parser pipeline][1] or the [context stack][2] ideas in other libraries, but [delayed commits][3] relate to prior work.

[1]: README.md#parser-pipelines
[2]: README.md#tracking-context
[3]: README.md#delayed-commits

Most parser combinator libraries I have seen are based on Haskell’s Parsec library, which has primitives named `try` and `lookAhead`. I believe [`delayedCommitMap`][delayedCommitMap] is a better primitive for two reasons.

[delayedCommitMap]: http://package.elm-lang.org/packages/elm-tools/parser/latest/Parser#delayedCommitMap


### Performance and Composition

Say we want to create a precise error message for `length [1,,3]`. The naive approach with Haskell’s Parsec library produces very bad error messages:

```haskell
spaceThenArg :: Parser Expr
spaceThenArg =
  try (spaces >> term)
```

This means we get a precise error from `term`, but then throw it away and say something went wrong at the space before the `[`. Very confusing! To improve quality, we must write something like this:

```haskell
spaceThenArg :: Parser Expr
spaceThenArg =
  choice
    [ do  lookAhead (spaces >> char '[')
          spaces
          term
    , try (spaces >> term)
    ]
```

Notice that we parse `spaces` twice no matter what.

Notice that we also had to hardcode `[` in the `lookAhead`. What if we update `term` to parse records that start with `{` as well? To get good commits on records, we must remember to update `lookAhead` to look for `oneOf "[{"`. Implementation details are leaking out of `term`!

With `delayedCommit` in this Elm library, you can just say:

```elm
spaceThenArg : Parser Expr
spaceThenArg =
  delayedCommit spaces term
```

It does less work, and is more reliable as `term` evolves. I believe `delayedCommit` makes `lookAhead` pointless.


### Expressiveness

You can define `try` in terms of [`delayedCommitMap`][delayedCommitMap] like this:

```elm
try : Parser a -> Parser a
try parser =
  delayedCommitMap always parser (succeed ())
```

No expressiveness is lost!

While it is possible to define `try`, I left it out of this package. In practice, `try` often leads to “bad commits” where your parser fails in a very specific way, but you then backtrack to a less specific error message. I considered naming it `allOrNothing` to better explain how it changes commit behavior, but ultimately, I thought it was best to encourage users to express their parsers with `delayedCommit` directly.


### Summary

Compared to previous work, `delayedCommit` lets you produce precise error messages **more efficiently**. By thinking about “commit behavior” directly, you also end up with **cleaner composition** of parsers. And these benefits come **without any loss of expressiveness**.
