module Parser.Advanced exposing
  ( Parser, run, inContext
  , int, float, number, symbol, keyword, variable, end
  , succeed, (|=), (|.), lazy, andThen, problem
  , oneOf, map, backtrackable, commit, token, Token(..)
  , sequence, Trailing(..), loop, Step(..)
  , spaces, lineComment, multiComment, Nestable(..)
  , getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString
  , Problem
  , withIndent, getIndent
  , getPosition, getRow, getCol, getOffset, getSource
  )


{-|

# Parsers
@docs Parser, run, inContext

# Building Blocks
@docs int, float, number, symbol, keyword, variable, end

# Pipelines
@docs succeed, (|=), (|.), lazy, andThen, problem

# Branches
@docs oneOf, map, backtrackable, commit, token, Token

# Loops
@docs sequence, Trailing, loop, Step

# Whitespace
@docs spaces, lineComment, multiComment, Nestable

# Chompers
@docs getChompedString, chompIf, chompWhile, chompUntil, chompUntilEndOr, mapChompedString

# Indentation
@docs withIndent, getIndent

# Positions
@docs getPosition, getRow, getCol, getOffset, getSource
-}


import Char
import Elm.Kernel.Parser
import Set



-- INFIX OPERATORS


infix left 5 (|=) = keeper
infix left 6 (|.) = ignorer


{- NOTE: the (|.) oporator binds tighter to slightly reduce the amount
of recursion in pipelines. For example:

    func
      |. a
      |. b
      |= c
      |. d
      |. e

With the same precedence:

    (ignorer (ignorer (keeper (ignorer (ignorer func a) b) c) d) e)

With higher precedence:

    keeper (ignorer (ignorer func a) b) (ignorer (ignorer c d) e)

So the maximum call depth goes from 5 to 3.
-}



-- PARSERS


type Parser context problem value =
  Parser (State context -> PStep context problem value)


type PStep context problem value
  = Good Bool value (State context)
  | Bad Bool (Bag context problem)


type alias State context =
  { src : String
  , offset : Int
  , indent : Int
  , context : List (Located context)
  , row : Int
  , col : Int
  }


type alias Located context =
  { row : Int
  , col : Int
  , context : context
  }



-- RUN


run : Parser c x a -> String -> Result (Problems c x) a
run (Parser parse) src =
  case parse { src = src, offset = 0, indent = 1, context = [], row = 1, col = 1} of
    Good _ value _ ->
      Ok value

    Bad _ bag ->
      Err (bagToList bag [])



-- PROBLEMS


type alias Problems c x =
  List (Problem c x)


type alias Problem c x =
  { row : Int
  , col : Int
  , problem : x
  , contextStack : List (Located c)
  }


type Bag c x
  = Empty
  | AddRight (Bag c x) (Problem c x)
  | Append (Bag c x) (Bag c x)


fromState : State c -> x -> Bag c x
fromState s x =
  AddRight Empty (Problem s.row s.col x s.context)


fromInfo : Int -> Int -> x -> List (Located c) -> Bag c x
fromInfo row col x context =
  AddRight Empty (Problem row col x context)


bagToList : Bag c x -> List (Problem c x) -> List (Problem c x)
bagToList bag list =
  case bag of
    Empty ->
      list

    AddRight bag1 x ->
      bagToList bag1 (x :: list)

    Append bag1 bag2 ->
      bagToList bag1 (bagToList bag2 list)



-- PRIMITIVES


succeed : a -> Parser c x a
succeed a =
  Parser <| \s ->
    Good False a s


problem : x -> Parser c x a
problem x =
  Parser <| \s ->
    Bad False (fromState s x)



-- MAPPING


map : (a -> b) -> Parser c x a -> Parser c x b
map func (Parser parse) =
  Parser <| \s0 ->
    case parse s0 of
      Good p a s1 ->
        Good p (func a) s1

      Bad p x ->
        Bad p x


map2 : (a -> b -> value) -> Parser c x a -> Parser c x b -> Parser c x value
map2 func (Parser parseA) (Parser parseB) =
  Parser <| \s0 ->
    case parseA s0 of
      Bad p x ->
        Bad p x

      Good p1 a s1 ->
        case parseB s1 of
          Bad p2 x ->
            Bad (p1 || p2) x

          Good p2 b s2 ->
            Good (p1 || p2) (func a b) s2


keeper : Parser c x (a -> b) -> Parser c x a -> Parser c x b
keeper parseFunc parseArg =
  map2 (<|) parseFunc parseArg


ignorer : Parser c x keep -> Parser c x ignore -> Parser c x keep
ignorer keepParser ignoreParser =
  map2 always keepParser ignoreParser



-- AND THEN


andThen : (a -> Parser c x b) -> Parser c x a -> Parser c x b
andThen callback (Parser parseA) =
  Parser <| \s0 ->
    case parseA s0 of
      Bad p x ->
        Bad p x

      Good p1 a s1 ->
        let
          (Parser parseB) =
            callback a
        in
        case parseB s1 of
          Bad p2 x ->
            Bad (p1 || p2) x

          Good p2 b s2 ->
            Good (p1 || p2) b s2



-- LAZY


lazy : (() -> Parser c x a) -> Parser c x a
lazy thunk =
  Parser <| \s ->
    let
      (Parser parse) =
        thunk ()
    in
    parse s



-- ONE OF


oneOf : List (Parser c x a) -> Parser c x a
oneOf parsers =
  Parser <| \s -> oneOfHelp s Empty parsers


oneOfHelp : State c -> Bag c x -> List (Parser c x a) -> PStep c x a
oneOfHelp s0 bag parsers =
  case parsers of
    [] ->
      Bad False bag

    Parser parse :: remainingParsers ->
      case parse s0 of
        Good _ _ _ as step ->
          step

        Bad p x as step ->
          if p then
            step
          else
            oneOfHelp s0 (Append bag x) remainingParsers



-- ZERO OR MORE


zeroOrMore : Parser c x a -> x -> Parser c x (List a)
zeroOrMore (Parser parse) stuck =
  Parser <| \s ->
    zeroOrMoreHelp False parse [] s stuck


zeroOrMoreHelp : Bool -> (State c -> PStep c x a) -> List a -> State c -> x -> PStep c x (List a)
zeroOrMoreHelp p parse revList s0 stuck =
  case parse s0 of
    Good p1 a s1 ->
      if s0.offset == s1.offset then
        Bad (p || p1) stuck
      else
        zeroOrMoreHelp (p || p1) parse (a :: revList) s1 stuck

    Bad p1 x ->
      Good (p || p1) (List.reverse revList) s0


type Step state a
  = Loop state
  | Done a


loop : state -> (state -> Parser c x (Step state a)) -> Parser c x a
loop state callback =
  Parser <| \s ->
    loopHelp False state callback s


loopHelp : Bool -> state -> (state -> Parser c x (Step state a)) -> State c -> PStep c x a
loopHelp p state callback s0 =
  let
    (Parser parse) =
      callback state
  in
  case parse s0 of
    Good p1 step s1 ->
      case step of
        Loop newState ->
          loopHelp (p || p1) newState callback s1

        Done result ->
          Good (p || p1) result s1

    Bad p1 x ->
      Bad (p || p1) x



-- BACKTRACKABLE


backtrackable : Parser c x a -> Parser c x a
backtrackable (Parser parse) =
  Parser <| \s0 ->
    case parse s0 of
      Bad _ x ->
        Bad False x

      Good _ a s1 ->
        Good False a s1


commit : a -> Parser c x a
commit a =
  Parser <| \s -> Good True a s



-- SYMBOL


symbol : Token x -> Parser c x ()
symbol =
  token



-- KEYWORD


keyword : Token x -> Parser c x ()
keyword (Token kwd expecting) =
  let
    progress =
      not (String.isEmpty kwd)
  in
  Parser <| \s ->
    let
      (newOffset, newRow, newCol) =
        isSubString kwd s.offset s.row s.col s.src
    in
    if newOffset == -1 || 0 <= isSubChar (\c -> Char.isAlphaNum c || c == '_') newOffset s.src then
      Bad False (fromState s expecting)
    else
      Good progress ()
        { src = s.src
        , offset = newOffset
        , indent = s.indent
        , context = s.context
        , row = newRow
        , col = newCol
        }



-- TOKEN


type Token x = Token String x


token : Token x -> Parser c x ()
token (Token str expecting) =
  let
    progress =
      not (String.isEmpty str)
  in
  Parser <| \s ->
    let
      (newOffset, newRow, newCol) =
        isSubString str s.offset s.row s.col s.src
    in
    if newOffset == -1 then
      Bad False (fromState s expecting)
    else
      Good progress ()
        { src = s.src
        , offset = newOffset
        , indent = s.indent
        , context = s.context
        , row = newRow
        , col = newCol
        }



-- INT


int : x -> x -> Parser c x Int
int expecting invalid =
  number
    { int = Ok identity
    , hex = Err invalid
    , octal = Err invalid
    , binary = Err invalid
    , float = Err invalid
    , invalid = invalid
    , expecting = expecting
    }



-- FLOAT


float : x -> x -> Parser c x Float
float expecting invalid =
  number
    { int = Ok toFloat
    , hex = Err invalid
    , octal = Err invalid
    , binary = Err invalid
    , float = Ok identity
    , invalid = invalid
    , expecting = expecting
    }



-- NUMBER


number
  : { int : Result x (Int -> a)
    , hex : Result x (Int -> a)
    , octal : Result x (Int -> a)
    , binary : Result x (Int -> a)
    , float : Result x (Float -> a)
    , invalid : x
    , expecting : x
    }
  -> Parser c x a
number c =
  Parser <| \s ->
    if isAsciiChar '0' s.offset s.src then
      let
        zeroOffset = s.offset + 1
      in
      if isAsciiChar 'x' zeroOffset s.src then
        finalizeNumber c.invalid c.hex String.toInt (consume isHex (zeroOffset + 1) s.src) s
      else if isAsciiChar 'o' zeroOffset s.src then
        finalizeNumber c.invalid c.octal toOctal (consume isOctal (zeroOffset + 1) s.src) s
      else if isAsciiChar 'b' zeroOffset s.src then
        finalizeNumber c.invalid c.binary toBinary (consume isBinary (zeroOffset + 1) s.src) s
      else
        finalizeFloat c.invalid c.expecting c.int c.float zeroOffset s

    else
      finalizeFloat c.invalid c.expecting c.int c.float (consume Char.isDigit s.offset s.src) s


--
-- INVARIANTS:
--   endOffset > s.offset
--
finalizeNumber : x -> Result x (t -> a) -> (String -> Maybe t) -> Int -> State c -> PStep c x a
finalizeNumber invalid handler toNumber endOffset s =
  case handler of
    Err x ->
      Bad True (fromState s x)

    Ok toValue ->
      case toNumber (String.slice s.offset endOffset s.src) of
        Nothing -> Bad True (fromState s invalid)
        Just n -> Good True (toValue n) (bumpOffset endOffset s)


bumpOffset : Int -> State c -> State c
bumpOffset newOffset s =
  { src = s.src
  , offset = newOffset
  , indent = s.indent
  , context = s.context
  , row = s.row
  , col = s.col + (newOffset - s.offset)
  }


finalizeFloat : x -> x -> Result x (Int -> a) -> Result x (Float -> a) -> Int -> State c -> PStep c x a
finalizeFloat invalid expecting intSettings floatSettings intOffset s =
  let
    floatOffset = consumeDotAndExp intOffset s.src
  in
  if floatOffset < 0 then
    Bad True (fromInfo s.row (s.col - (floatOffset + s.offset)) invalid s.context)

  else if s.offset == floatOffset then
    Bad False (fromState s expecting)

  else if intOffset == floatOffset then
    finalizeNumber invalid intSettings String.toInt intOffset s

  else
    finalizeNumber invalid floatSettings String.toFloat floatOffset s


consume : (Char -> Bool) -> Int -> String -> Int
consume isGood offset src =
  let
    newOffset = isSubChar isGood offset src
  in
  if newOffset < 0 then
    offset
  else
    consume isGood newOffset src


--
-- On a failure, returns negative index of problem.
--
consumeDotAndExp : Int -> String -> Int
consumeDotAndExp offset src =
  if isAsciiChar '.' offset src then
    consumeExp (consume Char.isDigit (offset + 1) src) src
  else
    consumeExp offset src


--
-- On a failure, returns negative index of problem.
--
consumeExp : Int -> String -> Int
consumeExp offset src =
  if isAsciiChar 'e' offset src || isAsciiChar 'E' offset src then
    let
      eOffset = offset + 1

      expOffset =
        if isAsciiChar '+' eOffset src || isAsciiChar '-' eOffset src then
          eOffset + 1
        else
          eOffset

      newOffset = consume Char.isDigit expOffset src
    in
    if expOffset == newOffset then
      -newOffset
    else
      newOffset

  else
    offset



-- HEX, OCTAL, AND BINARY


isHex : Char -> Bool
isHex char =
  let
    code = Char.toCode char
  in
  0x30 <= code && (code <= 0x39 || 0x41 <= code && code <= 0x46 || 0x61 <= code && code <= 0x66)


isOctal : Char -> Bool
isOctal char =
  let
    code = Char.toCode char
  in
  0x30 <= code && code <= 0x37


isBinary : Char -> Bool
isBinary char =
  char == '0' || char == '1'


toOctal : String -> Maybe Int
toOctal str =
  Just <| String.foldl addOctal 0 (String.dropLeft 2 str)


addOctal : Char -> Int -> Int
addOctal char total =
  8 * total + (Char.toCode char - 48)


toBinary : String -> Maybe Int
toBinary str =
  Just <| String.foldl addBinary 0 (String.dropLeft 2 str)


addBinary : Char -> Int -> Int
addBinary char total =
  if char == '0' then
    2 * total
  else
    2 * total + 1



-- END


end : x -> Parser c x ()
end x =
  Parser <| \s ->
    if String.length s.src == s.offset then
      Good False () s
    else
      Bad False (fromState s x)



-- CHOMPED STRINGS


{-|
    succeed (\start end -> String.slice start end )
      |= getOffset
      |. parser
      |= getOffset
-}
getChompedString : Parser c x a -> Parser c x String
getChompedString parser =
  mapChompedString always parser


mapChompedString : (String -> a -> b) -> Parser c x a -> Parser c x b
mapChompedString func (Parser parse) =
  Parser <| \s0 ->
    case parse s0 of
      Bad p x ->
        Bad p x

      Good p a s1 ->
        Good p (func (String.slice s0.offset s1.offset s0.src) a) s1



-- CHOMP IF


chompIf : (Char -> Bool) -> x -> Parser c x ()
chompIf isGood expecting =
  Parser <| \s ->
    let
      newOffset = isSubChar isGood s.offset s.src
    in
    -- not found
    if newOffset == -1 then
      Bad False (fromState s expecting)

    -- newline
    else if newOffset == -2 then
      Good True ()
        { src = s.src
        , offset = s.offset + 1
        , indent = s.indent
        , context = s.context
        , row = s.row + 1
        , col = 1
        }

    -- found
    else
      Good True ()
        { src = s.src
        , offset = newOffset
        , indent = s.indent
        , context = s.context
        , row = s.row
        , col = s.col + 1
        }



-- CHOMP WHILE


chompWhile : (Char -> Bool) -> Parser c x ()
chompWhile isGood =
  Parser <| \s ->
    chompWhileHelp isGood s.offset s.row s.col s


chompWhileHelp : (Char -> Bool) -> Int -> Int -> Int -> State c -> PStep c x ()
chompWhileHelp isGood offset row col s0 =
  let
    newOffset = isSubChar isGood offset s0.src
  in
  -- no match
  if newOffset == -1 then
    Good (s0.offset < offset) ()
      { src = s0.src
      , offset = offset
      , indent = s0.indent
      , context = s0.context
      , row = row
      , col = col
      }

  -- matched a newline
  else if newOffset == -2 then
    chompWhileHelp isGood (offset + 1) (row + 1) 1 s0

  -- normal match
  else
    chompWhileHelp isGood newOffset row (col + 1) s0



-- CHOMP UNTIL


chompUntil : Token x -> Parser c x ()
chompUntil (Token str expecting) =
  Parser <| \s ->
    let
      (newOffset, newRow, newCol) =
        findSubString str s.offset s.row s.col s.src
    in
    if newOffset == -1 then
      Bad False (fromInfo newRow newCol expecting s.context)

    else
      Good (s.offset < newOffset) ()
        { src = s.src
        , offset = newOffset
        , indent = s.indent
        , context = s.context
        , row = newRow
        , col = newCol
        }


chompUntilEndOr : String -> Parser c x ()
chompUntilEndOr str =
  Parser <| \s ->
    let
      (newOffset, newRow, newCol) =
        Elm.Kernel.Parser.findSubString str s.offset s.row s.col s.src

      adjustedOffset =
        if newOffset < 0 then String.length s.src else newOffset
    in
    Good (s.offset < adjustedOffset) ()
      { src = s.src
      , offset = adjustedOffset
      , indent = s.indent
      , context = s.context
      , row = newRow
      , col = newCol
      }



-- CONTEXT


{-| This is one of the big reasons to upgrade to `Parser.Advanced`! I use this
in the Elm compiler to mark what I _think_ the parser is doing. For example:

    type Context
      = Definition String
      | List

    type Expr
      = ListLiteral (List Expr)
      | Let String Expr Expr

    list : Parser Context Problem Expr
    list =
      TODO
-}
inContext : context -> Parser context x a -> Parser context x a
inContext context (Parser parse) =
  Parser <| \s0 ->
    case parse (changeContext (Located s0.row s0.col context :: s0.context) s0) of
      Good p a s1 ->
        Good p a (changeContext s0.context s1)

      Bad _ _ as step ->
        step


changeContext : List (Located c) -> State c -> State c
changeContext newContext s =
  { src = s.src
  , offset = s.offset
  , indent = s.indent
  , context = newContext
  , row = s.row
  , col = s.col
  }



-- INDENTATION


getIndent : Parser c x Int
getIndent =
  Parser <| \s -> Good False s.indent s


withIndent : Int -> Parser c x a -> Parser c x a
withIndent newIndent (Parser parse) =
  Parser <| \s0 ->
    case parse (changeIndent newIndent s0) of
      Good p a s1 ->
        Good p a (changeIndent s0.indent s1)

      Bad p x ->
        Bad p x


changeIndent : Int -> State c -> State c
changeIndent newIndent s =
  { src = s.src
  , offset = s.offset
  , indent = newIndent
  , context = s.context
  , row = s.row
  , col = s.col
  }



-- POSITION


getPosition : Parser c x (Int, Int)
getPosition =
  Parser <| \s -> Good False (s.row, s.col) s


getRow : Parser c x Int
getRow =
  Parser <| \s -> Good False s.row s


getCol : Parser c x Int
getCol =
  Parser <| \s -> Good False s.col s


getOffset : Parser c x Int
getOffset =
  Parser <| \s -> Good False s.offset s


getSource : Parser c x String
getSource =
  Parser <| \s -> Good False s.src s



-- LOW-LEVEL HELPERS


{-| When making a fast parser, you want to avoid allocation as much as
possible. That means you never want to mess with the source string, only
keep track of an offset into that string.

You use `isSubString` like this:

    isSubString "let" offset row col "let x = 4 in x"
        --==> ( newOffset, newRow, newCol )

You are looking for `"let"` at a given `offset`. On failure, the
`newOffset` is `-1`. On success, the `newOffset` is the new offset. With
our `"let"` example, it would be `offset + 3`.

You also provide the current `row` and `col` which do not align with
`offset` in a clean way. For example, when you see a `\n` you are at
`row = row + 1` and `col = 1`. Furthermore, some UTF16 characters are
two words wide, so even if there are no newlines, `offset` and `col`
may not be equal.
-}
isSubString : String -> Int -> Int -> Int -> String -> (Int, Int, Int)
isSubString =
  Elm.Kernel.Parser.isSubString


{-| Again, when parsing, you want to allocate as little as possible.
So this function lets you say:

    isSubChar isSpace offset "this is the source string"
        --==> newOffset

The `(Char -> Bool)` argument is called a predicate.
The `newOffset` value can be a few different things:

  - `-1` means that the predicate failed
  - `-2` means the predicate succeeded with a `\n`
  - otherwise you will get `offset + 1` or `offset + 2`
    depending on whether the UTF16 character is one or two
    words wide.
-}
isSubChar : (Char -> Bool) -> Int -> String -> Int
isSubChar =
  Elm.Kernel.Parser.isSubChar


{-| Check an offset in the string. Is it equal to the given Char? Are they
both ASCII characters?
-}
isAsciiChar : Char -> Int -> String -> Bool
isAsciiChar =
  Elm.Kernel.Parser.isAsciiChar


{-| Find a substring after a given offset.

    findSubString "42" offset row col "Is 42 the answer?"
        --==> (newOffset, newRow, newCol)

If `offset = 0` we would get `(3, 1, 4)`
If `offset = 7` we would get `(-1, 1, 18)`
-}
findSubString : String -> Int -> Int -> Int -> String -> (Int, Int, Int)
findSubString =
  Elm.Kernel.Parser.findSubString



-- VARIABLES


variable :
  { start : Char -> Bool
  , inner : Char -> Bool
  , reserved : Set.Set String
  , expecting : x
  }
  -> Parser c x String
variable i =
  Parser <| \s ->
    let
      firstOffset =
        isSubChar i.start s.offset s.src
    in
    if firstOffset == -1 then
      Bad False (fromState s i.expecting)
    else
      let
        s1 =
          if firstOffset == -2 then
            varHelp i.inner (s.offset + 1) (s.row + 1) 1 s.src s.indent s.context
          else
            varHelp i.inner firstOffset s.row (s.col + 1) s.src s.indent s.context

        name =
          String.slice s.offset s1.offset s.src
      in
      if Set.member name i.reserved then
        Bad False (fromState s i.expecting)
      else
        Good True name s1


varHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> List (Located c) -> State c
varHelp isGood offset row col src indent context =
  let
    newOffset = isSubChar isGood offset src
  in
  if newOffset == -1 then
    { src = src
    , offset = offset
    , indent = indent
    , context = context
    , row = row
    , col = col
    }

  else if newOffset == -2 then
    varHelp isGood (offset + 1) (row + 1) 1 src indent context

  else
    varHelp isGood newOffset row (col + 1) src indent context



-- SEQUENCES


sequence
  : { start : Token x
    , separator : Token x
    , end : Token x
    , spaces : Parser c x ()
    , item : Parser c x a
    , trailing : Trailing
    }
  -> Parser c x (List a)
sequence i =
  skip (token i.start) <|
  skip i.spaces <|
    sequenceEnd (token i.end) i.spaces i.item (token i.separator) i.trailing


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](http://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing = Forbidden | Optional | Mandatory


skip : Parser c x ignore -> Parser c x keep -> Parser c x keep
skip iParser kParser =
  map2 revAlways iParser kParser


revAlways : a -> b -> b
revAlways _ b =
  b


sequenceEnd : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> Trailing -> Parser c x (List a)
sequenceEnd ender ws parseItem sep trailing =
  let
    chompRest item =
      case trailing of
        Forbidden ->
          loop [item] (sequenceEndForbidden ender ws parseItem sep)

        Optional ->
          loop [item] (sequenceEndOptional ender ws parseItem sep)

        Mandatory ->
          ignorer
            ( skip ws <| skip sep <| skip ws <|
                loop [item] (sequenceEndMandatory ws parseItem sep)
            )
            ender
  in
  oneOf
    [ parseItem |> andThen chompRest
    , ender |> map (\_ -> [])
    ]


sequenceEndForbidden : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndForbidden ender ws parseItem sep revItems =
  let
    chompRest item =
      sequenceEndForbidden ender ws parseItem sep (item :: revItems)
  in
  skip ws <|
    oneOf
      [ skip sep <| skip ws <| map (\item -> Loop (item :: revItems)) parseItem
      , ender |> map (\_ -> Done (List.reverse revItems))
      ]


sequenceEndOptional : Parser c x () -> Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndOptional ender ws parseItem sep revItems =
  let
    parseEnd =
      map (\_ -> Done (List.reverse revItems)) ender
  in
  skip ws <|
    oneOf
      [ skip sep <| skip ws <|
          oneOf
            [ parseItem |> map (\item -> Loop (item :: revItems))
            , parseEnd
            ]
      , parseEnd
      ]


sequenceEndMandatory : Parser c x () -> Parser c x a -> Parser c x () -> List a -> Parser c x (Step (List a) (List a))
sequenceEndMandatory ws parseItem sep revItems =
  oneOf
    [ map (\item -> Loop (item :: revItems)) <|
        ignorer parseItem (ignorer ws (ignorer sep ws))
    , map (\_ -> Done (List.reverse revItems)) (succeed ())
    ]



-- WHITESPACE


spaces : Parser c x ()
spaces =
  chompWhile (\c -> c == ' ' || c == '\n' || c == '\r')


lineComment : Token x -> Parser c x ()
lineComment start =
  ignorer (token start) (chompUntilEndOr "\n")


multiComment : Token x -> Token x -> Nestable -> Parser c x ()
multiComment open close nestable =
  case nestable of
    NotNestable ->
      ignorer (token open) (chompUntil close)

    Nestable ->
      nestableComment open close


{-| Works just like [`Parser.Nestable`](Parser#nestable) to help distinguish
between unnestable `/*` `*/` comments like in JS and nestable `{-` `-}`
comments like in Elm.
-}
type Nestable = NotNestable | Nestable


nestableComment : Token x -> Token x -> Parser c x ()
nestableComment (Token oStr oX as open) (Token cStr cX as close) =
  case String.uncons oStr of
    Nothing ->
      problem oX

    Just (openChar, _) ->
      case String.uncons cStr of
        Nothing ->
          problem cX

        Just (closeChar, _) ->
          let
            isNotRelevant char =
              char /= openChar && char /= closeChar

            chompOpen =
              token open
          in
          ignorer chompOpen (nestableHelp isNotRelevant chompOpen (token close) cX 1)


nestableHelp : (Char -> Bool) -> Parser c x () -> Parser c x () -> x -> Int -> Parser c x ()
nestableHelp isNotRelevant open close expectingClose nestLevel =
  skip (chompWhile isNotRelevant) <|
    oneOf
      [ if nestLevel == 1 then
          close
        else
          close
            |> andThen (\_ -> nestableHelp isNotRelevant open close expectingClose (nestLevel - 1))
      , open
          |> andThen (\_ -> nestableHelp isNotRelevant open close expectingClose (nestLevel + 1))
      , chompIf isChar expectingClose
          |> andThen (\_ -> nestableHelp isNotRelevant open close expectingClose nestLevel)
      ]


isChar : Char -> Bool
isChar char =
  True
