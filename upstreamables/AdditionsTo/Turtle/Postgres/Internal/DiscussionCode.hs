{-# LANGUAGE OverloadedStrings #-}

{-
    This file is for discussion only. "inplace_" and "sed_" are from an earlier
    version of sdb. I rename them as "maybeInplace" and "maybeSed" here for
    clarity.

    1) It appears that use of "Maybe" in the types of "maybeInplace" and
        "maybeSed" is unnecessary. Seemingly, the "Maybe" was used to skip over
        comment lines and "empty" lines (those containing only whitespace): a
        Pattern that recognizes such "fluff" and returns "Nothing" causes
        maybeInplace to skip over superfluous lines.
        But this is unneeded: the original "inplace" already skips over (does
        not alter) lines which do not meet the pattern.

    2) So we don't even need to recognize comment/empty lines at all, let alone
        have Patterns that can contain Nothing: we just let superfluous lines
        be unrecognized (not matched), and "inplace" skips over them.

        Note that the old version of sdb depended on
        "commentOrEmpty >> pure Nothing" being last in the list. This gave it
        the lowest priority amongst the other patterns with "choice": otherwise
        a line containing a comment to the side, such as "foo = bar  # Comment"
        could be recognized as a comment for containing a substring starting
        with zero or more whiespace characters followed by a '#'.

    3) A comment about using "begins" with "contains":
        Any string that contains whitespace also contains a sub-string that
        begins with whitespace. So we have no need for the "begins" used within
        "contains".

    4) Comment-loosing bug:
        Suppose someone comments-out an option in postgres.conf:

            # foo = bar

        Right now, foo is not set to bar, but this could easily be done by
        un-commenting the line. Now suppose we use our Haskell code to set
        foo to salad. With the old version of sdb, the above would be recognized
        as matching our Pattern and be replaced with "foo = salad", loosing
        the comment.

    5) "once" line not needed:
        In optSettingPattern, once we find a key in a line known to not be
        a comment, that's all we need to know to go ahead and replace the line.

    5) No need to look for newlines:
        "commentOrEmpty" used to use "newline" in its definition. However,
        inplace and maybeInplace go line-by-line through a file. They use
        "input", which gives a "Shell Line". So we are gauranteed to never
        encounter newlines in our Patterns.

    So, I have made sdb use the original "inplace" function. This also means
    that we no longer need "maybeSed".
-}



-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Data.Text as Txt
import qualified System.IO as H

import Turtle

-- | Just like "inplace", but accepts a "Pattern (Maybe Text)".
-- (See Turtle.Pattern doc about inplace: it's a line-by-line file manipulator.)
-- Any sub-pattern within the given Pattern that contains "Nothing" is converted
-- into a Pattern that matches nothing. That conversion happens in maybeSed.
maybeInplace :: MonadIO io => Pattern (Maybe Text) -> FilePath -> io ()
maybeInplace pat file = liftIO (runManaged (do
    here              <- pwd
    (tmpfile, handle) <- mktemp here "turtle"
    outhandle handle (maybeSed pat (input file))
    liftIO (H.hClose handle)
    mv tmpfile file ))

-- | Just like "sed", but accepts a "Pattern (Maybe Text)".
-- (See Turtle.Pattern doc about sed.)
-- Note "flatten" is defined in the "where" clause. There, "my" has type
-- m (Maybe a), where "m" is a monad and "a" is anything. The "<- my" takes the
-- monad off and "y" is an "a". If "my" contains "Nothing", the pattern match
-- fails and m's "fail" function is envoked. When "m" is "Pattern", the fail
-- function gives us a Pattern that does not match any Text.
maybeSed :: Pattern (Maybe Text) -> Shell Line -> Shell Line
maybeSed pat orig = flatten $ do
    when (matchesEmpty pat) (die message)
    let pat' = fmap mconcat
            (many (pat <|> fmap (Just . Txt.singleton) anyChar))
    txt    <- lineToText <$> orig
    txt':_ <- return (match pat' txt)
    return (unsafeTextToLine <$> txt')
  where
    message = "sed: the given pattern matches the empty string"
    matchesEmpty = not . null . flip match ""
    flatten my = do
        Just y <- my
        return y


{-
   Please fiddle with the below. You can try the "Maybe" variant of "pattern"
   as well as the plain version, both given below. And you can try
   "maybeInplace" vs "inplace". A good test file is:

        foo
        skip
        bar

   Below, main switches "foo" and "bar", but leaves "skip" unaltered: neither
   erroring nor deleting the line.
-}
-- main = inplace pattern (Your test file here)
{-
pattern :: Pattern (Maybe Text)
pattern = choice [ foo, bar ]

foo :: Pattern (Maybe Text)
foo = text "foo" >> pure (Just "bar")

bar :: Pattern (Maybe Text)
bar = text "bar" >> pure (Just "foo")
-}

pattern :: Pattern Text
pattern = choice [ foo, bar ]

foo :: Pattern Text
foo = text "foo" >> pure "bar"

bar :: Pattern Text
bar = text "bar" >> pure "foo"


-- Though not in use anymore, I simplified the old code below:


------------------------- Defining "fluffBecomesNothing" -----------------------

-- | Pattern that matches comment/empty lines (fluff) and returns "Nothing".
-- (Technically, it works on "Maybe Lines".)
fluffBecomesNothing :: Pattern (Maybe Line)
fluffBecomesNothing = fluff >> pure Nothing where fluff = commentOrEmpty

-- | Recognizes a Line that is either a Postgres comment or is empty:
commentOrEmpty :: Pattern Line
commentOrEmpty =
    -- "<|>" is the Alternative type class operator. For "Pattern", it means
    -- "or".
    comment  <|>  empty

-- | Identifies a Postgres comment, when given the line and just the line:
comment :: Pattern Line
comment =
    {-
       Note that ">>" is very analogous to string concatenation here:
       we're concatenating an "abstract string": one that starts with zero or
       more whitespace characters, immediately followed by a '#', immediately
       followed by zero or more anythings. (But "Line" instead of "String")
       ("lineOnce" converts a "Pattern Char" into a "Pattern Line".)
    -}
    lineWhitespace >> (lineOnce $ char '#') >> lineChars

-- | Identifies empty Lines: zero or more whitespace chars, but no newlines:
-- (If it had a newline, it would not be a legal "Line".)
empty :: Pattern Line
empty = lineWhitespace

--------------------- End of Defining "fluffBecomesNothing" --------------------
