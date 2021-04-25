data ValidPassword : (min: Nat) -> (max: Nat) -> Type where
  MkPass: (xs: String) ->
          { auto p: length xs >= min && length xs <= max = True } ->
          ValidPassword min max

validPassword : ValidPassword 4 8
validPassword = MkPass "abcd"

invalidPassword : ValidPassword 4 8
invalidPassword = MkPass "abcd"

-- see https://github.com/hackle/idris/blob/master/range.idr