-- Lambda terms in unary de Bruijn notation
@generate Lambda
@size 1000000

-- Natural size notion
Lambda = Abs Lambda | App Lambda Lambda | Index DeBruijn (0).
DeBruijn = S DeBruijn | Z.

{- Note that if no size is given one is assumed.
   Index gets size zero, to avoid counting the
   type conversion into the term size. -}
