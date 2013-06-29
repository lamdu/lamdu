import "hint" HLint.HLint

ignore "Avoid lambda" = Lamdu.Sugar.AddNames

infixr 4 %%@~, <%@~, %%~, <+~, <*~, <-~, <//~, <^~, <^^~, <**~
infix 4 %%@=, <%@=, %%=, <+=, <*=, <-=, <//=, <^=, <^^=, <**=
infixr 2 <<~
infixr 9 #.
infixl 8 .#
infixr 8 ^!, ^@!
infixl 1 &, <&>, ??
infixl 8 ^., ^@.
infixr 9 <.>, <., .>
infixr 4 %@~, .~, +~, *~, -~, //~, ^~, ^^~, **~, &&~, <>~, ||~, %~
infix 4 %@=, .=, +=, *=, -=, //=, ^=, ^^=, **=, &&=, <>=, ||=, %=
infixr 2 <~
infixr 2 `zoom`, `magnify`
infixl 8 ^.., ^?, ^?!, ^@.., ^@?, ^@?!
infixl 8 ^#
infixr 4 <#~, #~, #%~, <#%~, #%%~
infix 4 <#=, #=, #%=, <#%=, #%%=
infixl 9 :>
infixr 4 </>~, <</>~, <.>~, <<.>~
infix 4 </>=, <</>=, <.>=, <<.>=
infixr 4 .|.~, .&.~, <.|.~, <.&.~
infix 4 .|.=, .&.=, <.|.=, <.&.=
