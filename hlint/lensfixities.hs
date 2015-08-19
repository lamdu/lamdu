import "hint" HLint.HLint

ignore "Use const"
ignore "Avoid lambda" = Lamdu.Sugar.Names

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
