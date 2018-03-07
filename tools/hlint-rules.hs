infixr 6 <>

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

warn = a ^. b ^. c ==> a ^. b . c
warn = a ^. b ^? c ==> a ^? b . c
warn = a ^? b <&> (^. c) ==> a ^? b . c
warn = a ^? b . _Just ==> a ^. b
warn = Control.Lens.at a . _Just ==> Control.Lens.ix a
warn = fromJust (a ^? b) ==> a ^?! b
warn = a .~ Just b ==> a ?~ b
warn = a & Control.Lens.mapped %~ b ==> a <&> b
warn = a & Control.Lens.mapped . b %~ c ==> a <&> b %~ c
warn = a & Control.Lens.mapped .~ b ==> b <$ a
warn = Control.Monad.Reader.ask <&> (^. a) ==> Control.Lens.view a
warn = (^. a) <$> Control.Monad.Reader.ask ==> Control.Lens.view a
warn = fmap (^. a) Control.Monad.Reader.ask ==> Control.Lens.view a
warn = Control.Lens.view a <&> (^. b) ==> Control.Lens.view (a . b)
warn = a & b .~ a ^. b ==> a
warn = a ^. b & c .~ a ^. b . c ==> a ^. b
warn = pure a <&> b ==> pure (a b)
warn = return a <&> b ==> return (a b)
warn = maybe mempty ==> foldMap
warn = lift a >>= lift . b ==> a >>= b & lift
warn = a & lift >>= lift . b ==> a >>= b & lift
warn = a & lift <&> b >>= lift . c ==> a <&> b >>= c & lift
warn = head (a ++ [b]) ==> fromMaybe b (a ^? traverse)
-- Lamdu style rules:
warn = return ==> pure

warn = mconcat [a | x <- take 1 b] ==> foldMap (\x -> a) (b ^? traverse)
