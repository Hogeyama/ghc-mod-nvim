
# ghc-mod-nvim

Wrapper plugin of [ghc-mod](https://hackage.haskell.org/package/ghc-mod) for nvim-hs,

## Commands

#### `:NeoGhcModCheck` `:NeoGhcModLint`

Runs `ghc-mod (check|lint)` and puts the result in quickfix window.


#### `:NeoGhcModInfo`

Put information of a given identifier (if not given, the identifier under the cursor)
to quickfix.


#### `:NeoGhcModType`

Put the type of expression under the cursor. With bang (`:NeoGhcModType!`)
yank the result.

