module Main where

import Neovim
import Neovim.GhcModNvim (plugin)

main :: IO ()
main = neovim defaultConfig { plugins = [ plugin ] }
