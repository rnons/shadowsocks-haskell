# shadowsocks-haskell

[![Hackage](https://img.shields.io/hackage/v/shadowsocks.svg)](https://hackage.haskell.org/package/shadowsocks)

Shadowsocks in Haskell. Original python version: https://github.com/clowwindy/shadowsocks

Compatible with other versions of shadowsocks.

## Install from hackage

You need to have `ghc` and `cabal` installed first. See https://www.haskell.org/downloads.

```
cabal install shadowsocks
```

## Build from source

You need to have [stack](https://haskellstack.org) installed first.

```
# build
stack build
# run local
stack exec sslocal
# run remote
stack exec ssserver
```

Or run `stack install` directly, which will copy sslocal/ssserver to `~/.local/bin`.
