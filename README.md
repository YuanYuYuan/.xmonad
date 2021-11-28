# Learn Haskell with XMonad!

Use XMonad built from source under stand alone Haskell environment.
Pre-installed Haskell is not required. You can install and use stack
to build the source code.

## Install XMonad & XMonad-contrib

```bash
submodule update --recursive
stack install
```

## Upgrade XMonad & XMonad-contrib

```bash
git submodule foreach git pull origin master
stack install
```

## Usage

Launch XMonad

```bash
exec xmonad
```

Build and restart XMonad

```bash
xmonad --compile && xmonad --restart
```

## Haskell Language Server

We may need a HLS to make coding `xmonad.hs` more comfortable.
Just install a collection of static HLS from AUR.

```bash
paru -S Haskell-language-server-static
```


