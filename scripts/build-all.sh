#!/bin/bash
set -o errexit -o pipefail

cabal update

# Install build tools from a sandbox to $HOME/.cabal/bin (cabal new-build doesn't support tools yet)

mkdir tmp
pushd tmp
cabal sandbox init
cabal install alex-3.1.7 --bindir=$HOME/.cabal/bin
cabal install happy-1.19.5 --bindir=$HOME/.cabal/bin
cabal install uuagc-0.9.52 --bindir=$HOME/.cabal/bin
cabal sandbox delete
popd
rmdir tmp

cp-newstyle-executable() { # Until cabal supports cabal run, assumes version 0.1
  cp dist-newstyle/build/$1-0.1/build/$1/$1 $HOME/.cabal/bin/${2:-$1}
}

pushd proxima-generator
cabal new-build
cp-newstyle-executable proxima-generator
popd

# Build Proxima instances
pushd proxima

pushd dazzle-editor
cabal new-build
cp-newstyle-executable dazzle-editor
popd
pushd helium-editor
cabal new-build
cp-newstyle-executable helium-editor
popd

# Create run-time directories for multi-editor variations

mkdir -p declaration-form
cp -r multi-editor/* declaration-form
cp multi-editor/Document-declaration-form.xml declaration-form/Document.xml
cp multi-editor/src/Settings-declaration-form.hs declaration-form/src/Settings.hs
pushd declaration-form
cabal new-build
cp-newstyle-executable multi-editor declaration-form
popd

mkdir -p task-list
cp -r multi-editor/* task-list
cp multi-editor/Document-task-list.xml task-list/Document.xml
cp multi-editor/src/Settings-task-list.hs task-list/src/Settings.hs
pushd task-list
cabal new-build
cp-newstyle-executable multi-editor task-list
popd

mkdir -p sudoku
cp -r multi-editor/* sudoku
cp multi-editor/Document-sudoku.xml sudoku/Document.xml
cp multi-editor/src/Settings-sudoku.hs sudoku/src/Settings.hs
pushd sudoku
cabal new-build
cp-newstyle-executable multi-editor sudoku
popd

mkdir -p styled-text
cp -r multi-editor/* styled-text
cp multi-editor/Document-styled-text.xml styled-text/Document.xml
cp multi-editor/src/Settings-styled-text.hs styled-text/src/Settings.hs
pushd styled-text
cabal new-build
cp-newstyle-executable multi-editor styled-text
popd

popd
