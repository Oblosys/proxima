#!/bin/bash
set -o nounset -o errexit -o pipefail

# Add sandbox bin to path for accessing build tools during install
export PATH="$PATH:`pwd`/.cabal-sandbox/bin"

pushd dazzle-editor/
cabal sandbox init
popd
pushd multi-editor/
cabal sandbox init --sandbox=../dazzle-editor/.cabal-sandbox
popd
pushd helium-editor/
cabal sandbox init --sandbox=../dazzle-editor/.cabal-sandbox
popd
pushd ../proxima-generator
cabal sandbox init --sandbox=../proxima/dazzle-editor/.cabal-sandbox
popd

pushd dazzle-editor/
cabal update
cabal install alex-3.1.7 --bindir=$HOME/.cabal/bin
cabal install happy-1.19.5 --bindir=$HOME/.cabal/bin
cabal install uuagc-0.9.52 --bindir=$HOME/.cabal/bin
popd
pushd ../proxima-generator
cabal install
popd

# Build Proxima instances

pushd dazzle-editor
cabal install
popd
pushd helium-editor
cabal install
popd

# Create run-time directories for multi-editor variations

mkdir declaration-form
cp -r multi-editor/ declaration-form
cp multi-editor/Document-declaration-form.xml declaration-form/Document.xml
cp multi-editor/src/Settings-declaration-form.hs declaration-form/src/Settings.hs
pushd declaration-form
cabal install
popd
mv $HOME/.cabal/bin/multi-editor $HOME/.cabal/bin/declaration-form

mkdir task-list
cp -r multi-editor/* task-list
cp multi-editor/Document-task-list.xml task-list/Document.xml
cp multi-editor/src/Settings-task-list.hs task-list/src/Settings.hs
pushd task-list
cabal install
popd
mv $HOME/.cabal/bin/multi-editor $HOME/.cabal/bin/task-list

mkdir sudoku
cp -r multi-editor/* sudoku
cp multi-editor/Document-sudoku.xml sudoku/Document.xml
cp multi-editor/src/Settings-sudoku.hs sudoku/src/Settings.hs
pushd sudoku
cabal install
popd
mv $HOME/.cabal/bin/multi-editor $HOME/.cabal/bin/sudoku

mkdir styled-text
cp -r multi-editor/* styled-text
cp multi-editor/Document-styled-text.xml styled-text/Document.xml
cp multi-editor/src/Settings-styled-text.hs styled-text/src/Settings.hs
pushd styled-text
cabal install
popd
mv $HOME/.cabal/bin/multi-editor $HOME/.cabal/bin/styled-text
