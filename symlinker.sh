#!/usr/bin/sh

echo "Creating symbolic link of dotfiles..."

echo `dirname $0`

cd `dirname $0`

for filepath in .*
do
    ln -s $filepath ~/
done

for filepath in *
do
    ln -s $filepath ~/
done
