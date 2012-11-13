#! /bin/bash
# .bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# bash setup
if [ -d "${HOME}/.bash" ] ; then
	for file in "${HOME}"/.bash/* ; do
		. "${file}"
	done
fi

# common shell setup
if [ -d "${HOME}/.commonsh" ] ; then
	for file in "${HOME}"/.commonsh/* ; do
		. "${file}"
	done
fi
