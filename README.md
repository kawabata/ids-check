# IDS check tool

## Abstract

This tool checks the validity and unifiability ideographs based on its
IDS (Ideographic Description Sequence).

## How to check IDS

You need [Emacs](https://www.gnu.org/software/emacs/) (version 24.4 or
later) and [Cask](https://github.com/cask/cask). Then, prepare the IDS
data text file and run the following.

```
% cask install
```
(This will install all necessary libraries)

```
% cask exec emacs --script ids-check.el file1 file2 ...
```

(It may take several minutes to re-normalize all internal IDS data.)

*Files* should be tab-separated `Identifier vs IDS' data. When
multiple files are specified, they are all checked against each other.

If an identifier or an IDS are **not** placed in proper columns, you
can specify columns by attaching **",num,num"** at the end of each file.

For example, if your IDS data contains identifier and IDS in 1st and
3rd columns, as
[extf-ids.txt](https://github.com/cjkvi/cjkvi-ids/blob/master/extf-ids.txt),
then you can specify as following.

```
% cask exec emacs --script ids-check.el extf-ids.txt,1,3
```
