# HMGit

[![CI](https://github.com/falgon/hmgit/actions/workflows/build.yml/badge.svg)](https://github.com/falgon/hmgit/actions/workflows/build.yml)
[![CodeFactor](https://www.codefactor.io/repository/github/falgon/hmgit/badge)](https://www.codefactor.io/repository/github/falgon/hmgit)
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Ffalgon%2Fhmgit.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Ffalgon%2Fhmgit?ref=badge_shield)
<a href="./LICENSE">
<img src="https://img.shields.io/badge/license-BSD%203--Clause-blue.svg" alt="License" />
</a>
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-8D82AC.svg)
[![Documents](https://img.shields.io/badge/docs-available-blue.svg)](https://falgon.github.io/hmgit/)


Haskell Minimal Git (This is not made for practical purposes)

- Focusing on Git version 2.17.1
- This was made for [技術書店11](https://techbookfest.org/event/tbf11)

## Usage

HMGit has 8 commands: `add`, `cat-file`, `diff`, `hash-object`, `init`, `ls-files`, `status`, `commit`

```bash
$ stack exec hmgit -- --help
    __  ____  __________ __
   / / / /  |/  / ____(_) /_
  / /_/ / /|_/ / / __/ / __/
 / __  / /  / / /_/ / / /_
/_/ /_/_/  /_/\____/_/\__/

Usage: hmgit [--version] [--db-name <database name>] COMMAND
  The subset of awesome content tracker Git
  Version: 1.0.0
  Commit hash: 76a315cce6bdcfe428338dca5466849eab3868d4

Available options:
  -h,--help                Show this help text
  --version                Prints the HMGit suite version that the hmgit program
                           came from.
  --db-name <database name>
                           hmgit database name

Available commands:
  add                      Add file contents to the index
  cat-file                 Provide content or type and size information for
                           repository objects
  diff                     Show changes between commits, commit and working
                           tree, etc
  hash-object              Compute object ID and optionally creates a blob from
                           a file
  init                     Create an empty Git repository or reinitialize an
                           existing one
  ls-files                 Show information about files in the index and the
                           working tree
  status                   Show the working tree status
  commit                   Record changes to the repository
```

Each command can display help.

```bash
$ stack exec hmgit -- add --help # e.g. add
Usage: hmgit add [-n|--dry-run] [<pathspec>...]
  Add file contents to the index

Available options:
  -h,--help                Show this help text
  -n,--dry-run             Don’t actually add the file(s), just show if they
                           exist and/or will be ignored.
  <pathspec>...            Files to add content from. see the pathspec entry in
                           gitglossary(7).
```

The operation from initialization to commit can be reproduced as follows
(Currently it is possible to push to a remote repository with the help of git).

```bash
$ stack install
$ cd ~/workdir

# init
$ hmgit --db-name .git init test-hmgit && cd test-hmgit
Initialized empty HMGit repository in: test-hmgit

# check status
$ echo hello git > hello
$ hmgit --db-name .git status
New files:
        hello

# staging
$ hmgit --db-name .git add hello
$ hmgit --db-name .git status

# commit
$ hmgit --db-name .git commit -m 'hello git commit' --author "roki <roki@example.com>"
[main (commit) ec891bf] hello git commit

# push (git power)
$ git remote add origin git@github.com:falgon/hmgit-playground.git
$ git push origin main
Counting objects: 3, done.
Writing objects: 100% (3/3), 211 bytes | 211.00 KiB/s, done.
Total 3 (delta 0), reused 0 (delta 0)
To github.com:falgon/hmgit-playground.git
 + 41dab4a...ec891bf main -> main

# diff
$ echo meow >> hello
$ hmgit --db-name .git status
Changes files:
        hello
$ hmgit --db-name .git diff
--- a/hello
+++ b/hello
@@
 hello git
+meow

# cat-file
$ find .git/objects -type f | cut -d/ -f3,4 | sed 's$/$$g' | xargs -n1 hmgit --db-name .git cat-file -t
commit
tree
blob
$ find .git/objects -type f | cut -d/ -f3,4 | sed 's$/$$g' | xargs -n1 hmgit --db-name .git cat-file -s
163
33
10
$ find .git/objects -type f | cut -d/ -f3,4 | sed 's$/$$g' | xargs -n1 hmgit --db-name .git cat-file -p
tree 66eea8c80abea0e9836aab458e48ab9a379186e5
author roki <roki@example.com> 1621539748 +0900
committer roki <roki@example.com> 1621539748 +0900

hello git commit
100644 blob 8d0e41234f24b6da002d962a26c2495ea16a425f    hello
hello git

# hash-object
$ echo bow | hmgit --db-name .git hash-object --stdin
de03f25c7324281fc5b6f146fe273fa85b689690

# search recursively with pathspecs (subset)
$ mkdir -p foo/hoge/piyo foo/hoge/bar
$ echo "main = pure ()" > foo/hoge/piyo/a.hs
$ echo "main = pure ()" > foo/hoge/bar/b.hs
$ echo "main = pure ()" > c1.hs
$ hmgit --db-name .git status '*.hs'
New files:
        foo/hoge/piyo/a.hs
        foo/hoge/bar/b.hs
        c1.hs
$ hmgit --db-name .git status '?1.hs'
New files:
        c1.hs

# ls-files
$ hmgit --db-name .git add .
$ hmgit --db-name .git ls-files '*.hs'
c1.hs
foo/hoge/bar/b.hs
foo/hoge/piyo/a.hs
$ hmgit --db-name .git ls-files -s '*.hs'
100644 76a9bdb5d48831da2e59c811ecbcbb5a379bdfb5 0       c1.hs
100644 76a9bdb5d48831da2e59c811ecbcbb5a379bdfb5 0       foo/hoge/bar/b.hs
100644 76a9bdb5d48831da2e59c811ecbcbb5a379bdfb5 0       foo/hoge/piyo/a.hs
```

## Build

```bash
$ stack build
```

## Test

```bash
$ stack test
```

## License

[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Ffalgon%2Fhmgit.svg?type=large)](https://app.fossa.com/projects/git%2Bgithub.com%2Ffalgon%2Fhmgit?ref=badge_large)
