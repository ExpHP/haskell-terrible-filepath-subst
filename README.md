haskell-terrible-filepath-subst
===============================

ergonomic filepath substition library for those who fear the Shift key (and have an American keyboard)

for instance, if you wanted to rename a bunch of jpegs to jpg:

```text
    pattern:  [].jpeg
replacement:  [].jpg
```

or if you have files like "dirname/file.ext" and want to rename them to "file-dirname.ext":

```text
    pattern:  [d]/[f].[e]
replacement:  [f]-[d].[e]
```

notice I haven't shown any examples of how to use the actual API of this library; that's because I don't even remember how to use it. [Here is the single place I use the API of this library directly in all of my code.](https://github.com/ExpHP/calc-tblg/blob/c4dc67b/src/ShakeUtil/Defs.hs#L83-L112) (but note that single usage is doing a lot of legwork, as in the rest of my code I am writing [things like this](https://github.com/ExpHP/calc-tblg/blob/c4dc67b/src/ShakeUtil/Defs.hs#L83-L112))
