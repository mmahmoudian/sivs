## 0.2.9

[fix] Adding `markdown` package to the list of suggestions to bypass the vignette building error.

## 0.2.8

- `is.sivs()`
    - [fix] This function was not properly exported and in this version is correctly exported.

## 0.2.7

- `suggest()`
    - [fix] the function was throwing uncatched error when the criteria was strict enough not to pass any features. Now it wil return `NULL` along with a `message`. ([issue #4](https://github.com/mmahmoudian/sivs/issues/4))

## 0.2.6

The package is now built against the R 4.2.2, but nothing is preventing the user to build it agains any other minor version of R version 4. To assist this process, the `makefile` was updated to make the process smoother (try `make help`).

- `sivs()`
    - [fix] [a bug](https://github.com/mmahmoudian/sivs/issues/3) was fixed that was throwing error when number fo features were less than available cores of the CPU.
    - [fix] some part of the code was still using the old way of comparing class of objects. Those were migrated to `inherits()`

## 0.2.5

There is no functional changes in the R codebase. All changes are in the documentation to add the citation information. If you are interested in knowing the changes in details, please read the git commits in the project's Github page.

## 0.2.4

All the changes are minor and does not affect any of the numeric outcomes of the package.

- `sivs()`
    - [add] a debug mode was added to the sivs(). It can be turned on by `debug.mode = TRUE`
    - [fix] the code is now complient with R v4.x.x as it uses `inherit()` to check class for array objects
    - [fix] the importance levels of some verbosity messages is now corrected to be uniformed
    - [update] the output of the parallel clusters are now sent to `/dev/null` (or its alternative in Windows) by default. Use `debug.mode = TRUE` to send them to stdout
    - [fix] the sessionInfo is not correctly returned in the final object
