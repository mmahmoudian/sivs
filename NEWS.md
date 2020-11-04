## 0.2.4

All the changes are minor and does not affect any of the numeric outcomes of the package.

- `sivs()`
    - [add] a debug mode was added to the sivs(). It can be turned on by `debug.mode = TRUE`
    - [fix] the code is now complient with R v4.x.x as it uses `inherit()` to check class for array objects
    - [fix] the importance levels of some verbosity messages is now corrected to be uniformed
    - [update] the output of the parallel clusters are now sent to `/dev/null` (or its alternative in Windows) by default. Use `debug.mode = TRUE` to send them to stdout
    - [fix] the sessionInfo is not correctly returned in the final object