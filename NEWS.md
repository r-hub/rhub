# devel

## New features

* The output of `list_my_checks()` and `list_package_checks()` gained a 
`get_group()` and a `get_check()` methods returning a `rhub_check` object 
from the list, by group_id or check_id.

* The output of `check()`, `check_on_` and `check_for_cran()` functions 
gained a `browse()` method replacing the `web()` method for opening the 
URLs corresponding to a `rhub_check` object.

## Bug fixes

* In printing methods the submitted time is now always correct thanks to 
explicitly specifying units for `as.numeric.difftime` (@jimhester, #94 and 
@schloerke, #135).

# 1.0.2

First public release.
