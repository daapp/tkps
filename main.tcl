#! /bin/sh
# \
exec tclsh "$0" ${1+"$@"}

set __app__ tkps

if {[lsearch -glob -index 1 [info loaded] tclkit*] > -1} {
    package require starkit
    if {[starkit::startup] ne "sourced"} {
	source [file join $starkit::topdir bin $__app__]
    }
} else {
    source [file join [file dirname [info script]] bin $__app__]
}
