# Rewritten in modern Tcl/Tk by Alexander Danilov (alexander.a.danilov@gmail.com)
#
# Adapted/enhanced for Linux by Ed Petron (epetron@leba.net)
# Kidong Lee (kidong@shinbiro.com) May 1997
#
# Originally written by Henry Minsky (hqm@ai.mit.edu) May 1994
#

################################################################
# You can get the implementation dependent signal names for your system
# from /usr/include/signal.h
#
package require Tk
package require Ttk

tcl::tm::path add $env(HOME)/lib/tcl

package require tkapp
package require widget::scrolledwindow
package require widget::dialog
package require tablelist
package require textutil
package require fileutil
package require lambda

namespace import tkapp::w


namespace eval tkps {
    proc constant {name value} {
        uplevel [list trace variable $name rw [list ::apply {{val name1 name2 opts} {
            upvar $name1 var
            if {$opts eq "w"} {
                return -code error "\"$val\" is read only "
            } else {
                set var $val
            }
        }} $value]]
    }

    constant APPLICATION tkps
    constant VERSION 1.99

    variable SIGNALS
    array set SIGNALS {
        COMMON {
            {INT         2        interupt}
            {QUIT        3        quit}
            {IOT         6        abort}
            {KILL        9        non-catchable, non-ignorable kill}
            {STOP        17       sendable stop signal not from tty}
            {ALRM        14       alarm clock}
            {TERM        15       software termination signal}
        }

        ALL {
            {HUP         1         hangup}
            {INT         2         interrupt}
            {QUIT        3         quit}
            {ILL         4         illegal instruction (not reset when caught)}
            {TRAP        5         trace trap (not reset when caught)}
            {ABRT        6         abort()}
            {IOT         6         SIGABRT compatibility}
            {EMT         7         EMT instruction}
            {FPE         8         floating point exception}
            {KILL        9         kill (cannot be caught or ignored)}
            {BUS         10        bus error}
            {SEGV        11        segmentation violation}
            {SYS         12        bad argument to system call}
            {PIPE        13        write on a pipe with no one to read it}
            {ALRM        14        alarm clock}
            {TERM        15        software termination signal from kill}
            {URG         16        urgent condition on IO channel}
            {STOP        17        sendable stop signal not from tty}
            {TSTP        18        stop signal from tty}
            {CONT        19        continue a stopped process}
            {CHLD        20        to parent on child stop or exit}
            {TTIN        21        to readers pgrp upon background tty read}
            {TTOU        22        like TTIN for output if (tp->t_local&LTOSTOP)}
            {IO          23        input/output possible signal}
            {XCPU        24        exceeded CPU time limit}
            {XFSZ        25        exceeded file size limit}
            {VTALRM      26        virtual time alarm}
            {PROF        27        profiling time alarm}
            {WINCH       28        window size changes}
            {INFO        29        information request}
            {USR1        30        user defined signal 1}
            {USR2        31        user defined signal 2}
        }

        POSIX {
            {HUP         1         hangup}
            {INT         2         interrupt}
            {QUIT        3         quit}
            {ILL         4         illegal instruction (not reset when caught)}
            {ABRT        6         abort()}
            {FPE         8         floating point exception}
            {KILL        9         kill (cannot be caught or ignored)}
            {SEGV        11        segmentation violation}
            {PIPE        13        write on a pipe with no one to read it}
            {ALRM        14        alarm clock}
            {TERM        15        software termination signal from kill}
            {STOP        17        sendable stop signal not from tty}
            {TSTP        18        stop signal from tty}
            {CONT        19        continue a stopped process}
            {CHLD        20        to parent on child stop or exit}
            {TTIN        21        to readers pgrp upon background tty read}
            {TTOU        22        like TTIN for output if (tp->t_local&LTOSTOP)}
            {USR1        30        user defined signal 1}
            {USR2        31        user defined signal 2}
        }
    }

    variable common_ps_keywords {
        {%cpu       percentage cpu usage (alias pcpu)}
        {%mem       percentage memory usage (alias pmem)}
        {uid        effective user ID}
        {user       user name (from uid)}
        {majflt     total page faults}
        {minflt     total page reclaims}
        {msgrcv     total messages received (reads from pipes/sockets)}
        {msgsnd     total messages sent (writes on pipes/sockets)}
        {vsz        virtual size in Kbytes (alias vsize)}
        {nice       nice value (alias ni)}
        {nsigs      total signals taken (alias nsignals)}
        {nswap      total swaps in/out}
        {pgid       process group number}
        {pid        process ID}
        {ppid       parent process ID}
        {rgid       real group ID}
        {ruid       real user ID}
        {ruser      user name (from ruid)}
        {start      time started}
        {time       accumulated cpu time, user + system (alias cputime)}
        {tpgid      control terminal process group ID}
        {tsiz       text size (in Kbytes)}
        {tty        full name of control terminal}
        {lim        memoryuse limit}
        {logname    login name of user who started the process}
    }

    variable ALL_ps_keywords {
        {%cpu       percentage cpu usage (alias pcpu)}
        {%mem       percentage memory usage (alias pmem)}
        {acflag     accounting flag (alias acflg)}
        {cpu        short-term cpu usage factor (for scheduling)}
        {inblk      total blocks read (alias inblock)}
        {jobc       job control count}
        {ktrace     tracing flags}
        {ktracep    tracing vnode}
        {lim        memoryuse limit}
        {lstart     time started}
        {majflt     total page faults}
        {minflt     total page reclaims}
        {msgrcv     total messages received (reads from pipes/sockets)}
        {msgsnd     total messages sent (writes on pipes/sockets)}
        {nice       nice value (alias ni)}
        {nivcsw     total involuntary context switches}
        {nsigs      total signals taken (alias nsignals)}
        {nswap      total swaps in/out}
        {nvcsw      total voluntary context switches}
        {nwchan     wait channel (as an address)}
        {oublk      total blocks written (alias oublock)}
        {p_ru       resource usage (valid only for zombie)}
        {paddr      swap address}
        {pagein     pageins (same as majflt)}
        {pgid       process group number}
        {pid        process ID}
        {ppid       parent process ID}
        {pri        scheduling priority}
        {re         core residency time (in seconds; 127 = infinity)}
        {rgid       real group ID}

        {rlink      reverse link on run queue, or 0}
        {rss        resident set size}
        {rsz        resident set size + (text size / text use count) (alias rs- size)}
        {ruid       real user ID}
        {ruser      user name (from ruid)}
        {sess       session pointer}
        {sig        pending signals (alias pending)}
        {sigcatch   caught signals (alias caught)}
        {sigignore  ignored signals (alias ignored)}
        {sigmask    blocked signals (alias blocked)}
        {sl         sleep time (in seconds; 127 = infinity)}
        {start      time started}
        {svgid      saved gid from a setgid executable}
        {svuid      saved uid from a setuid executable}
        {tdev       control terminal device number}
        {time       accumulated cpu time, user + system (alias cputime)}
        {tpgid      control terminal process group ID}
        {tsess      control terminal session pointer}
        {tsiz       text size (in Kbytes)}
        {tt         control terminal name (two letter abbreviation)}
        {tty        full name of control terminal}
        {ucomm      name to be used for accounting}
        {uid        effective user ID}
        {upr        scheduling priority on return from system call (alias usrpri)}
        {user       user name (from uid)}
        {vsz        virtual size in Kbytes (alias vsize)}
        {wchan      wait channel (as a symbolic name)}
        {xstat      exit or stop status (valid only for stopped or zombie process)}
        {logname    login name of user who started the process}
    }

    variable PROCESS_FLAGS {
        {SLOAD         0x0000001     in core}
        {SSYS          0x0000002     swapper or pager process}
        {SLOCK         0x0000004     process being swapped out}
        {SSWAP         0x0000008     save area flag}
        {STRC          0x0000010     process is being traced}
        {SWTED         0x0000020     another tracing flag}
        {SSINTR        0x0000040     sleep is interruptible}
        {SPAGE         0x0000080     process in page wait state}
        {SKEEP         0x0000100     another flag to prevent swap out}
        {SOMASK        0x0000200     restore old mask after taking signal}
        {SWEXIT        0x0000400     working on exiting}
        {SPHYSIO       0x0000800     doing physical I/O}
        {SVFORK        0x0001000     process resulted from vfork(2)}
        {SVFDONE       0x0002000     another vfork flag}
        {SNOVM         0x0004000     no vm, parent in a vfork}
        {SPAGV         0x0008000     init data space on demand, from vnode}
        {SSEQL         0x0010000     user warned of sequential vm behavior}
        {SUANOM        0x0020000     user warned of random vm behavior}
        {STIMO         0x0040000     timing out during sleep}
        {SNOCLDSTOP    0x0080000     no SIGCHLD when children stop}
        {SCTTY         0x0100000     has a controlling terminal}
        {SOWEUPC       0x0200000     owe process an addupc() call at next}
        {SSEL          0x0400000     selecting; wakeup/waiting danger}
        {SEXEC         0x0800000     process called exec(2)}
        {SHPUX         0x1000000     HP-UX process (HPUXCOMPAT)}
        {SULOCK        0x2000000     locked in core after swap error}
        {SPTECHG       0x4000000     pte's for process have changed}
    }

    variable state_fields {
        {D Process in disk (or other short term, uninterruptable) wait.}
        {I Process that is idle (sleeping for longer than about 20 seconds).}
        {P Process in page wait.}
        {R Process is Runnable.}
        {S Process is sleeping for less than about 20 seconds.}
        {T Process is stopped.}
        {Z Process is dead (a ``zombie'').}
        {+ Process is in the foreground process group of its control terminal.}
        {< Process has raised CPU scheduling priority.}
        {> Process has specified a soft limit on memory requirements and is currently exceeding that limit; such a pro cess is (necessarily) not swapped.}
        {A  Process has asked for random page replacement (VA_ANOM, from vadvise(2),  for example, lisp(1) in a garbage collect).}
        {E The process is trying to exit.}
        {L The process has pages locked in core (for example, for raw I/O).}
        {N The process has reduced CPU scheduling priority (see setpriority(2)).}
        {S The process has asked for FIFO page replacement (VA_SEQL, from vadvise(2),  for example, a large image processing program using virtual memory to sequentially address voluminous data).}
        {s The process is a session leader.}
        {V The process is suspended during a vfork.}
        {W The process is swapped out.}
        {X The process is being traced or debugged.}
    }

    # defaults
    variable confirm_signals 1
    variable list_which_signals $common_ps_keywords

    variable sortOption ""

    # The default update time of display is 10 seconds
    # You can change it in the configure menu.
    constant MIN_UPDATE_PERIOD 2000
    variable updatePeriod 10000

    # The default command line args to "ps"
    constant DEFAULT_PS_ARGS  "auxww"

    variable pid_column 0
}


################################################################


# add one menu entry for each signal
proc tkps::addItems {menu items} {
    foreach entry $items {
        set signame [lindex $entry 0]
        $menu add command -label $entry -command [list sendSignal $signame]
    }
}


# split string into specified number of parts into list
proc tkps::stringToList {str parts} {
    set r [list]
    for {set i 0} {$i <= $parts-2} {incr i} {
        lappend r [lindex $str $i]
    }
    lappend r [join [lrange $str $parts-1 end] { }]
    return $r
}

# This runs ps and gets the results into a list of entries.
# FILTER is a variable used to filter the results, a la grep.
proc tkps::getUnixProcs {} {
    variable ps_args
    variable sortOption
    variable greppat

    # The PID column is the column which has the pid numbers in it.
    # This can change depending on the options passed to 'ps'.
    variable pid_column

    # save the old list scroll value
    set oldyview [[w pstable] nearest 0]
    set oldsize [[w pstable] size]

    # Open a pipe to the "ps" program, with some args.
    set unix_procs_fd  [open "|ps $ps_args $sortOption"]

    # Get the column headers, from the first line of output from ps.
    set header [gets $unix_procs_fd]

    set ps_columns $header
    set columns_ [concat {*}[lmap c $ps_columns {list 0 $c}]]
    [w pstable] configure -columns $columns_

    set pid_column [lsearch $ps_columns "PID"]
    if { $pid_column < 0 } {
        puts "Couldn't locate the PID column in the output from 'ps' \
                so I can't send a signal to a process:"
        puts $header
        exit 1
    }

    # Clear the list items.
    [w pstable] delete 0 end
    # Fill in listbox with process entries from 'ps' command output.
    while { [set i [gets $unix_procs_fd]] != {}  }  {
        if [regexp $greppat $i] {
            [w pstable] insert end [stringToList $i [llength $ps_columns]]
        }
    }

    close $unix_procs_fd

    # if the list has not changed size much, try to preserve viewpoint
    if {abs([[w pstable] size] - $oldsize) < 2} {
        [w pstable] yview $oldyview
    }
    focus [w pstable]
}

proc tkps::updateUnixProcs {} {
    getUnixProcs
}

################################################################
#
# Dialog box for confirmation of kill command
#
# Returns 1 if proceed, 0 if cancel
#

proc tkps::confirmDialog {signame pids} {
    set dialog [widget::dialog [w app].sendSignalConfirm \
                    -modal local \
                    -separator 1 \
                    -type okcancel \
                    -parent [w app] \
                    -padding 10 \
                    -title "Confirm KILL command" \
                   ]
    ttk::frame $dialog.f
    label $dialog.f.header -text "Send $signame to processes $pids ?"

    pack $dialog.f.header -side top -fill x

    $dialog setwidget $dialog.f

    set r [$dialog display]
    destroy $dialog

    return [expr {$r eq "ok"}]
}


################################################################
proc tkps::msgDialog {msg} {
    widget::dialog [w message [w app].message]\
        -modal local \
        -title Help \
        -parent [w app] \
        -separator 1 \
        -type ok \
        -padding 10
    set f [[w message] getframe]

    message $f.msg -text $msg -aspect 200

    pack $f.msg -side top -fill both

    [w message] display
    destroy [w message]
}


proc tkps::showHelp {} {
    msgDialog {This program will send a signal to the selected process. There \
are several equivalent ways to choose a signal to send. \

First, select a process from the list below, then select a signal to send to \
it, either using a button on the bottom of the window, or from one of the \
signal menus. The commonly used signals have their own buttons along the \
bottom of the window.

It can be used shotcut key, `k' as KILL, `n' as INT, `q' as QUIT, \
`i' as IOT, `t' as TERM, `p' as STOP, `h' as HUP signal.

The signal menus contain the following (redundant) sets of signals:
 Common_Signals contains commonly used signals.
 POSIX_Signals contains POSIX standard signals.
 All_signals contains all signals available.

The "Filter" text entry field is essentially equivalent to "ps auxww | grep foo" for some value of foo.

The "Find" entry box lets you select the first process matching the entry foo.

The Options menu contains some configuration settings.
 "Confirm"  will pop up a dialog before executing a kill command.
 "List Common Process Info": double click on process pops up dialog of common useful process info.
 "List ALL Process Info": double click on process pops up dialog of ALL process info available through ps.
 "Set Update Period" adjusts the time between updating the display (and running "ps" again, which is expensive for some reason.
 "Set Command Line Args" sets the option string which is sent to ps. It defaults to "auxww" }

}

proc tkps::showAbout {} {
    msgDialog [format {
The tkps browser was written by
Ed Petron (epetron@leba.net)
Kidong Lee (kidong@shinbiro.com)

Originally written by Henry Minsky (hqm@ai.mit.edu)

This is Version %s, August 2001

Terms of the GNU Public License apply.
} $[namespace current]::VERSION]
}


################################################################
# This ought to be a generic program to change a variable's value
proc tkps::changeUpdatePeriod {} {
    variable updatePeriod MIN_UPDATE_PERIOD update_time
    variable prompt

    set prev_update_time $updatePeriod
    set update_time $updatePeriod

    # create top level window
    widget::dialog [w change [w app].change] \
        -title "Set update period" \
        -parent [w app] \
        -type okcancel \
        -separator 1 \
        -modal local \
        -padding 10

    set f [[w change] getframe]

    ttk::label $f.updateLabel -text "Update period (ms):"
    ttk::entry [w update_period $f.update] -justify right \
        -textvariable [namespace current]::update_time

    pack $f.updateLabel -side left
    pack [w update_period] -side right

    set button [[w change] display]
    destroy [w change]

    # Don't let the updates go too fast.
    if {$button eq "ok"} {
        if {![string is integer -strict $update_time] || $update_time < $MIN_UPDATE_PERIOD} {
            set updatePeriod $MIN_UPDATE_PERIOD
        } else {
            set updatePeriod $update_time
        }
    } else {
        set updatePeriod $prev_update_time
    }
}

################################################################
# Dialog to change args to ps. This should call a dialog subroutine.
#
proc tkps::changePsArgs {} {
    variable ps_args args DEFAULT_PS_ARGS
    variable prompt

    widget::dialog [w args [w app].args] \
        -parent [w app] \
        -modal local \
        -title "Set command line args" \
        -type okcancel \
        -padding 10 \
        -separator 1

    set args $ps_args
    set prev_ps_args $ps_args

    set f [[w args] getframe]

    ttk::label $f.argsLabel -text {Command line args for "ps": }
    ttk::entry $f.args -textvariable [namespace current]::args

    pack $f.argsLabel -side left
    pack $f.args -side right

    set button [[w args] display]
    destroy [w args]

    # Don't let the updates go too fast.
    if {$button eq "ok"} {
        if {$args != ""} {
            set ps_args $args
        } else {
            set ps_args $DEFAULT_PS_ARGS
        }
    } else {
        set ps_args $prev_ps_args
    }

    updateUnixProcs
}

# table - list of lists (table of data)
# align - list of align instruction: left or right
# separator - between columns
# rowPrefix - string before first column
# rowSuffix - string after last column
proc tkps::tableToText {table aligns {separator " "} {rowPrefix ""} {rowSuffix ""}} {
    set row2lengths [lambda {row} {lmap val $row {string length $val}}]
    set selectMax [lambda {l1 l2} {lmap a $l1 b $l2 {expr {max($a, $b)}}}]
    set sizes [{*}$row2lengths [lindex $table 0]]

    foreach row [lrange $table 1 end] {
        set lengths [{*}$row2lengths $row]
        set sizes [{*}$selectMax $sizes $lengths]
    }

    set formats [list]
    foreach size $sizes align $aligns {
        switch -- $align {
            left {
                lappend formats %-${size}s
            }
            right {
                lappend formats %${size}s
            }
            default {
                error "invalid align \"$align\": should be left or right"
            }
        }
    }
    set rowFormat "$rowPrefix[join $formats $separator]$rowSuffix"

    return [join [lmap row $table { format $rowFormat {*}$row }] \n]
}


################################################################
# Routines to display a popup text widget with detailed info on a process


proc tkps::fillInfoWindow {widget pid} {
    set family TkFixedFont
    $widget configure -font $family

    $widget tag delete {*}[$widget tag names]
    $widget tag configure header -font "$family -18 bold" -lmargin1 30
    $widget tag configure child -foreground blue

    if {[catch {set status [split [fileutil::cat [file join /proc $pid status]] \n]} errorMessage]} {
        return
    }
    set status [split [fileutil::cat [file join /proc $pid status]] \n]
    set pidInfo [lrange $status 0 2]

    $widget configure -state normal
    $widget delete 1.0 end

    set procTable [list [list PID: $pid]]
    foreach l $pidInfo {
        set value [lassign $l key]
        lappend procTable [list $key $value]
    }

    lappend procTable {{} {}}

    set proc_fd [ open "|ps -p $pid -u" ]
    gets $proc_fd ps_header
    gets $proc_fd ps_line
    close $proc_fd
    # create dictionary from headers and values
    set procState [dict create {*}[concat {*}[lmap k $ps_header v $ps_line {list $k $v}]]]

    lappend procTable \
        [list User: [dict get $procState USER]] \
        [list CPU%: [dict get $procState %CPU]] \
        [list MEM%: [dict get $procState %MEM]]

    $widget insert end [tableToText $procTable {left left}]\n\n

    # replace zero character with space
    $widget insert end "Command line" header \
        \n[string map [list \u0000 { }] [fileutil::cat [file join /proc $pid cmdline]]]\n\n

    set tasks [glob -nocomplain -type d -tails -directory [file join /proc $pid task] {[0-9]*}]
    if {[llength $tasks] > 0} {
        $widget insert end "Tasks" header \n
        foreach task $tasks {
            $widget insert end "task " {} $task task " children: " {}
            foreach child [fileutil::cat [file join /proc $pid task $task children]] {
                $widget tag bind child-$child <Any-Enter> "$widget tag configure child-$child -foreground red"
                $widget tag bind child-$child <Any-Leave> "$widget tag configure child-$child -foreground blue"
                $widget tag bind child-$child <1> [list [namespace current]::fillInfoWindow $widget $child]

                $widget insert end $child [list child child-$child] " "
            }
            $widget insert end "\n"
        }
    }
    $widget insert end \n

    $widget insert end "Memory usage" header \n
    $widget insert end [tableToText [lsearch -all -inline -regexp $status {^Vm}] {left right left}]

    $widget configure -state disabled
}


################################################################
# Finds first entry matching $findpat
#
# Also scrolls the display to make the item visible if it is not already.

proc tkps::findUnixProc {findpat} {
    set entries [[w pstable] size]
    for {set i 0} {$i < $entries} {incr i} {
        if [regexp $findpat [[w pstable] get $i]] {
            [w pstable] yview $i
            [w pstable] select set $i
        } else {
            [w pstable] select clear $i
        }
    }
}


# Send signal looks at the currently selected entries in the listbox
# and sends the signal to all of them.
proc tkps::sendSignal {signal} {
    variable confirm_signals
    set pids [selectedProcesses]
    set proceed 1
    if {$pids != {}} {
        if {$confirm_signals} {
            set proceed [confirmDialog $signal $pids]
        }

        if {$proceed} {
            eval exec [format "kill -%s" $signal] $pids
        }

        updateUnixProcs
    }
}


# get the selected entries from the listbox and extract
# the pid fields from each selection
proc tkps::selectedProcesses {} {
    variable pid_column

    return [lmap i [[w pstable] curselection] {
        lindex [[w pstable] get $i] $pid_column
    }]
}


# The loop running in the background.
# We want to make sure that we don't update if there is
# a current selection in the window.
proc tkps::updateLoop {} {
    variable updatePeriod

    if {[[w pstable] curselection] == {}} {
        updateUnixProcs
    }

    variable currenttime [clock format [clock seconds]]
    after $updatePeriod [namespace current]::updateLoop
}


proc tkps::showProcessDescription {pid} {
    pack [w description] -side bottom -fill both
    fillInfoWindow [w description].process.text $pid
}

proc tkps::hideProcessDescription {} {
    pack forget [w description]
}

proc tkps::init {} {
    set systemRCFile "/etc/${tkps::APPLICATION}rc"
    set userRCFile ""
    switch $::tcl_platform(platform) {
        "unix" {
            if {[info exists ::env(DOTDIR)]} {
                set userRCFile [file join $::env(DOTDIR) .${tkps::APPLICATION}rc]
            } else {
                set userRCFile [file join $::env(HOME) .${tkps::APPLICATION}rc]
            }
        }
        default {
            error "System $::tcl_platform(platform) is not supported."
        }
    }

    ################################################################
    # Default settings

    option add *Menubutton.font fixed startupFile
    option add *Menu.font fixed startupFile
    option add *Button.font fixed startupFile
    option add *list.font fixed
    option add *header.font fixed
    option add *process.text.font fixed startupFile

    option add [string totitle $tkps::APPLICATION].title "tkps - Process Manager" startupFile

    catch {option read $systemRCFile startupFile}
    catch {option read $userRCFile userDefault}
}


proc tkps::start {psArgs} {
    variable common_ps_keywords
    variable ALL_ps_keywords
    variable posix_sigs
    variable ps_args
    variable DEFAULT_PS_ARGS
    variable pid_column

    init

    set ns [namespace current]

    tkapp [w app .tkps] \
        -name [option get . title Title] \
        -application TkPS \
        -defaultmenu 1 \
        -aboutcommand ${ns}::showAbout

    ################################################################
    # Define menu bar items
    #

    # menu bar widget
    set m [[w app] getmenu]

    $m insert 2 cascade -label Options -menu $m.options -underline 0
    $m insert 3 cascade -label "Signal" -menu $m.signals -underline 0

    menu $m.options -tearoff 0
    menu $m.options.sortoptions
    $m.options.sortoptions add command \
        -label "USER" \
        -command [list namespace eval $ns {set sortOption "--sort user"}]
    $m.options.sortoptions add command \
        -label "UID" \
        -command [list namespace eval $ns {set sortOption "--sort uid"}]
    $m.options.sortoptions add command \
        -label "%CPU" \
        -command [list namespace eval $ns {set sortOption "--sort -pcpu"}]
    $m.options.sortoptions add command \
        -label "PID" \
        -command [list namespace eval $ns {set sortOption "-Op"}]

    $m.options add checkbutton \
        -label "Confirm Signals" \
        -variable ${ns}::confirm_signals
    $m.options add separator
    $m.options add radiobutton \
        -label "List Common Process Info" \
        -variable ${ns}::list_which_signals \
        -value $common_ps_keywords
    $m.options add radiobutton \
        -label "List ALL Process Info" \
        -variable ${ns}::list_which_signals \
        -value $ALL_ps_keywords
    $m.options add cascade \
        -label "Sort Processes by..." \
        -menu $m.options.sortoptions
    $m.options add separator
    $m.options add command \
        -label "Set Update Period..." \
        -command changeUpdatePeriod
    $m.options add command \
        -label "Set 'ps' Command Line Args..." \
        -command changePsArgs

    menu $m.signals -tearoff 0

    menu $m.signals.com_signals
    $m.signals add cascade -label "Common Signals" -menu $m.signals.com_signals
    addItems $m.signals.com_signals $tkps::SIGNALS(COMMON)

    menu $m.signals.all_signals
    $m.signals add cascade -label "POSIX Signals" -menu $m.signals.posix_signals
    addItems $m.signals.all_signals $tkps::SIGNALS(ALL)

    menu $m.signals.posix_signals
    $m.signals add cascade -label "All Signals" -menu $m.signals.all_signals
    addItems $m.signals.posix_signals $tkps::SIGNALS(POSIX)

    $m.help add command -label "Help" -command ${ns}::showHelp

    ################################################################
    #
    # Create an entry field for restricting the visible entries.
    # This simulates the "ps auxww | grep foo" idiom.
    #

    [w app] toolbar add label findlabel -text "Find:"
    [w app] toolbar add \
        [entry [w findentry [[w app] gettoolbar].findentry] -textvariable ${ns}::findpat]
    [w app] toolbar add label greplabel -text "Filter:"
    [w app] toolbar add \
        [entry [w filterentry [[w app] gettoolbar].filterentry] \
             -textvariable ${ns}::greppat]
    [w app] toolbar add \
        [ttk::button [[w app] gettoolbar].update \
             -text "Update" \
             -command getUnixProcs]

    bind [w filterentry] <Return> ${ns}::updateUnixProcs
    bind [w findentry] <Return> [list namespace eval $ns {findUnixProc $findpat}]
    bind [w app] <Escape> ${ns}::hideProcessDescription

    [w app] toolbar add space

    set mf [[w app] getframe]

    set sw [widget::scrolledwindow $mf.pstable]

    tablelist::tablelist [w pstable $sw.table]

    $sw setwidget [w pstable]
    pack $sw -fill both -expand true

    ttk::frame [w signals $mf.signals]
    # Make a button bar for the common signals
    foreach {signal key} {KILL k INT n QUIT q IOT i TERM t STOP p HUP h} {
        set name [string tolower $signal]
        ttk::button [w signals].[string tolower $signal] \
            -text $signal \
            -underline 0 \
            -command [list sendSignal $signal]
        pack [w signals].[string tolower $signal] -side left -padx 5 -pady 5
        bind [[w pstable] bodytag] <[string toupper $key]> [list sendSignal $signal]
    }
    pack [w signals] -side bottom -fill x

    ttk::labelframe [w description $mf.description]
    ttk::frame [w description].title
    ttk::label [w description].title.ldescription -text "Info: "
    button [w description].title.hide \
        -text "\u274c" -padx 3 -pady 0 -bg red -fg white \
        -command hideProcessDescription
    pack [w description].title.hide -side right -fill none -padx 3 -pady 3
    pack [w description].title.ldescription -side right

    [w description] configure -labelwidget [w description].title

    set process [ttk::frame [w description].process]
    text $process.text -yscrollcommand [list $process.scroll set] -padx 10 -pady 10
    ttk::scrollbar $process.scroll -command [list $process.text yview]

    grid $process.text -sticky nsew
    grid $process.scroll -row 0 -column 1 -sticky ns
    grid columnconfigure $process 0 -weight 1
    grid rowconfigure $process 0 -weight 1

    pack $process -side left -fill both -expand true

    [w app] statusbar add label -- -textvariable [namespace current]::currenttime

    ################################################################
    # Set up args to 'ps'.
    # We either got args from the command line, or we default
    # to auxww

    if {[llength $psArgs] > 0} {
        set ps_args [lindex $psArgs 0]
    } else {
        set ps_args $DEFAULT_PS_ARGS
    }

    # Set up bindings for the browser.

    bind [w pstable]  <Button-1> {focus [w pstable]}
    bind [[w pstable] bodytag] <Double-Button-1> [list namespace eval $ns {
        set oldconfirm $confirm_signals
        set confirm_signals 1

        showProcessDescription [lindex [[w pstable] get [lindex [[w pstable] curselection] 0]] $pid_column]
        set confirm_signals $oldconfirm
    }]
    bind [[w pstable] bodytag] <Return> [list namespace eval $ns {
        set oldconfirm $confirm_signals
        set confirm_signals 1
        showProcessDescription [lindex [[w pstable] get [lindex [[w pstable] curselection] 0]] $pid_column]
        set confirm_signals $oldconfirm
    }]

    bind [w app] <F1> ${ns}::showHelp

    wm withdraw .

    updateLoop
}
