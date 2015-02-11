Apis: a worker bee for building application hives

Apis is a set of libraries that support building applications using
multiple processes, shared-nothing concurrency, and, optionally,
distributed processing.

Formerly, Apis was just intended to be a library for messaging between
Lisp processes, but it stalled when I found a different development
platform for the project I was working on that offered some
significant prdocutvity gains.

I've returned to Apis for another project, and now I find it useful to
include more application infrastructure and organize the communication
layer around local and remote IPC.

A "cocoa" subsystem is now in place that supports building a skeleton
Cocoa application on OSX. A savvy user could probably extend it into a
fully-function OSX GUI application.

I plan to add support for similar skeleton apps on Windows and Linux
soon.

For now, Apis is unapologetically dependent on Clozure Common Lisp:

  http://ccl.clozure.com/

