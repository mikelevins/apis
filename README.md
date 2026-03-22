# net.bardcode.apis

A worker bee for application hives

Apis is a Common Lisp library that supports shared-nothing concurrency and
distributed processing.

Its current version supports local message passing. Past versions, which are still accessible in the git history, supported distributed processing with message delivery to remote processes and hosts. Apis is in active development and I expect to restore those features in due course.

## next-available-runtime

This branch explores a concurrency runtime that separates the concurrent threading model from the actors to give a model that is simpler than the original model, and that can accommodate higher levels of concurrency than one thread per actor.
