cl-tnetstrings
==============

Common Lisp implementation of tnetstrings

Install
--------

Until Zach deems this suitable for inclusion in quicklisp, add it as a local-project

    $ cd ~/quicklisp/local-projects
    $ git clone https://github.com/cddr/cl-tnetstrings.git
    $ $LISP
    CL-USER> (ql:quickload "cl-tnetstrings")
    CL-USER> (asdf:operate 'asdf:test-op :cl-tnetstrings)


Usage
------

    TNETSTRINGS> (dump 42)
    "2:42#"
    TNETSTRINGS> (dump 42.0)
    "4:42.0^"
    TNETSTRINGS> (dump "forty two")
    "9:forty two,"
    TNETSTRINGS> (dump (dict :a 1 :b 2))
    "16:1:A,1:1#1:B,1:2#}"
    TNETSTRINGS> (dump t)
    "4:true!"
    TNETSTRINGS> (dump nil)
    "0:]"

If any of the strings produced above are passed to parse, the result is equalp to the original object.


Bugs
----

If you find one, please raise an issue in the github project