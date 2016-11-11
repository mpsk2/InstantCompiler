Instant
=======

Table of Content
----------------

1. About
2. Authors
3. Building

About
-----

Simple compiler to JVM and LLVM of Instant language. That is first task for *MRJP* - *Metody realizacji języków programowania* (ang. *Compiler constuction*) at University of Warsaw, Mathematics, Informatics and Mechanics Department.
Here is PolishDescription_ and EnglishDescription_.
That solution is for max grade, but does not work with llvm>=3.7 (change in load instruction semantics).
If you think I should change anything, including LICENSE (Jasimn is BSD license), please inform me.

.. _PolishDescription: http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2016/instant.html
.. _EnglishDescription: http://www.mimuw.edu.pl/~ben/Zajecia/Mrj2015/instant-en.html

Authors
-------

The only author is *Michał Piotr Stankiewicz*.

Building
--------

You need to have installed GHC, Cabal, Make

.. code-block:: sh

    sudo apt-get update
    sudo apt-get install ghc cabal-install make
    cd /path/to/project
    cabal install alex happy
    make

