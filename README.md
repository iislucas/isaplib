## About IsapLib

isaplib is Standard ML (SML) library built on the code-base of
[Isabelle](http://www.cl.cam.ac.uk/research/hvg/Isabelle/).

It currently uses some pretty printing code specific to
[PolyML](http://www.polyml.org/), but could fairly easily be ported to other SML
implementations. This library was used by
[Quantomatic](http://dream.inf.ed.ac.uk/projects/quantomatic/) and
[IsaPlanner](http://dream.inf.ed.ac.uk/projects/isaplanner/) related projects.

## Building on top of Isabelle

This is for when you have Isabelle installed (see
http://www.cl.cam.ac.uk/research/hvg/isabelle/index.html) and you want to load
isaplib's additional libraries.

 1. Place the isaplib directory in Isabelle's contrib directory, or just clone
    the isaplib repository at that location with the command:
    ```
    git clone git@github.com:iislucas/isaplib.git
    ```

 2. You can then have new theories inherrit from:
    ```
    Develop can then inherrit from "~~/contrib/isaplib/pure_isaplib"
    ```

## LICENCE and DISCLAIMER

This software is under a combination of the Lesser GNU General Public License
(GPL) and a similar license from the University of Cambridge. This software is
provided "as is": you use the software at your own risk and we make no
warranties of any sort. See the LICENCE file for more information on the licence
and disclaimer.
