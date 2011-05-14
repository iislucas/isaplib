## About 

isaplib is Standard ML (SML) library, based on the code-based of [Isabelle](http://www.cl.cam.ac.uk/research/hvg/Isabelle/), with some of the Isabelle-specifics removed, and some additional libraries from [IsaPlanner](http://dream.inf.ed.ac.uk/projects/isaplanner/) added. It currently uses some pretty printing code specific to [PolyML](http://www.polyml.org/), but could fairly easily be ported to other SML implementations. This library is used by [Quantomatic](http://dream.inf.ed.ac.uk/projects/quantomatic/) and [IsaPlanner](http://dream.inf.ed.ac.uk/projects/isaplanner/). 

## Building

The `Makefile` assumes that [PolyML](http://www.polyml.org/) is installed and in the path. Running make will create a PolyML heap image called `all.polyml-heap` in the `heaps` subdirectory. 

## LICENCE and DISCLAIMER

This software is under a combination of the Lesser GNU General Public License (GPL) and a similar license from the University of Cambridge. This software is provided "as is": you use the software at your own risk and we make no warranties of any sort. See the file LICENCE.txt for more information on the licence and disclaimer.
