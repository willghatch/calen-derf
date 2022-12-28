calen-derf
==========

`calen-derf` started life as a class project, and quickly died thereafter.
I thought I might grow and polish it to be a useful library, but I never made time for it and likely never will.
I started it thinking I wanted a better tool for interacting with my calendar from the command line and getting calendar notifications without running a fat GUI program.
But it just wasn't a priority long after the class ended, so I never got it to a point where even I used it for anything.

It has the beginnings of a library for dealing with ics and vcf files.  
But no documentation or anything.

At the time of writing, the only useful thing it does is let you add
events to a vdir calendar, and get notifications on non-recurring events
(if you have the program `notify-send` installed, which probably only
exists for Unix OSes with an X11 display).
So there you have it.

I thought about taking this down because it is not useful now and I don't think it will ever grow to become useful.
But I decided to leave it up, as there is some small chance that someone will find something useful in the code somehow, such as if they are writing a similar library.

License
=======

GNU LGPL version 3 or any later version published by the Free Software Foundation.

