# Your Sky — Custom Star Maps

[*Your Sky*](https://www.fourmilab.ch/yoursky/)
is a Web-based interactive resource available on the Fourmilab.ch site
since 1998 which makes custom maps of the sky for any location on Earth
and any date from 4713 B.C. into the distant future.  Maps can include
stars as faint as magnitude 6.5, constellation names, outlines, and
boundaries, the Moon and planets, deep sky objects from a database of
more than 500, and a comet or asteroid whose position is calculated
from its orbital elements.  A variety of display options allow
customising the map for its intended use.  All-sky maps or horizon
views can be generated, as well as images from a “virtual telescope”
aimed anywhere in the sky.

This repository maintains the master copies of the *Your Sky* Web
pages, the Common Gateway Interface (CGI) programs (written in C) which
process user requests and generate the star map images, and utilities
which download and update databases of comets and asteroids from
publicly-available sources.

## Structure of the repository

This repository is organised into the following directories.

* **webtree**: Replica of the Web tree from the Fourmilab site
containing all of the HTML documents, images, and downloads.  These
pages contain relative references to style sheets, icons, and other
resources on the Fourmilab Web site and will not display properly
without modification in other environments.

* **src**: Source code for the Common Gateway Interface (CGI) programs
that generate the sky maps and response pages for requests made
from the Web pages.

* **cgi**: Resources used by the CGI programs built in the **src**
directory including icons and fixed HTML epilogues for results returned.

* **cron**: Utilities to download current databases of comets and
asteroids from the Jet Propulsion Laboratory and update the tables used
by *Your Sky* to track and plot the position of objects on sky maps.

## Web resources

* [*Your Sky*](https://www.fourmilab.ch/yoursky/)
