# Update Asteroid and Comet Orbit Elements

The orbital elements of asteroids and comets which can be tracked are
retrieved from public databases maintained by the Jet Propulsion
Laboratory (JPL).  These are “osculating” orbital elements and only
valid for a limited period of time and are updated based upon new
observations and discoveries.

The object catalogues used by Your Sky are created automatically from
JPL databases by utilities in this directory.  The update process is
run a Fourmilab by the `UpdateYourskyJPLOrbitalElements` job, run
from the Crontab mechanism once a week.  The Fortran programs in the
`Translate_JPL` directory extract the information from the files
downloaded from JPL and the Perl programs in the `GenHTML` directory
generate the HTML indices from that information.

It is in the nature of resources on the Internet that they change from
time to time, breaking processes that access them. Over the years,
these tools have been modified from time to time as a result of these
changes.  It is probable such changes will be required in the future.
