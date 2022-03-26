#! /usr/bin/perl

    #   Parse JPL asteroid database (legacy format, converted
    #   from dastcom5 by Translate_JPL/trans_ast) and generate
    #   the Your Sky object catalogue Web documents for numbered
    #   asteroids, breaking documents every 1000 asteroids,
    #   and generating an index document linking to the individual
    #   documents.


    use strict;
    use warnings;

    my $l;

    my $prefix = '';

    my ($pex, $lex) = (' ', ' ');

    my (undef, $tm, $th, $dd, $dm, $dy) = gmtime(time());
    my $udate = sprintf("%4d-%02d-%02d %02d:%02d UTC", $dy + 1900, $dm + 1, $dd,
        $th, $tm);

    my ($firstnum, $lastnum) = (0, 0);
    my $aperdoc = 1000;         # Asteroids per document
    my $nextbreak = 0;

    my $lnum = 0;
    my $ofname;

    #   Create the index file and write its header

    open(XF, ">${prefix}asteroid_numbers.html") || die("Cannot create ${prefix}asteroid_numbers.html");
    print XF << "EOD";
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<title>Your Sky Object Catalogue:  Asteroids by Number</title>
<meta name="robots" content="noindex,nofollow" />
<link rel="stylesheet" href="styles/objcat.css" type="text/css" />
<link rel="stylesheet" href="styles/ast_comet.css" type="text/css" />
</head>

<body>
<h1>
<img src="../help/images/ystitlew.png" width="511" height="142" alt="Your Sky" />
<br />
Object Catalogue: Asteroids by Number
</h1>
<hr />
<ul>
EOD

    while ($l = <>) {
        #   Ignore header lines
        if ($l !~ m:\s*\d+\s:) {
#print("Ditch: $l");
            next;
        }

        #   Strip leading and trailing space
        $l =~ s/\s+$//;
        $l =~ s/^\s+//;

        if (!($l =~ m:(^\d+)\s(.{18})\d\d\d\d\d\s+\d+\.\d+\s:)) {
die("No parse (1): $l\n");
        }
#print("Asteroid ($1) ($2)\n");

#       #   Ignore asteroids which haven't been assigned a name
#       if ($2 =~ m/^\d/) {
#           next;
#       }

        if (!($l =~ m:(^\d+)\s(.{18})\d\d\d\d\d\s+(\d+\.\d+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s+\d+\.\d+\s+\d+\.\d+\s+\d+\.\d+\s+(\-?\d+\.\d+)\s+:)) {
die("No parse (2): $l\n");
        }

        my ($cnum, $cname, $Ea, $Ee, $Ei, $EH) = ($1, $2, $3, $4, $5, $6);
        $cname =~ s/\s+$//;

        #   If asteroid name field is blank, synthesise a name from the
        #   number.

        if ($cname eq '') {
            $cname = "A$cnum";
#print(STDERR "Filled in name for ($cname)\n");
        }

        die("Out of sequence: $cnum follows $lnum") if ($cnum <= $lnum);
        if ($cnum != ($lnum + 1)) {
            print(STDERR "Gap between $lnum and $cnum\n");
        }
        $lnum = $cnum;

        my $els = $l;
        $els =~ s/\s+/\+/g;

        #   Escape any ampersands just in case
        $cname =~ s/&/%26/g;
        $els =~ s/&/%26/g;

        if ($cnum >= $nextbreak) {

            if ($nextbreak > 0) {
                endfile($nextbreak, $cnum);
            }

            $firstnum = $cnum;

            $nextbreak = $nextbreak + $aperdoc;
            my $endoc = $nextbreak - 1;

            #   Create a document for a group of 1000 asteroids

#print(STDERR "Opening ${prefix}asteroid_numbers$firstnum.html at Cnum=$cnum  Nextbreak=$nextbreak\n");
            open(OF, ">${prefix}asteroid_numbers$firstnum.html") || die("Cannot create ${prefix}asteroid_numbers$firstnum.html");
            $ofname = "asteroid_numbers$firstnum.html";

            #   Write a link to the document to the index file
            print(XF "<li><a href=\"${prefix}asteroid_numbers$firstnum.html\">$firstnum-$endoc</a></li>\n");

            #   Write the header to the asteroid list document
            print OF << "EOD";
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<title>Your Sky Object Catalogue: Asteroids $firstnum to $endoc</title>
<meta name="robots" content="noindex,nofollow" />
<link rel="stylesheet" href="styles/objcat.css" type="text/css" />
<link rel="stylesheet" href="styles/ast_comet.css" type="text/css" />
</head>

<body>
<h1>
<img src="../help/images/ystitlew.png" width="511" height="142" alt="Your Sky" />
<br />
Object Catalogue:<br />
Asteroids $firstnum to $endoc
</h1>
<p style="text-align: center;">
Click object name to display position in the <cite>Your Sky</cite>
Virtual telescope.<br />
Click &#8858; symbol to plot orbit in <cite>Solar System Live</cite>.
</p>
<hr />
<p />

<table class="e">
<tr>
<th>Number</th>
<th>Name</th>
<th>Semi-major<br />Axis (AU)</th>
<th>Eccentricity</th>
<th>Inclination</th>
<th>Absolute<br />Magnitude</th>
</tr>
EOD

        }

        $lastnum = $cnum;

#       print(OF "<a href=\"/cgi-bin/Yourtel?elements=$els" .
#             "&amp;aim=10&amp;z=1&amp;edump=-xe\">$cname &nbsp; &nbsp; ($cnum)</a><br />\n");
#print("&nbsp; &nbsp; " . excom($l) . "<br />\n");

        my ($fEa, $fEe, $fEi, $fEH) = (
                                        sprintf("%.2f", $Ea),
                                        sprintf("%.2f", $Ee),
                                        sprintf("%.1f", $Ei),
                                        sprintf("%.2f", $EH)
                                      );

        #   Choose whether Solar System Live will show the
        #   inner or full solar system based upon the
        #   semi-major axis ($Ea) of the object's orbit.
        my $SolarScale = ($Ea <= 5.5) ? "i" : "f";

        print OF << "EOD";
<tr>
<td class="c">
    <a href="/cgi-bin/Yourtel?elements=$els&amp;aim=10&amp;z=1&amp;edump=-xe">$cnum</a>
    &nbsp;
    <a href="/cgi-bin/Solar?sys=-S$SolarScale&amp;imgsize=640&amp;edump=-xe&amp;elements=$els">&#8858;</a>
</td>
<td class="n">$cname</td>
<td>$fEa</td>
<td>$fEe</td>
<td>$fEi</td>
<td>$fEH</td>
</tr>
EOD
    }

    endfile($nextbreak, -1);

    print XF << "EOD";
</ul>
<p />
<h3><a href="asteroid_names.html">Asteroids by Name</a></h3>
<p>
<b>
<a href="../">Back to <em>Your Sky</em></a>
&nbsp; &nbsp; <a href="catalogues.html">Other Object Catalogues</a>
</b>
</p>
<p />
<hr />
<table class="right">
<tr><td class="c">
    <a href="http://validator.w3.org/check?uri=http://www.fourmilab.ch/yoursky/catalogues/asteroid_numbers.html"
       class="i"><img
       src="/images/icons/valid-xhtml10.png"
       alt="Valid XHTML 1.0" height="31" width="88"
       class="button" /></a>
</td></tr>
</table>
<address>
by <a href="/">John Walker</a><br />
Last updated: $udate
</address>
</body>
</html>
EOD
    close(XF);

    #   Close current file

    sub endfile {
        my ($l, $cn) = @_;

#print(STDERR "Closing file at Cnum=$cn  Nextbreak=$l\n");
        print OF << "EOD";
</table>

<p />
<p>
<b>
EOD

        my $b = $l - (2 * $aperdoc);
        my $be = $b + ($aperdoc - 1);
        if ($b == 0) {
            $b = 1;
            $be = $aperdoc - $b;
        }
        my $f = $l;
        my $fe = $f + ($aperdoc - 1);

        if ($b > 0) {
            print(OF "<a href=\"${prefix}asteroid_numbers$b.html\">« Asteroids $b-$be</a>\n");
        }
        if ($cn > 0) {
            if ($b > 0) {
                print(OF "&nbsp; &nbsp; ");
            }
            print(OF "<a href=\"${prefix}asteroid_numbers$f.html\">» Asteroids $f-$fe</a>\n");
        }

        print OF << "EOD";
&nbsp; &nbsp; <a href="${prefix}asteroid_numbers.html">Index by Numbers</a>
<br />
<a href="../">Back to <em>Your Sky</em></a>
&nbsp; &nbsp; <a href="catalogues.html">Other Object Catalogues</a>
</b>
</p>
<p />
<hr />
<table class="right">
<tr><td class="c">
    <a href="http://validator.w3.org/check?uri=http://www.fourmilab.ch/yoursky/catalogues/$ofname"
        class="i"><img
        src="/images/icons/valid-xhtml10.png"
        alt="Valid XHTML 1.0" height="31" width="88"
        class="button" /></a>
</td></tr>
</table>
<address>
by <a href="/">John Walker</a><br />
Last updated: $udate
</address>
</body>
</html>
EOD

        close(OF);
    }
