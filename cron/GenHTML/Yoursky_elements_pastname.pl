#! /usr/bin/perl

    #   Parse JPL asteroid database (legacy format, converted
    #   from dastcom5 by Translate_JPL/trans_ast) and generate
    #   the Your Sky object catalogue Web documents for named
    #   asteroids, one document per initial letter.

    use strict;
    use warnings;

    my $l;

    my $prefix = '';

    my @asteroids;
    my $ofname;

    my (undef, $tm, $th, $dd, $dm, $dy) = gmtime(time());
    my $udate = sprintf("%4d-%02d-%02d %02d:%02d UTC", $dy + 1900, $dm + 1, $dd,
        $th, $tm);

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

        #   Ignore asteroids which haven't been assigned a name
        if ($2 =~ m/^\d/) {
           next;
        }

        push(@asteroids, $l);
    }

    @asteroids = sort { numlast(excom($a)) cmp numlast(excom($b)) } @asteroids;

    my ($pex, $lex) = (' ', ' ');

    while ($l = shift(@asteroids)) {
        if (!($l =~ m:(^\d+)\s(.{18})\d\d\d\d\d\s+(\d+\.\d+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s+\d+\.\d+\s+\d+\.\d+\s+\d+\.\d+\s+(\-?\d+\.\d+)\s+:)) {
die("No parse (2): $l\n");
        }

        my ($cnum, $cname, $Ea, $Ee, $Ei, $EH) = ($1, $2, $3, $4, $5, $6);
        $cname =~ s/\s+$//;

        #   If asteroid name field is blank, synthesise a name from the
        #   number.

        if ($cname eq '') {
            $cname = "A$cnum";
#print(STDERR "No name for ($cname)\n");
            next;
        }

#print(STDERR "Asteroid ($cnum $cname)\n");

        my $cex = $cname;
        $cex =~ m/^\W*(.)/;
        $cex = $1;
        $cex =~ tr/a-z/A-Z/;

        my $els = $l;
        $els =~ s/\s+/\+/g;

        #   Escape any ampersands just in case
        $cname =~ s/&/%26/g;
        $els =~ s/&/%26/g;

        if ($cex ne $lex) {
            if ($lex ne ' ') {
                endfile($pex, $cex);
            }
            $pex = $lex;
            $lex = $cex;

#print(STDERR "-$cex-\n");
            open(OF, ">${prefix}asteroid_names$cex.html") || die("Cannot create ${prefix}asteroid_names$cex.html");
            $ofname = "asteroid_names$cex.html";

            print OF << "EOD";
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<title>Your Sky Object Catalogue: Named Asteroids Beginning with $cex</title>
<meta name="robots" content="noindex,nofollow" />
<link rel="stylesheet" href="styles/objcat.css" type="text/css" />
<link rel="stylesheet" href="styles/ast_comet.css" type="text/css" />
</head>

<body>
<h1>
<img src="../help/images/ystitlew.png" width="511" height="142" alt="Your Sky" />
<br />
Object Catalogue:<br />
Named Asteroids Beginning with $cex
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
<th class="n">Name</th>
<th>Number</th>
<th>Semi-major<br />Axis (AU)</th>
<th>Eccentricity</th>
<th>Inclination</th>
<th>Absolute<br />Magnitude</th>
</tr>
EOD

        }

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
<td class="n">
    <a href="/cgi-bin/Yourtel?elements=$els&amp;aim=10&amp;z=1&amp;edump=-xe">$cname</a>
    &nbsp;
    <a href="/cgi-bin/Solar?sys=-S$SolarScale&amp;imgsize=640&amp;edump=-xe&amp;elements=$els">&#8858;</a>
</td>
<td>$cnum</td>
<td>$fEa</td>
<td>$fEe</td>
<td>$fEi</td>
<td>$fEH</td>
</tr>
EOD
    }

    endfile($pex, ' ');

    #   Extract canonical name of asteroid for sorting

    sub excom {
        my ($s) = @_;

        $s =~ s:^\s*\d+\s+::;
        $s =~ tr/a-z/A-Z/;
        $s =~ s/[^A-Z0-9]//g;
        return $s;
    }

    #   Transform text to sort numbers after letters

    sub numlast {
        my ($s) = @_;

        $s =~ tr/0-9/a-j/;
        return $s;
    }

    #   Close current file

    sub endfile {
        my ($b, $f) = @_;

        print OF << "EOD";
</table>

<p />
<p>
<b>
EOD

        if ($b ne ' ') {
            print(OF "<a href=\"${prefix}asteroid_names$b.html\">Names beginning with $b</a>\n");
        }
        if ($f ne ' ') {
            if ($b ne ' ') {
                print(OF "&nbsp; &nbsp; ");
            }
            print(OF "<a href=\"${prefix}asteroid_names$f.html\">Names beginning with $f</a>\n");
        }

        print OF << "EOD";
&nbsp; &nbsp; <a href="asteroid_names.html">Alphabetical Index</a>
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
