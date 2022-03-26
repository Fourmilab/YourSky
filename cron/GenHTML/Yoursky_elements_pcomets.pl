#! /usr/bin/perl

    #   Parse JPL periodic comet database, as downloaded
    #   from:
    #
    #       https://ssd.jpl.nasa.gov/dat/ELEMENTS.COMET
    #
    #   We exclude destroyed comets, ignore all but the latest
    #   elements in the case of elements for multiple perihelion
    #   passages, and process only numbered comets with well-known
    #   orbital elements.  These are output as links in an HTML
    #   document for inclusion in the Your Sky Object Catalogue.

    use strict;
    use warnings;

    print << "EOD";
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<title>Your Sky Object Catalogue: Periodic Comets</title>
<meta name="robots" content="noindex,nofollow" />
<link rel="stylesheet" href="styles/objcat.css" type="text/css" />
<link rel="stylesheet" href="styles/ast_comet.css" type="text/css" />
</head>
<body>
<h1>
<img src="../help/images/ystitlew.png" width="511" height="142" alt="Your Sky" />
<br />
Object Catalogue: Periodic Comets
</h1>
<p style="text-align: center;">
Click object name to display position in the <cite>Your Sky</cite>
Virtual telescope.<br />
Click &#8858; symbol to plot orbit in <cite>Solar System Live</cite>.
</p>
<hr />
<p />
EOD

    #   Read the comet elements database and extract items of interest

    my $l;

    my @comets;
    
    my $inhdr = 1;

    while ($l = <>) {
    
        if ($inhdr && ($l !~ m:[A-Z]/[\w\-]:)) {
#print("Ditch: $l");
            next;
        }
        $inhdr = 0;

        #   Ignore destroyed comets
        if ($l =~ m:[^\-]D(\-[A-Z])?/[\w\-]:) {
#print("Dead: $l");
            next;
        }

        #   Strip trailing space
        $l =~ s/\s+$//;

        if (!($l =~ m:(^[A-Za-z0-9/\-'\(\)\s]+)\s+[\-\d]+\s+\d+\.\d+\s:)) {
die("No parse: $l");
            next;
        }

        #   The JPL database contains entries for multiple perihelion
        #   passes for periodic comets which have been observed at
        #   more than one perihelion.  These always occur from oldest
        #   to newest.  For each entry, check if the previous entry
        #   was for the same comet and, if so, replace it rather than
        #   appending it to the @comets array.

        if ((scalar(@comets) > 0) &&
            (substr($comets[$#comets], 0, 52) eq
             substr($l, 0, 52))) {
#print("Dup: $l\n");
            pop(@comets);
        }
        
        #   Ignore any comets which have not been assigned numbers.
        #   Non-numbered comets are parabolic, hyperbolic, or do not
        #   have well-established orbits.
        
        if ($l =~ m/^\s\s\s/) {
#print("Skip: $l\n");
            next;
        }
        
        push(@comets, $l);
#print("Add: $l\n");
    }
    
    #   The periodic comets have been loaded into @comets.  Now
    #   sort them into the order in which we wish to display them.

    @comets = sort { numlast(excom($a)) cmp numlast(excom($b)) } @comets;

#print(STDERR scalar(@comets), " comets.\n");
#use Data::Dumper;
#print(STDERR Dumper(\@comets));

    #   Generate table of comets for body of document

    my $lex = 'FIRST';

    while ($l = shift(@comets)) {
#print(STDERR "($l)\n");
        if (!($l =~ m:^([\d\s]{3})(.{40})\s+\-?\d+\s+(\d+\.\d+)\s+(\d+\.\d+)\s+(\d+\.\d+)\s+\d+\.\d+\s+\d+\.\d+\s+(\-?\d+\.\d+)\s+:)) {

die("No parse: $l\n");
        }

        my ($cnum, $cname, $Eq, $Ee, $Ei, $ETp) = ($1, $2, $3, $4, $5, $6);
        $cnum =~ s/\s+$//;
        $cname =~ s/\s+$//;

#print(STDERR "Comet ($cnum $cname)  q = $Eq  e = $Ee  i = $Ei  Tp = $ETp\n");

        #   Extract index heading from canonical name
        my $cex = $cname;
        $cex =~ m/^..(.)/;
        $cex = uc($1);

        my $canon = "$cnum$cname";

        my $els = $canon . ' ' . substr($l, 46);
        $els =~ s/\s+/\+/g;

        #   Escape any ampersands just in case
        $cnum =~ s/&/%26/g;
        $cname =~ s/&/%26/g;
        $els =~ s/&/%26/g;
        
        #   If first-letter heading has changed, generate
        #   table header.

        if ($cex ne $lex) {
            if ($lex ne ' ') {
                print << "EOD";
</table>
EOD
            }
            $lex = $cex;
            print << "EOD";
<p />
<table class="e">
<tr>
<td class="h" colspan="5">$cex</td>
</tr>
<tr>
<th class="s">&nbsp;</th>
<th colspan="2" align="center">Perihelion</th>
</tr>
<tr>
<th class="n">Name</th>
<th>Date</th>
<th class="r">AU</th>
<th>Eccentricity</th>
<th>Inclination</th>
</tr>
EOD
        }

#       print("<a href=\"/cgi-bin/Yourtel?elements=$els" .
#             "&amp;aim=10&amp;z=1&amp;edump=-xe\">$cname</a><br />\n");
#print("&nbsp; &nbsp; " . excom($l) . "<br />\n");

        my ($fEq, $fEe, $fEi) = (
                                sprintf("%.2f", $Eq),
                                sprintf("%.2f", $Ee),
                                sprintf("%.1f", $Ei),
                               );
        $ETp =~ m/(\-?\d+)(\d\d)(\d\d)\.\d/;
        my $fETp = sprintf("%d-%02d-%02d", $1, $2, $3);

        #   For the Solar System Live link to plot the orbit, we
        #   want to show the inner system if the orbit is entirely
        #   within it, otherwise the complete system.  Cometary
        #   orbital elements do not explicitly contain the
        #   semi-major axis, so we have to derive it from the
        #   perihelion distance ($Eq) and the eccentricity ($Ee),
        #   which we've parsed above.
        my $Ea = 9999999;
        if ($Ee < 1) {
            $Ea = $Eq / (1.0 - $Ee);
        }
#print("<!-- Semi-major axis: $Ea -->\n");
        my $SolarScale = ($Ea <= 5.5) ? "i" : "f";

        print << "EOD";
<tr>
<td class="n">
    <a href="/cgi-bin/Yourtel?elements=$els&amp;aim=10&amp;z=1&amp;edump=-xe">$canon</a>
    &nbsp;
    <a href="/cgi-bin/Solar?sys=-S$SolarScale&amp;imgsize=640&amp;edump=-xe&amp;elements=$els">&#8858;</a>
</td>
<td>$fETp</td>
<td>&nbsp;$fEq</td>
<td>$fEe</td>
<td>$fEi</td>
</tr>
EOD

    }

    #   Generate end of document

    my (undef, $tm, $th, $dd, $dm, $dy) = gmtime(time());
    my $udate = sprintf("%4d-%02d-%02d %02d:%02d UTC", $dy + 1900, $dm + 1, $dd,
        $th, $tm);

    print << "EOD";
</table>
<p />
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
    <a href="http://validator.w3.org/check?uri=http://www.fourmilab.ch/yoursky/catalogues/periodic_comets.html"
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

    #   Extract canonical name of comet for sorting

    sub excom {
        my ($s) = @_;

        $s = substr($s, 5, 32);
        $s =~ s/\s+$//;
        return uc($s);
    }

    #   Transform text to sort numbers after letters

    sub numlast {
        my ($s) = @_;

        $s =~ tr/0-9/a-j/;
        return $s;
    }
