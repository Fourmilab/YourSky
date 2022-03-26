    require "splitfields.pl";

    open(IF, "<$filename.csv");
    open(OF, ">/ftp/yoursky/catalogues/$ofile.html");

    $first = 1;

    if (!(defined $zset)) {
        $zset = 1;
    }

    while ($l = <IF>) {
        $l =~ s/\s+$//;
        if (!($l =~ m/\s*\;/)) {
            @fields = &splitfields($l);
#           print("$l\n");
#           $w = join(" : ", @fields);
#           print("--> $w\n");
            if ($first) {
                $first = 0;
                print OF <<"EOH"
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">
<html version="-//W3C//DTD HTML 3.2 Final//EN">
<head>
<title>Your Sky Object Catalogue: $what</title>
</head>
<body bgcolor="#C0C0C0">
<h1>
<center>
<img src="../help/images/ystitle.jpg" width=506 height=120 alt="Your Sky">
<br>
Object Catalogue: $what
</center>
</h1>
<hr>
<p>
<center>
<table border cellpadding=3>
<tr>
EOH
                ;

                #   Create table column titles

                print(OF "<th>");
                print(OF join(" <th>", @fields) . " \n");
                $nfields = $#fields;
                for ($i = 0; $i <= $#fields; $i++) {
                    if ($fields[$i] =~ m/Right ascension/i) {
                        $ra = $i;
                    } elsif ($fields[$i] =~ m/Declination/i) {
                        $dec = $i;
                    } elsif ($fields[$i] =~ m/Magnitude/i) {
                        $mag = $i;
                    }
                }
#print(STDERR "Ra = $ra Dec = $dec\n");
            } else {

                #   Create table entry

                print(OF "<tr><td>");
                for ($i = 0; $i <= $nfields; $i++) {
                    if ((!(defined $fields[$i])) || length($fields[$i]) == 0) {
                        $fields[$i] = "&nbsp;";
                    }
                }
                if (defined $ra && defined $dec && $#fields >= (($ra > $dec) ? $ra : $dec) &&
                    length($fields[$ra]) > 0 && length($fields[$dec]) > 0) {
                    $zra = $fields[$ra];
                    $zdec = $fields[$dec];
                    $zra =~ s/\s//g;
                    $zdec =~ s/\s//g;
                    $zdec =~ s/°/d/g;
                    $zdec =~ s/\'/m/g;
                    $zdec =~ s/\"/s/g;
                    if ($zra ne '&nbsp;' && $zdec ne '&nbsp;') {
                        $dsmo = '';
                        if ($deepmag && defined $mag && $#fields >= $mag) {
                            $dsmo = "&deepm=$fields[$mag]";
                        }
                        $smag = '';
                        if ($startweak && defined $mag && $#fields >= $mag) {
                            $stm = $fields[$mag];
                            $stmo = sprintf("%.1f", $stm + 0.5);
                            if ($stm > 3.5) {
                                $smag .= "&starnm=$stmo";
                            }
                            if ($stm > 4.0) {
                                $smag .= "&starbm=$stmo";
                            }
                            if ($stm > 6.5) {
                                $smag .= "&limag=$stmo";
                            }
                        }
                        $fields[0] = "<a href=\"/cgi-bin/uncgi/Yourtel?lat=$zdec&lon=$zra&z=$zset$dsmo$smag\">" . $fields[0] . "</a>";
                    }
                }
                print(OF join(" <td align=center>", @fields));
                print(OF "\n");
            }
        }
    }
    close(IF);

    print OF <<"EOH"
</table>
</center>
<p>
<b>
<a href="../">Back to <em>Your Sky</em></a>
&nbsp;&nbsp;<a href="catalogues.html">Other Object Catalogues</a>
</b>
<p>
<hr>
<address>
<a href="/index.html">by John Walker</a><br>
</address>
</body>
</html>
EOH
                ;

    close(OF);
