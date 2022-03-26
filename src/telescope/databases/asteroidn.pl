    require "splitfields.pl";

    open(IF, "<$filename.csv");
    open(OF, ">/ftp/yoursky/catalogues/$ofile.html");

    $first = 1;

    if (!(defined $zset)) {
        $zset = 1;
    }

    $tb = 0;
    $lname = 0;

    $lfirst = '';
    $llast = '';

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
<h1>Index by Asteroid Number</h1>
<font size="+2">
EOH
                ;

                $nfields = $#fields;
#print(STDERR "Ra = $ra Dec = $dec\n");
            } else {
                $fields[0] =~ m/(\S+)\s+(.*$)/;

                $astname = $2;
                $astno = $1;
                $astnum = sprintf("%4d", $1);

                #   Create table entry

                if (($astno % 100) == 1) {
                    if ($astno != 1) {
                        $lnext = $astno;
                        $lendr = $lnext + 99;
                        print OFA <<"EOH"
</pre>
<p>
<b>
<a href="$ofile$lnext.html">Asteroids $lnext-$lendr</a>
EOH
                        ;
                        if ($llast ne '') {
                            $llastr = $llast + 99;
                            print OFA <<"EOH"
&nbsp;&nbsp;<a href="$ofile$llast.html">Asteroids $llast-$llastr</a>
EOH
                            ;
                        }
                        print OFA <<"EOH"
&nbsp;&nbsp;<a href="$ofile.html">Index by Number</a>
<br>
<a href="catalogues.html">Other Object Catalogues</a>
&nbsp;&nbsp;<a href="../">Up to <em>Your Sky</em></a>
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
                        close(OFA);
                    }
                    $llast = $lfirst;
                    $lfirst = $astno;
                    $endr = $lfirst + 99;
                    open(OFA, ">/ftp/yoursky/catalogues/$ofile$lfirst.html");
                    print OFA <<"EOH"
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
$what: $lfirst-$endr
</center>
</h1>
<hr>
<p>
<pre>
<b>Number   Name           Mag.</b>
EOH
                    ;
                    print(OF "<a href=\"$ofile$lfirst.html\">$lfirst-$endr</a>&nbsp;&nbsp;\n");
                }

                if (length($astname) > $lname) {
                    $lname = length($astname);
                }

                $elts = "=$actype,$l";
                for ($i = 0; $i <= $nfields; $i++) {
                    if ((!(defined $fields[$i])) || length($fields[$i]) == 0) {
                        $fields[$i] = "&nbsp;";
                    }
                }
                $elto = '';
                while ($elts =~ m/^(\w*)(\W)(.*)$/) {
                    $elto .= $1 . sprintf("%%%02X", ord($2));
                    $elts = $3;
                }
                $elto =~ s/\s/+/g;
                $elto .= $elts;
                $pad = '';
                while ((length($astname) + length($pad)) < 16) {
                    $pad .= " ";
                }
                $fields[0] = " $astnum  <a href=\"/cgi-bin/uncgi/Yourtel?elements=$elto&aim=10&z=1&edump=-xe\">" . $astname . "</a>" . $pad;
                printf(OFA "%s %4.1f\n", $fields[0], $fields[1]);
            }
        }
    }
    close(IF);

    $llastr = $llast + 99;
    print OFA <<"EOH"
</pre>
<p>
<b>
<a href="$ofile$llast.html">Asteroids $llast-$llastr</a>
&nbsp;&nbsp;<a href="$ofile.html">Index by Number</a>
<br>
<a href="catalogues.html">Other Object Catalogues</a>
&nbsp;&nbsp;<a href="../">Up to <em>Your Sky</em></a>
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
    close(OFA);

    print OF <<"EOH"
</font>
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

    print("Longest name: $lname characters\n");
