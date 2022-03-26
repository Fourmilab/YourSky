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
<p>
<pre>
EOH
                ;

                #   Create table column titles
                $nfields = $#fields;
            } else {

                #   Create table entry

                $astname = $fields[0];
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
                $fields[0] = " <a href=\"/cgi-bin/uncgi/Yourtel?elements=$elto&aim=10&z=1&edump=-xe\">" . $astname . "</a>";
                printf(OF "%s\n", $fields[0]);
            }
        }
    }
    close(IF);

    print OF <<"EOH"
</pre>
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
