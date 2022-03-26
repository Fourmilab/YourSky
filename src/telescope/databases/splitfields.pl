;# splitfields.pl
;#
;# Usage:
;#      require 'splitfields.pl';
;#      @words = &shellwords($line);
;#      or
;#      @words = &splitfields(@lines);
;#      or
;#      @words = &splitfields;           # defaults to $_ (and clobbers it)

#   Modified version of the Perl library routine shellwords.pl
#   used to parse CSV files with possibly-quoted fields.

sub splitfields {
    package splitfields;
    local($_) = join('', @_) if @_;
    local(@words,$snippet,$field);

    s/^\s+//;
    while ($_ ne '') {
        $field = '';
        for (;;) {
            if (s/^"([^"]*)"//) {
                $snippet = $1;
            } elsif (/^"/) {
                die "Unmatched double quote: $_\n";
            } elsif (s/^([^,]+)//) {
                $snippet = $1;
            } else {
                s/^,//;
                last;
            }
            $field .= $snippet;
        }
# print(STDERR "Field = <$field>\n");

        #   Quote special characters for use in HTML

        $field =~ s/&/&amp;/;
        $field =~ s/</&lt/;
        $field =~ s/>/&gt/;

        push(@words, $field);
    }
    @words;
}
1;
