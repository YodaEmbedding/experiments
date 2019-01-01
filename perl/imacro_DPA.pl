#!/usr/bin/perl
use strict;
use warnings;

open (out, '>>data.txt');
print out "VERSION BUILD=8810214 RECORDER=FX\n";

for (my $i = 729; $i <= 1093; $i=$i+1) {
    print out "TAG POS=1 TYPE=INPUT:TEXT FORM=NAME:form1 ATTR=ID:m$i CONTENT=30\n";
    print out "TAG POS=1 TYPE=SELECT FORM=NAME:form1 ATTR=ID:t$i CONTENT=%259\n";
}

close out;
