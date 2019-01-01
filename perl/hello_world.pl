#!/usr/bin/perl
use strict;
use warnings;
use Term::ReadKey;

sub pause {
    my $s = shift;
    $s = "Press enter to continue..." if(!defined $s);
    $s .= "\n" unless($s =~ /(.*)\n/);
    print($s);

    ReadMode(4);  # Turn off controls keys
    while(!defined(my $key = ReadKey(-1))) {}
    ReadMode(0);  # Reset tty mode
}

print("Hello, world!\n");
pause();
pause("Press another key!");
pause("One more time! Please...?\n");
