#!/usr/bin/perl
use strict;
use warnings;
use Term::ReadKey;

my $in_fname;
my $out_fname;

while(1) {
    print("Enter filename:\n");
    $in_fname = ReadLine(0);
    $in_fname =~ s/^
            "*            # strip extra quotes
            ([^ \n "]*)   # real filename part
            "*            # strip extra quotes
            \n            # strip newline
        $/$1/x;

    my($drive, $path, $fname, $ext);

    if ($in_fname =~ /^
        (                                   # ($1) optional file path
            (                               # ($2) optional drive letter
                ([A-Za-z])                  # ($3) drive letter
                :                           # colon
            )?
            [\\\/]                          # slash
            (                               # ($4) rest of file path
                [^ \n \\\/ :\*\?"<>\|]+     # directory name
                [\\\/]                      # slash
            )*
        )?
        (                                   # ($5) file name
            (                               # ($6) files without extension
                ([^ \n :\*\?"<>\| \.]+)     # ($7) file name
                \.?                         # optional period
                $                           # end of string
            )
            |
            (                               # ($8) files with extension
                ([^ \n :\*\?"<>\|]+)        # ($9) file name
                \.                          # period
                ([^ \n :\*\?"<>\| \.]+)     # ($10) file extension
                $                           # end of string
            )
        )
        /x) {
        $path = $1;
        $drive = $3;
        $fname = $7;
        $fname = $9 if(!defined $fname);
        $ext = $10;
    } else {
        die("Invalid filename $in_fname: $!");
    }

    # Append "_sort" to in_fname (before file extension).
    $out_fname = $path if(defined $path);
    $out_fname .= $fname . "_sort";
    $out_fname .= ".$ext" if(defined $ext);

    # Read file into @lines
    open(my $in, "<", $in_fname) or die("Can't open $in_fname: $!");
    my @lines = <$in>;
    close($in) or die("$in: $!");

    # Remove "\n" from each element of @lines
    foreach(0 .. $#lines) {
        $lines[$_] =~ s/(.*)\n/$1/;
    }

    @lines = sort(@lines);
    # @lines = sort({$a <=> $b} @lines);

    my $lines_len = @lines;
    print("\nNumber of lines: $lines_len\n");

    # Write @lines into file
    open(my $out, ">", $out_fname) or die("Can't open $out_fname: $!");
    print({$out} "$lines[$_]\n") foreach(0 .. ($#lines - 1));
    print({$out} $lines[-1]);
    close($out) or die("$out: $!");
}
