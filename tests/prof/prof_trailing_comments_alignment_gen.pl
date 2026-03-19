#!/usr/bin/perl
# File: tests/prof/prof_trailing_comments_alignment_gen.pl
# Abstract:
#   Generate prof_trailing_comments_alignment.m, to profile trailing comment alignment within
#   matlab-ts-mode, e.g. electric indent by visiting prof_trailing_comments_alignment.m and running:
#     (require 'profiler)
#     (progn (profiler-stop) (profiler-start 'cpu)
#            (indent-region (point-min) (point-max))
#            (profiler-stop) (profiler-report)))
#

use strict;
use warnings;

Main();

# Subroutine: WriteTrailingCommentsOfSize ==========================================================
# Abstract:
#   Write $size lines with trailing comments.
#
sub WriteTrailingCommentsOfSize {
    my ($fh, $commandName, $size) = @_;

    for (my $i = 0; $i < $size; $i++) {
        print $fh "$commandName arg$i  % comment $i\n";
    }

} # end WriteTrailingCommentsOfSize



# Subroutine: Main =================================================================================
# Abstract:
#   Main entry for tests/prof/prof_trailing_comments_alignment_gen.pl
#
sub Main {
    my $file = 'prof_trailing_comments_alignment.m';
    open(my $fh, '>', $file) || die "create $file: $!";

    WriteTrailingCommentsOfSize($fh, 'commandOne', 10);

    print $fh "\n";

    WriteTrailingCommentsOfSize($fh, 'commandTwo', 10000);

    close($fh) || die "close $file: $!";
} # end Main


# LocalWords:  usr progn fh
