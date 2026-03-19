#!/usr/bin/perl
# File: tests/prof/prof_assign_alignment_gen.pl
# Abstract:
#   Generate prof_assign_alignment.m, to profile consecutive assignment statements alignement within
#   matlab-ts-mode, e.g. electric indent by visiting prof_assign_alignment.m and running:
#     (require 'profiler)
#     (progn (profiler-stop) (profiler-start 'cpu)
#            (indent-region (point-min) (point-max))
#            (profiler-stop) (profiler-report)))
#

use strict;
use warnings;

Main();

# Subroutine: WriteAssignOfSize ====================================================================
# Abstract:
#   Write $size assignment lines.
#
sub WriteAssignOfSize {
    my ($fh, $assignName, $size) = @_;

    for (my $i = 0; $i < $size; $i++) {
        printf($fh "$assignName$i=$i;\n");
    }

} # end WriteAssignOfSize



# Subroutine: Main =================================================================================
# Abstract:
#   Main entry for tests/prof/prof_assign_alignment_gen.pl
#
sub Main {
    my $file = 'prof_assign_alignment.m';
    open(my $fh, '>', $file) || die "create $file: $!";

    WriteAssignOfSize($fh, 'a', 10);

    print $fh "\n";

    WriteAssignOfSize($fh, 'b', 10000);

    close($fh) || die "close $file: $!";
} # end Main


# LocalWords:  usr progn fh
