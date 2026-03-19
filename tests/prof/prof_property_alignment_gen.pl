#!/usr/bin/perl
# File: tests/prof/prof_property_alignment_gen.pl
# Abstract:
#   Generate prof_property_alignment.m, to profile trailing comment alignment within
#   matlab-ts-mode, e.g. electric indent by visiting prof_property_alignment.m and running:
#     (require 'profiler)
#     (progn (profiler-stop) (profiler-start 'cpu)
#            (indent-region (point-min) (point-max))
#            (profiler-stop) (profiler-report)))
#

use strict;
use warnings;

Main();

# Subroutine: WritePropertiesOfSize ==========================================================
# Abstract:
#   Write $size lines property lines
#
sub WritePropertiesOfSize {
    my ($fh, $propName, $size) = @_;

    print $fh "properties\n";

    for (my $i = 0; $i < $size; $i++) {
        print $fh "  $propName$i (1,1)\n";
    }

    print $fh "end\n";

} # end WritePropertiesOfSize



# Subroutine: Main =================================================================================
# Abstract:
#   Main entry for tests/prof/prof_property_alignment_gen.pl
#
sub Main {
    my $file = 'prof_property_alignment.m';
    open(my $fh, '>', $file) || die "create $file: $!";

    print $fh "classdef prof_property_alignment\n";

    WritePropertiesOfSize($fh, 'foo', 10);

    print $fh "\n";

    WritePropertiesOfSize($fh, 'bar', 10000);

    print $fh "end\n";

    close($fh) || die "close $file: $!";
} # end Main


# LocalWords:  usr progn fh
