#!/usr/bin/perl
# File: tests/prof/prof_struct_gen.pl
# Abstract:
#   Generate prof_struct.m, to profile struct handling with matlab-ts-mode, e.g. electric indent
#   by visiting prof_struct.m and running:
#     (require 'profiler)
#     (progn (profiler-stop) (profiler-start 'cpu) (indent-region (point-min) (point-max)) (profiler-stop) (profiler-report)))
#

use strict;
use warnings;

Main();

# Subroutine: WriteStructOfSize ====================================================================
# Abstract:
#   Write a structure with $size fields.
#
sub WriteStructOfSize {
    my ($fh, $structName, $size) = @_;

    print $fh "$structName = struct(...\n";

    for (my $i = 0; $i < $size; $i++) {
        printf($fh "'f$i',$i%s...\n", ($i+1 < $size ? "," : " "));
    }
    print $fh ");\n";

} # end WriteStructOfSize



# Subroutine: Main =================================================================================
# Abstract:
#   Main entry for tests/prof/prof_struct_gen.pl
#
sub Main {
    my $file = 'prof_struct.m';
    open(my $fh, '>', $file) || die "create $file: $!";

    WriteStructOfSize($fh, 's10', 10);

    print $fh "\n";

    WriteStructOfSize($fh, 's10000', 10000);

    close($fh) || die "close $file: $!";
} # end Main


# LocalWords:  usr progn fh
