#!/usr/bin/env perl
use strict;
use warnings;

# GCD. We assume positive numbers
sub gcd($$);
sub gcd($$) 
{ 
    my ($a, $b) = @_;
    # Make: a <= b
    ($a, $b) = ($b, $a) if ($a > $b);

    return undef if $a <= 0;
    return $a if ($a == 1) or ($b-$a == 0);
    return gcd($b-$a, $a);
}

die sprintf "Need two integer arguments, got %d", scalar(@ARGV) unless 
    @ARGV == 2;
my ($a, $b) = @ARGV[0,1];
printf "The GCD of %d and %d is %d\n", $a, $b, gcd($a, $b);
