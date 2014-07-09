#!/usr/bin/env bash
# Greatest Common Divisor in POSIX shell using Euclid's algorithm.  On
# return, variable gcd_value is set and is the gcd of parameters $1
# and $2. The GCD of a negative number is the same as the GCD of its
# absolute value, since a negative number is -1 times its positive
# value.  Negative numbers are set when there is an error; -1 is set
# when the wrong number of parameters are given.
gcd() {
    typeset -i a=$1
    typeset -i b=$2
    if (( a > b )) ; then
	a=$b
	b=$1
    fi
   if (( a == 1 || (b-a) == 0)) ; then
       gcd_value=$a
       return 0
   fi
   typeset -i c
   ((c=b-a))
   gcd $c $a
}

gcd $1 $2
echo $gcd_value
