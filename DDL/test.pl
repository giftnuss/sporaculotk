# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "\nTests 1..1\n\n"; }
END {print " 1..........not ok\n" unless $loaded;}
use DBI;
use DDL::Oracle;
$loaded = 1;
print "1..........ok\n";

