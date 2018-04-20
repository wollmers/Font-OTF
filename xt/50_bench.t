#!perl
use 5.006;
use open qw(:locale);
use strict;
use warnings;
#use utf8;

use lib qw(../lib/);


use Benchmark qw(:all) ;
use Data::Dumper;

my $data;

if (0) {
    cmpthese( 50_000, {
       'x' => sub {
            $object->x($data)
        },
       'y' => sub {
            $object->y($data)
        },
    });
}



if (0) {
    timethese( 10_000, {
        'x' => sub {
            x($data)
        },
    });
}


