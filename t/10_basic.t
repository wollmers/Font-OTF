#!perl
use 5.008;

use strict;
use warnings;
use utf8;

use lib qw(../lib/);

use Test::More;
use Test::Deep;

use Data::Dumper;

my $class = 'Font::OTF';

use_ok($class);

my $object = new_ok($class);

if (1) {
  ok($object->new());
  ok($object->new(1,2));
  ok($object->new({}));
  ok($object->new({a => 1}));

  ok($class->new());
}


done_testing;
