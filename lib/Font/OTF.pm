package Font::OTF;

use 5.006;
use strict;
use warnings;
our $VERSION = '0.01';
#use utf8;

sub new {
  my $class = shift;
  # uncoverable condition false
  bless @_ ? @_ > 1 ? {@_} : {%{$_[0]}} : {}, ref $class || $class;
}


1;

__END__

=head1 NAME

Font::OTF - Perl interface to OpenType font files

=head1 SYNOPSIS

  use Font::OTF;

  $font = Font::OTF->new;

=head1 ABSTRACT

Font::OTF is a

=head1 DESCRIPTION

=head2 CONSTRUCTOR

=over 4

=item new()

Creates a new object which maintains internal storage areas
for the font structure.

=back

=head2 METHODS

=over 4


=item foo(\@a,\@b)

Does ...

=back

=head2 EXPORT

None by design.

=head1 SEE ALSO

Font::TTF

=head1 REFERENCES



=head1 AUTHOR

Helmut Wollmersdorfer E<lt>helmut.wollmersdorfer@gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2018 by Helmut Wollmersdorfer

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
