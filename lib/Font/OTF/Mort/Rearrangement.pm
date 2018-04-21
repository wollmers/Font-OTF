package Font::OTF::Mort::Rearrangement;

=head1 NAME

Font::OTF::Mort::Rearrangement - Rearrangement Mort subtable for AAT

=head1 METHODS

=cut

use strict;
use vars qw(@ISA);
use Font::OTF::Utils;
use Font::OTF::AATutils;

@ISA = qw(Font::OTF::Mort::Subtable);

sub new {
    my ($class, $direction, $orientation, $subFeatureFlags) = @_;
    my ($self) = {
                    'direction'            => $direction,
                    'orientation'        => $orientation,
                    'subFeatureFlags'    => $subFeatureFlags
                };

    $class = ref($class) || $class;
    bless $self, $class;
}

=head2 $t->read

Reads the table into memory

=cut

sub read {
    my ($self, $fh) = @_;

    my ($classes, $states) = AAT_read_state_table($fh, 0);
    $self->{'classes'} = $classes;
    $self->{'states'} = $states;

    $self;
}

=head2 $t->pack_sub()

=cut

sub pack_sub {
    my ($self) = @_;

    return AAT_pack_state_table($self->{'classes'}, $self->{'states'}, 0);
}

=head2 $t->print($fh)

Prints a human-readable representation of the table

=cut

sub print {
    my ($self, $fh) = @_;

    my $post = $self->post();

    $fh = 'STDOUT' unless defined $fh;

    $self->print_classes($fh);

    $fh->print("\n");
    my $states = $self->{'states'};
    my @verbs = (    "0", "Ax->xA", "xD->Dx", "AxD->DxA",
                    "ABx->xAB", "ABx->xBA", "xCD->CDx", "xCD->DCx",
                    "AxCD->CDxA", "AxCD->DCxA", "ABxD->DxAB", "ABxD->DxBA",
                    "ABxCD->CDxAB", "ABxCD->CDxBA", "ABxCD->DCxAB", "ABxCD->DCxBA");
    for (0 .. $#$states) {
        $fh->printf("\t\tState %d:", $_);
        my $state = $states->[$_];
        for (@$state) {
            my $flags;
            $flags .= "!" if ($_->{'flags'} & 0x4000);
            $flags .= "<" if ($_->{'flags'} & 0x8000);
            $flags .= ">" if ($_->{'flags'} & 0x2000);
            $fh->printf("\t(%s%d,%s)", $flags, $_->{'nextState'}, $verbs[($_->{'flags'} & 0x000f)]);
        }
        $fh->print("\n");
    }
}

1;

=head1 BUGS

None known

=head1 AUTHOR

Jonathan Kew L<http://scripts.sil.org/FontUtils>.


=head1 LICENSING

Copyright (c) 1998-2016, SIL International (http://www.sil.org)

This module is released under the terms of the Artistic License 2.0.
For details, see the full text of the license in the file LICENSE.



=cut



