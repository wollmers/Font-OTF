=head1 NAME

Font::OTF::Useall - shortcut to 'use' all the Font::OTF modules

=head1 SYNOPSIS

    use Font::OTF::Useall;

=head1 DESCRIPTION

Useful for debugging, this module simply does a 'use' on all the other
modules that are part of Font::OTF.

=cut

use Font::OTF::Ttc;
use Font::OTF::PSNames;
use Font::OTF::OTTags;
use Font::OTF::EBDT;
use Font::OTF::EBLC;
use Font::OTF::DSIG;
use Font::OTF::Sill;
use Font::OTF::Silf;
use Font::OTF::Cvt_;
use Font::OTF::Fpgm;
use Font::OTF::Glyf;
use Font::OTF::Hdmx;
use Font::OTF::Kern;
use Font::OTF::Loca;
use Font::OTF::LTSH;
use Font::OTF::Name;
use Font::OTF::OS_2;
use Font::OTF::PCLT;
use Font::OTF::Post;
use Font::OTF::Prep;
use Font::OTF::Vmtx;
use Font::OTF::AATKern;
use Font::OTF::AATutils;
use Font::OTF::Anchor;
use Font::OTF::Bsln;
use Font::OTF::Delta;
use Font::OTF::Fdsc;
use Font::OTF::Feat;
use Font::OTF::GrFeat;
use Font::OTF::Fmtx;
use Font::OTF::GPOS;
use Font::OTF::Mort;
use Font::OTF::Prop;
use Font::OTF::GDEF;
use Font::OTF::Coverage;
use Font::OTF::GSUB;
use Font::OTF::Hhea;
use Font::OTF::Table;
use Font::OTF::Ttopen;
use Font::OTF::Glyph;
use Font::OTF::Head;
use Font::OTF::Hmtx;
use Font::OTF::Vhea;
use Font::OTF::Cmap;
use Font::OTF::Utils;
use Font::OTF::Maxp;
use Font::OTF::Font;
use Font::OTF::Kern::ClassArray;
use Font::OTF::Kern::CompactClassArray;
use Font::OTF::Kern::OrderedList;
use Font::OTF::Kern::StateTable;
use Font::OTF::Kern::Subtable;
use Font::OTF::Mort::Chain;
use Font::OTF::Mort::Contextual;
use Font::OTF::Mort::Insertion;
use Font::OTF::Mort::Ligature;
use Font::OTF::Mort::Noncontextual;
use Font::OTF::Mort::Rearrangement;
use Font::OTF::Mort::Subtable;
use Font::OTF::Features::Cvar;
use Font::OTF::Features::Size;
use Font::OTF::Features::Sset;
use Font::OTF::Woff;
use Font::OTF::Woff::MetaData;
use Font::OTF::Woff::PrivateData;
use Font::OTF::Glat;
use Font::OTF::Gloc;
use Font::OTF::Dumper;


1;

=head1 AUTHOR

Martin Hosken L<http://scripts.sil.org/FontUtils>.


=head1 LICENSING

Copyright (c) 1998-2016, SIL International (http://www.sil.org)

This module is released under the terms of the Artistic License 2.0.
For details, see the full text of the license in the file LICENSE.



=cut
